#define _GNU_SOURCE
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

extern char **environ;

static int use_full_launcher(char **argv)
{
    char executable[PATH_MAX];
    ssize_t length = readlink("/proc/self/exe", executable, sizeof executable - 1);
    if (length >= 0 && (size_t)length < sizeof executable - 1) {
        executable[length] = '\0';
        char *name = strrchr(executable, '/');
        if (name && (size_t)(name - executable) + sizeof "/cloncher" <= sizeof executable) {
            strcpy(name + 1, "cloncher");
            argv[0] = executable;
            execv(executable, argv);
        }
    }
    perror("cloncher-client: could not start the launcher");
    return 1;
}

static int connect_launcher(void)
{
    const char *directory = getenv("CLONCHER_RUNTIME_DIR");
    if (!directory) directory = getenv("XDG_RUNTIME_DIR");
    char fallback[32];
    if (!directory) {
        snprintf(fallback, sizeof fallback, "/tmp/cloncher-%u", (unsigned)geteuid());
        directory = fallback;
    }
    struct sockaddr_un address = {.sun_family = AF_UNIX};
    size_t length = strlen(directory);
    const char *separator = length && directory[length - 1] != '/' ? "/" : "";
    int written = snprintf(address.sun_path, sizeof address.sun_path,
                           "%s%scloncher/launcher.sock", directory, separator);
    if (written < 0 || (size_t)written >= sizeof address.sun_path) return -1;
    int fd = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
    if (fd < 0) return -1;
    if (connect(fd, (struct sockaddr *)&address, sizeof address) == 0) return fd;
    close(fd);
    return -1;
}

static void write_string(FILE *out, const char *start, const char *end)
{
    fputc('"', out);
    for (; start != end; ++start) {
        const unsigned char ch = (unsigned char)*start;
        if (ch < 32) fprintf(out, "\\u%04x", (unsigned)ch);
        else {
            if (ch == '"' || ch == '\\') fputc('\\', out);
            fputc(ch, out);
        }
    }
    fputc('"', out);
}

static int send_root(int fd)
{
    char *message = NULL;
    size_t length = 0;
    FILE *out = open_memstream(&message, &length);
    if (!out) return 1;
    fputs("{\"args\":[\"root\"],\"lines\":[],\"env\":[", out);
    const char *separator = "";
    for (char **entry = environ; *entry; ++entry) {
        const char *equals = strchr(*entry, '=');
        if (!equals) continue;
        fputs(separator, out);
        fputc('[', out);
        write_string(out, *entry, equals);
        fputc(',', out);
        write_string(out, equals + 1, equals + strlen(equals));
        fputc(']', out);
        separator = ",";
    }
    fputs("]}\n", out);
    if (fclose(out) == EOF) {
        free(message);
        return 1;
    }
    size_t start = 0;
    while (start < length) {
        ssize_t count = send(fd, message + start, length - start, MSG_NOSIGNAL);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) break;
        start += (size_t)count;
    }
    free(message);
    if (start != length) return 1;

    char reply[256];
    start = 0;
    while (start < sizeof reply - 1) {
        ssize_t count = recv(fd, reply + start, sizeof reply - 1 - start, 0);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) return 1;
        if (memchr(reply + start, '\n', (size_t)count)) {
            reply[start + (size_t)count] = '\0';
            const char *exit_key = strstr(reply, "\"exit\"");
            const char *colon = exit_key ? strchr(exit_key, ':') : NULL;
            return colon ? (int)strtol(colon + 1, NULL, 10) : 1;
        }
        start += (size_t)count;
    }
    return 1;
}

int main(int argc, char **argv)
{
    if (getenv("LAUNCHER_STANDALONE") ||
        (argc != 1 && !(argc == 2 && !strcmp(argv[1], "root"))))
        return use_full_launcher(argv);
    int fd = connect_launcher();
    if (fd < 0) return use_full_launcher(argv);
    int result = send_root(fd);
    close(fd);
    return result;
}
