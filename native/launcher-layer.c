#define _GNU_SOURCE
#include <SDL3/SDL.h>
#include <limits.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <wayland-client.h>

/* The version 1 requests used by this adapter. The unused get_popup
 * slot stays in the table to preserve the subsequent wire opcodes. */
static const struct wl_interface layer_surface_interface;
static const struct wl_interface *get_surface_types[] = {
    &layer_surface_interface, &wl_surface_interface, &wl_output_interface, NULL, NULL
};
static const struct wl_message shell_requests[] = {
    {"get_layer_surface", "no?ous", get_surface_types}
};
static const struct wl_interface shell_interface = {
    "zwlr_layer_shell_v1", 1, 1, shell_requests, 0, NULL
};
static const struct wl_message surface_requests[] = {
    {"set_size", "uu", NULL},
    {"set_anchor", "u", NULL},
    {"set_exclusive_zone", "i", NULL},
    {"set_margin", "iiii", NULL},
    {"set_keyboard_interactivity", "u", NULL},
    {"get_popup", "o", NULL},
    {"ack_configure", "u", NULL},
    {"destroy", "", NULL}
};
static const struct wl_message surface_events[] = {
    {"configure", "uuu", NULL}, {"closed", "", NULL}
};
static const struct wl_interface layer_surface_interface = {
    "zwlr_layer_surface_v1", 1, 8, surface_requests, 2, surface_events
};

enum {
    SET_SIZE = 0, SET_ANCHOR = 1, SET_EXCLUSIVE_ZONE = 2, SET_MARGIN = 3,
    SET_KEYBOARD_INTERACTIVITY = 4, ACK_CONFIGURE = 6, DESTROY = 7,
    ANCHOR_TOP = 1, ANCHOR_RIGHT = 8,
    KEYBOARD_NONE = 0, KEYBOARD_EXCLUSIVE = 1, LAYER_OVERLAY = 3
};

struct timer_buffer {
    struct wl_buffer *handle;
    void *pixels;
    size_t size;
    int width, height, stride;
    bool busy;
};

struct launcher_layer {
    SDL_Window *window;
    struct wl_display *display;
    struct wl_surface *surface;
    struct wl_proxy *role;
    struct wl_proxy *shell;
    struct wl_compositor *compositor;
    struct wl_shm *shm;
    struct timer_buffer buffers[2];
    bool timer;   /* any translucent overlay: the timer, or the note editor when `note` */
    bool note;
    bool configured;
};

static struct launcher_layer layer;
static bool defer_layer;

static const char timer_app_id[] = "cloncher.timer-overlay";
static const char note_app_id[] = "cloncher.note";

bool cloncher_is_note_overlay(const char *app_id) {
    return app_id && !strcmp(app_id, note_app_id);
}

/* Both overlays share the translucent, fixed-size presentation path. */
bool cloncher_is_timer_overlay(const char *app_id) {
    return app_id && (!strcmp(app_id, timer_app_id) || cloncher_is_note_overlay(app_id));
}

bool cloncher_layer_active(void) { return layer.role != NULL; }
bool cloncher_layer_ready(void) { return layer.shell != NULL; }
void cloncher_defer_layer(bool defer) { defer_layer = defer; }

static void registry_global(void *data, struct wl_registry *registry, uint32_t name,
                            const char *interface, uint32_t version)
{
    struct launcher_layer *state = data;
    if (!state->shell && version && !strcmp(interface, shell_interface.name))
        state->shell = wl_registry_bind(registry, name, &shell_interface, 1);
    if (state->timer && !state->compositor && version && !strcmp(interface, wl_compositor_interface.name))
        state->compositor = wl_registry_bind(registry, name, &wl_compositor_interface, 1);
    if (state->timer && !state->shm && version && !strcmp(interface, wl_shm_interface.name))
        state->shm = wl_registry_bind(registry, name, &wl_shm_interface, 1);
}

static void registry_remove(void *data, struct wl_registry *registry, uint32_t name)
{
    (void)data; (void)registry; (void)name;
}

static void configured(void *data, struct wl_proxy *role, uint32_t serial, uint32_t width, uint32_t height)
{
    struct launcher_layer *state = data;
    wl_proxy_marshal_flags(role, ACK_CONFIGURE, NULL, 1, 0, serial);
    if (!state->timer) {
        int current_width, current_height;
        SDL_GetWindowSize(state->window, &current_width, &current_height);
        if (width && height && (width != (uint32_t)current_width || height != (uint32_t)current_height))
            SDL_SetWindowSize(state->window, (int)width, (int)height);
    }
    state->configured = true;
}

static void closed(void *data, struct wl_proxy *role)
{
    (void)role;
    struct launcher_layer *state = data;
    SDL_Event event = {.type = SDL_EVENT_WINDOW_CLOSE_REQUESTED};
    event.window.windowID = SDL_GetWindowID(state->window);
    SDL_PushEvent(&event);
}

static const struct wl_registry_listener registry_listener = {registry_global, registry_remove};
static const struct {
    void (*configure)(void *, struct wl_proxy *, uint32_t, uint32_t, uint32_t);
    void (*closed)(void *, struct wl_proxy *);
} layer_listener = {configured, closed};

static void free_timer_buffer(struct timer_buffer *buffer)
{
    if (!buffer->handle)
        return;
    wl_buffer_destroy(buffer->handle);
    munmap(buffer->pixels, buffer->size);
    *buffer = (struct timer_buffer){0};
}

static void timer_buffer_released(void *data, struct wl_buffer *handle)
{
    (void)handle;
    ((struct timer_buffer *)data)->busy = false;
}

static const struct wl_buffer_listener timer_buffer_listener = {timer_buffer_released};

static bool allocate_timer_buffer(struct timer_buffer *buffer, int width, int height, int stride)
{
    if (width <= 0 || height <= 0 || stride <= 0 || height > INT_MAX / stride)
        return false;
    const int size = stride * height;
    const int fd = memfd_create("cloncher-timer", MFD_CLOEXEC);
    if (fd < 0)
        return false;
    void *pixels = ftruncate(fd, size) == 0 ?
        mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0) : MAP_FAILED;
    struct wl_shm_pool *pool = pixels == MAP_FAILED ? NULL : wl_shm_create_pool(layer.shm, fd, size);
    close(fd);
    struct wl_buffer *handle = NULL;
    if (pool) {
        handle = wl_shm_pool_create_buffer(pool, 0, width, height, stride, WL_SHM_FORMAT_ARGB8888);
        wl_shm_pool_destroy(pool);
    }
    if (!handle) {
        if (pixels != MAP_FAILED) munmap(pixels, size);
        return false;
    }
    *buffer = (struct timer_buffer){handle, pixels, size, width, height, stride, false};
    wl_buffer_add_listener(handle, &timer_buffer_listener, buffer);
    return true;
}

/* SDL's Wayland window framebuffer uses a renderer that can recreate the
 * SDL window with an xdg_toplevel role. Keep our existing custom role. */
void cloncher_present_timer(const void *bgra, int width, int height, int stride)
{
    if (!layer.timer || !layer.role || !layer.shm)
        return;
    if (layer.buffers[0].busy && layer.buffers[1].busy)
        wl_display_roundtrip(layer.display);
    struct timer_buffer *buffer = layer.buffers[0].busy ? &layer.buffers[1] : &layer.buffers[0];
    if (buffer->busy)
        return;
    if (buffer->width != width || buffer->height != height || buffer->stride != stride) {
        free_timer_buffer(buffer);
        if (!allocate_timer_buffer(buffer, width, height, stride))
            return;
    }
    memcpy(buffer->pixels, bgra, buffer->size);
    buffer->busy = true;
    wl_surface_attach(layer.surface, buffer->handle, 0, 0);
    if (wl_surface_get_version(layer.surface) >= WL_SURFACE_DAMAGE_BUFFER_SINCE_VERSION)
        wl_surface_damage_buffer(layer.surface, 0, 0, width, height);
    else
        wl_surface_damage(layer.surface, 0, 0, INT_MAX, INT_MAX);
    wl_surface_commit(layer.surface);
    wl_display_flush(layer.display);
}

static void clear_role(void)
{
    if (layer.role)
        wl_proxy_marshal_flags(layer.role, DESTROY, NULL, 1, WL_MARSHAL_FLAG_DESTROY);
    if (layer.shell)
        wl_proxy_destroy(layer.shell);
    if (layer.compositor)
        wl_compositor_destroy(layer.compositor);
    if (layer.timer) {
        free_timer_buffer(&layer.buffers[0]);
        free_timer_buffer(&layer.buffers[1]);
        if (layer.shm) wl_shm_destroy(layer.shm);
    }
    layer = (struct launcher_layer){0};
}

bool cloncher_attach_layer(int width, int height)
{
    if (!layer.shell || !layer.surface || layer.role)
        return false;
    struct wl_region *input = NULL;
    if (layer.timer && !layer.note) {   /* the timer lets clicks through */
        if (!layer.compositor || !(input = wl_compositor_create_region(layer.compositor)))
            return SDL_SetError("could not create an empty Wayland input region");
    }
    /* NULL selects the compositor's default output, rather than pinning a monitor. */
    layer.role = wl_proxy_marshal_flags(layer.shell, 0, &layer_surface_interface, 1, 0,
                                        NULL, layer.surface, NULL, LAYER_OVERLAY,
                                        layer.note ? note_app_id : layer.timer ? timer_app_id : "launcher");
    if (input) {
        if (layer.role) wl_surface_set_input_region(layer.surface, input);
        wl_region_destroy(input);
    }
    if (!layer.role)
        return false;
    wl_proxy_add_listener(layer.role, (void (**)(void))&layer_listener, &layer);
    wl_proxy_marshal_flags(layer.role, SET_SIZE, NULL, 1, 0, (uint32_t)width, (uint32_t)height);
    /* The note editor is unanchored, so the compositor centers it. */
    wl_proxy_marshal_flags(layer.role, SET_ANCHOR, NULL, 1, 0,
                           layer.note ? 0 : layer.timer ? ANCHOR_TOP | ANCHOR_RIGHT : ANCHOR_TOP);
    wl_proxy_marshal_flags(layer.role, SET_MARGIN, NULL, 1, 0,
                           layer.note ? 0 : layer.timer ? 16 : 140, layer.timer && !layer.note ? 16 : 0, 0, 0);
    /* Follow the bar's reserved space without reserving any ourselves. */
    wl_proxy_marshal_flags(layer.role, SET_EXCLUSIVE_ZONE, NULL, 1, 0, layer.timer ? 0 : -1);
    wl_proxy_marshal_flags(layer.role, SET_KEYBOARD_INTERACTIVITY, NULL, 1, 0,
                           layer.timer && !layer.note ? KEYBOARD_NONE : KEYBOARD_EXCLUSIVE);
    wl_surface_commit(layer.surface);
    return wl_display_roundtrip(layer.display) >= 0 && layer.configured;
}

SDL_Window *cloncher_create_window(const char *title, int width, int height, SDL_WindowFlags flags)
{
    const char *app_id = SDL_GetHint(SDL_HINT_APP_ID);
    const bool timer = cloncher_is_timer_overlay(app_id);
    if (!timer && (!app_id || strcmp(app_id, "cloncher.launcher")))
        return SDL_CreateWindow(title, width, height, flags);
    if (timer) {
        const char *driver = SDL_GetCurrentVideoDriver();
        if (!driver || strcmp(driver, "wayland")) {
            SDL_SetError("cloncher.timer-overlay requires Wayland");
            return NULL;
        }
        flags = (flags & ~SDL_WINDOW_RESIZABLE) | SDL_WINDOW_TRANSPARENT;
        if (!SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8))
            return NULL;
    }

    SDL_PropertiesID props = SDL_CreateProperties();
    if (!props)
        return NULL;
    SDL_SetStringProperty(props, SDL_PROP_WINDOW_CREATE_TITLE_STRING, title);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_WIDTH_NUMBER, width);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_HEIGHT_NUMBER, height);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_FLAGS_NUMBER, flags);
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_WAYLAND_SURFACE_ROLE_CUSTOM_BOOLEAN, true);
    if (timer)
        SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_TRANSPARENT_BOOLEAN, true);
    SDL_Window *window = SDL_CreateWindowWithProperties(props);
    SDL_DestroyProperties(props);
    if (!window)
        return NULL;

    props = SDL_GetWindowProperties(window);
    struct wl_display *display = SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WAYLAND_DISPLAY_POINTER, NULL);
    struct wl_surface *surface = SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WAYLAND_SURFACE_POINTER, NULL);
    if (display && surface) {
        layer.timer = timer;
        layer.note = cloncher_is_note_overlay(app_id);
        struct wl_registry *registry = wl_display_get_registry(display);
        wl_registry_add_listener(registry, &registry_listener, &layer);
        int ready = wl_display_roundtrip(display);
        wl_registry_destroy(registry);
        if (ready >= 0 && layer.shell) {
            layer.window = window;
            layer.display = display;
            layer.surface = surface;
            if ((!timer && defer_layer) || ((!timer || layer.shm) && cloncher_attach_layer(width, height)))
                return window;
        }
    }

    clear_role();
    SDL_DestroyWindow(window);
    if (timer) {
        SDL_SetError("cloncher.timer-overlay requires a configured Wayland layer surface");
        return NULL;
    }
    return SDL_CreateWindow(title, width, height, flags);
}

void cloncher_destroy_window(SDL_Window *window)
{
    if (window == layer.window)
        clear_role();
    SDL_DestroyWindow(window);
}

bool cloncher_set_window_size(SDL_Window *window, int width, int height)
{
    if (window == layer.window && layer.timer)
        return SDL_SetError("cloncher.timer-overlay has a fixed size");
    if (!SDL_SetWindowSize(window, width, height))
        return false;
    if (window == layer.window && layer.role)
        wl_proxy_marshal_flags(layer.role, SET_SIZE, NULL, 1, 0, (uint32_t)width, (uint32_t)height);
    return true;
}
