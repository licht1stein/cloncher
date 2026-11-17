#include <SDL3/SDL.h>
#include <string.h>
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
    ANCHOR_TOP = 1, KEYBOARD_EXCLUSIVE = 1, LAYER_OVERLAY = 3
};

struct launcher_layer {
    SDL_Window *window;
    struct wl_proxy *role;
    struct wl_proxy *shell;
    bool configured;
};

static struct launcher_layer layer;

bool cloncher_layer_active(void) { return layer.role != NULL; }

static void registry_global(void *data, struct wl_registry *registry, uint32_t name,
                            const char *interface, uint32_t version)
{
    struct launcher_layer *state = data;
    if (!state->shell && version && !strcmp(interface, shell_interface.name))
        state->shell = wl_registry_bind(registry, name, &shell_interface, 1);
}

static void registry_remove(void *data, struct wl_registry *registry, uint32_t name)
{
    (void)data; (void)registry; (void)name;
}

static void configured(void *data, struct wl_proxy *role, uint32_t serial, uint32_t width, uint32_t height)
{
    struct launcher_layer *state = data;
    wl_proxy_marshal_flags(role, ACK_CONFIGURE, NULL, 1, 0, serial);
    int current_width, current_height;
    SDL_GetWindowSize(state->window, &current_width, &current_height);
    if (width && height && (width != (uint32_t)current_width || height != (uint32_t)current_height))
        SDL_SetWindowSize(state->window, (int)width, (int)height);
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

static void clear_role(void)
{
    if (layer.role)
        wl_proxy_marshal_flags(layer.role, DESTROY, NULL, 1, WL_MARSHAL_FLAG_DESTROY);
    if (layer.shell)
        wl_proxy_destroy(layer.shell);
    layer = (struct launcher_layer){0};
}

SDL_Window *cloncher_create_window(const char *title, int width, int height, SDL_WindowFlags flags)
{
    const char *app_id = SDL_GetHint(SDL_HINT_APP_ID);
    if (!app_id || strcmp(app_id, "cloncher.launcher"))
        return SDL_CreateWindow(title, width, height, flags);

    SDL_PropertiesID props = SDL_CreateProperties();
    if (!props)
        return NULL;
    SDL_SetStringProperty(props, SDL_PROP_WINDOW_CREATE_TITLE_STRING, title);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_WIDTH_NUMBER, width);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_HEIGHT_NUMBER, height);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_FLAGS_NUMBER, flags);
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_WAYLAND_SURFACE_ROLE_CUSTOM_BOOLEAN, true);
    SDL_Window *window = SDL_CreateWindowWithProperties(props);
    SDL_DestroyProperties(props);
    if (!window)
        return NULL;

    props = SDL_GetWindowProperties(window);
    struct wl_display *display = SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WAYLAND_DISPLAY_POINTER, NULL);
    struct wl_surface *surface = SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WAYLAND_SURFACE_POINTER, NULL);
    if (display && surface) {
        struct wl_registry *registry = wl_display_get_registry(display);
        wl_registry_add_listener(registry, &registry_listener, &layer);
        int ready = wl_display_roundtrip(display);
        wl_registry_destroy(registry);
        if (ready >= 0 && layer.shell) {
            layer.window = window;
            layer.role = wl_proxy_marshal_flags(layer.shell, 0, &layer_surface_interface, 1, 0,
                                                NULL, surface, NULL, LAYER_OVERLAY, "launcher");
            if (layer.role) {
                wl_proxy_add_listener(layer.role, (void (**)(void))&layer_listener, &layer);
                wl_proxy_marshal_flags(layer.role, SET_SIZE, NULL, 1, 0, (uint32_t)width, (uint32_t)height);
                wl_proxy_marshal_flags(layer.role, SET_ANCHOR, NULL, 1, 0, ANCHOR_TOP);
                wl_proxy_marshal_flags(layer.role, SET_MARGIN, NULL, 1, 0, 140, 0, 0, 0);
                wl_proxy_marshal_flags(layer.role, SET_EXCLUSIVE_ZONE, NULL, 1, 0, -1);
                wl_proxy_marshal_flags(layer.role, SET_KEYBOARD_INTERACTIVITY, NULL, 1, 0, KEYBOARD_EXCLUSIVE);
                wl_surface_commit(surface);
                if (wl_display_roundtrip(display) >= 0 && layer.configured)
                    return window;
            }
        }
    }

    clear_role();
    SDL_DestroyWindow(window);
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
    if (!SDL_SetWindowSize(window, width, height))
        return false;
    if (window == layer.window)
        wl_proxy_marshal_flags(layer.role, SET_SIZE, NULL, 1, 0, (uint32_t)width, (uint32_t)height);
    return true;
}
