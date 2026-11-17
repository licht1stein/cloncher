/* Included by cui_host.c so the prepared window can share its font setup. */
#include <pthread.h>

static cui_app *prepared_launcher;
static bool use_prepared_launcher;
static bool launcher_preparation_unavailable;
static pthread_mutex_t prepared_theme_lock = PTHREAD_MUTEX_INITIALIZER;
static host_frame_waker *prepared_theme_waker;

extern bool cloncher_layer_ready(void);
extern bool cloncher_attach_layer(int, int);
extern void cloncher_defer_layer(bool);
extern void cui__plat_show(cui_app *);

static void launcher_load_fonts(cui_app *app) {
    cui_ctx *ui = cui_app_ui(app);
    const char *emoji_font = getenv("TEMPLATE_EMOJI_FONT");
    if (emoji_font) cui_emoji_font_load_file(ui, emoji_font);
    else cui_emoji_font_load_system(ui);
    host_text_fonts(ui);
}

static void wake_prepared_theme(void *unused) {
    (void)unused;
    pthread_mutex_lock(&prepared_theme_lock);
    if (prepared_theme_waker) wake_theme(prepared_theme_waker);
    pthread_mutex_unlock(&prepared_theme_lock);
}

static void launcher_unwatch_theme(void) {
    host_theme_unwatch();
    pthread_mutex_lock(&prepared_theme_lock);
    prepared_theme_waker = NULL;
    pthread_mutex_unlock(&prepared_theme_lock);
}

static void discard_prepared_launcher(void) {
    if (!prepared_launcher) return;
    cui_app_destroy(prepared_launcher);
    host_text_fonts_unmap();
    launcher_unwatch_theme();
    prepared_launcher = NULL;
}

void cloncher_prepare_launcher(int width, int height) {
    if (prepared_launcher || launcher_preparation_unavailable ||
        strcmp(cui_app_backend(NULL), "wayland") || getenv("CUI_SCREENSHOT")) return;
#ifdef CUI_FONTCONFIG_FILE
    setenv("FONTCONFIG_FILE", CUI_FONTCONFIG_FILE, 0);
#endif
    host_theme_watch(wake_prepared_theme, NULL);
    const cui_app_desc desc = {
        .title = "Cloncher", .app_id = "cloncher.launcher", .hidden = true,
        .width = width, .height = height, .min_width = 360, .min_height = 62,
        .theme = host_theme() == 1 ? CUI_THEME_DARK : CUI_THEME_LIGHT, .accent = CUI_ACCENT_NEUTRAL
    };
    cloncher_defer_layer(true);
    cui_app *app = cui_app_create(&desc);
    cloncher_defer_layer(false);
    if (!app || !cloncher_layer_ready()) {
        if (app) cui_app_destroy(app);
        launcher_unwatch_theme();
        launcher_preparation_unavailable = true;
        return;
    }
    launcher_load_fonts(app);
    prepared_launcher = app;
}

void cloncher_use_prepared_launcher(void) {
    use_prepared_launcher = true;
}

static void launcher_watch_theme(const cui_app_desc *desc, host_frame_waker *waker) {
    if (use_prepared_launcher && prepared_launcher && desc->app_id &&
        !strcmp(desc->app_id, "cloncher.launcher")) {
        pthread_mutex_lock(&prepared_theme_lock);
        prepared_theme_waker = waker;
        pthread_mutex_unlock(&prepared_theme_lock);
    } else {
        discard_prepared_launcher();
        host_theme_watch(wake_theme, waker);
    }
}

static cui_app *take_prepared_launcher(const cui_app_desc *desc) {
    const bool requested = use_prepared_launcher;
    use_prepared_launcher = false;
    if (!requested) return NULL;
    cui_app *app = prepared_launcher;
    prepared_launcher = NULL;
    if (app) {
        cui_vec2 size = cui_app_size(app);
        if ((int)size.x != desc->width || (int)size.y != desc->height)
            cui_app_set_size(app, desc->width, desc->height);
        if (!cloncher_attach_layer(desc->width, desc->height)) {
            cui_app_destroy(app);
            host_text_fonts_unmap();
            launcher_preparation_unavailable = true;
            return NULL;
        }
        if (!getenv("CUI_THEME")) cui_set_theme(cui_app_ui(app), desc->theme);
        cui__plat_show(app);
    }
    return app;
}
