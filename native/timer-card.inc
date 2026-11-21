#include "cui_internal.h"

/* Resolve before changing alpha: a CUI token is itself encoded as a color. */
static cui_color timer_card_color(cui_ctx *ui, cui_color token, uint8_t alpha) {
    cui_color color = cui_color_of(ui, token);
    color.a = alpha;
    return color;
}

/* Draw a single line at a stable baseline. Shrink the time before truncating;
 * let CUI handle ellipsis placement and UTF-8 boundaries for all three lines. */
static float timer_card_line(cui_ctx *ui, const char *text, cui_font font, cui_color color,
                             float x, float width, float baseline, float size, float min_size,
                             bool centered) {
    int len = text ? (int)strcspn(text, "\r\n") : 0;
    if (!len) return x + (centered ? width * 0.5f : 0);

    cui_text_metrics m = cui_measure_text(ui, font, size, text, len);
    if (m.width > width && size > min_size) {
        size = fmaxf(min_size, size * width / m.width);
        m = cui_measure_text(ui, font, size, text, len);
    }
    float y = baseline - m.ascent;
    if (m.width > width + 0.5f) {
        cui__draw_text_ellipsis(ui, font, size, x, y, width, color, text, len, false);
    } else {
        if (centered) x += (width - m.width) * 0.5f;
        cui_draw_text(ui, font, size, (cui_vec2){x, y}, color, text, len);
    }
    return x;
}

static void timer_card_ring(cui_ctx *ui, double progress, cui_color track, cui_color ink,
                            const char *icon) {
    const cui_vec2 center = {248, 54};
    const float radius = 29, stroke = 4, outer = radius + stroke * 0.5f;
    const float top = -CUI_PI * 0.5f;
    const cui_rect bounds = {center.x - outer, center.y - outer, 2 * outer, 2 * outer};
    cui_draw_rect_border(ui, bounds, CUI_CORNERS(outer), CUI_TRANSPARENT,
                         progress >= 1 ? ink : track, stroke);
    if (progress > 0 && progress < 1) {
        float end = top + (float)progress * (2 * CUI_PI);
        cui_draw_arc(ui, center, radius, top, end, stroke, ink);
        cui_draw_circle(ui, (cui_vec2){center.x, center.y - radius}, stroke * 0.5f, ink);
        cui_draw_circle(ui, (cui_vec2){center.x + cosf(end) * radius,
                                     center.y + sinf(end) * radius}, stroke * 0.5f, ink);
    }
    cui_draw_icon(ui, icon, bounds, 22, ink);
}

static void timer_card_alarm(cui_ctx *ui, double animation, cui_color track, cui_color ink) {
    timer_card_ring(ui, 1, track, ink, CUI_ICON_BELL);
    if (animation < 1) {
        /* Fade in and out around a steady bell, with a quiet beat between ripples. */
        const float phase = (float)animation;
        const float fade = sinf(CUI_PI * phase);
        const float radius = 32 + 16 * phase;
        ink.a = (uint8_t)(150 * fade * fade);
        cui_draw_rect_border(ui, (cui_rect){248 - radius, 54 - radius, 2 * radius, 2 * radius},
                             CUI_CORNERS(radius), CUI_TRANSPARENT, ink, 2);
    }
}

/* Progress is countdown fraction, negative for a stopwatch, or alarm phase when finished. */
CUI_API void cloncher_timer_card(cui_ctx *ui, const char *digits, const char *title,
                                 const char *status, double progress, int state) {
    const bool stopwatch = progress < 0;
    const cui_color surface = timer_card_color(ui, CUI_COLOR_SURFACE, 153); /* 60% */
    const cui_color border = timer_card_color(ui, CUI_COLOR_BORDER_SUBTLE, 255);
    const cui_color foreground = timer_card_color(ui, CUI_COLOR_FOREGROUND, 255);
    const cui_color ink = timer_card_color(ui, state == 2 ? CUI_COLOR_DANGER :
                                             state == 1 ? CUI_COLOR_TEXT_SECONDARY :
                                                          CUI_COLOR_ACCENT, 255);
    cui_draw_rect_border(ui, (cui_rect){1, 1, 298, 106}, CUI_CORNERS(16), surface,
                         state == 2 ? cui_mix(border, ink, 0.4f) : border, 1);

    const float x = 19, width = stopwatch ? 262 : 184;
    timer_card_line(ui, title, CUI_FONT_MEDIUM, foreground, x, width, 27, 13, 13, stopwatch);
    timer_card_line(ui, digits, CUI_FONT_MONO, foreground, x, width, 70,
                    stopwatch ? 48 : 42, 17, stopwatch);
    if (stopwatch) {
        /* Center the icon and status as a unit; the readout uses the full card. */
        const float leading = 18;
        float status_x = timer_card_line(ui, status, CUI_FONT_BODY, foreground,
                                         x + leading, width - leading, 91, 12, 12, true);
        cui_draw_icon(ui, state == 1 ? CUI_ICON_PAUSE : CUI_ICON_CLOCK,
                      (cui_rect){status_x - leading, 80, 13, 13}, 13, ink);
    } else {
        timer_card_line(ui, status, CUI_FONT_BODY, foreground, x, width, 91, 12, 12, false);
        if (state == 2) timer_card_alarm(ui, progress, border, ink);
        else timer_card_ring(ui, progress, border, ink, state == 1 ? CUI_ICON_PAUSE : CUI_ICON_CLOCK);
    }
}
