/* SIOD-TR: Raylib Graphics Module
 * Phase 1: Minimal interactive graphics
 * Scheme In One Defun - The Reawakening
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "siod.h"
#include "raylib.h"

/* Forward declarations */
static LISP lraylib_init_window(LISP width, LISP height, LISP title);
static LISP lraylib_close_window(void);
static LISP lraylib_window_should_close(void);
static LISP lraylib_set_target_fps(LISP fps);
static LISP lraylib_get_fps(void);

static LISP lraylib_begin_drawing(void);
static LISP lraylib_end_drawing(void);
static LISP lraylib_clear_background(LISP color);

static LISP lraylib_draw_pixel(LISP x, LISP y, LISP color);
static LISP lraylib_draw_circle(LISP centerx, LISP centery, LISP radius, LISP color);
static LISP lraylib_draw_rectangle(LISP x, LISP y, LISP width, LISP height, LISP color);
static LISP lraylib_draw_line(LISP x1, LISP y1, LISP x2, LISP y2, LISP color);
static LISP lraylib_draw_text(LISP text, LISP x, LISP y, LISP size, LISP color);

static LISP lraylib_key_pressed(LISP key);
static LISP lraylib_key_down(LISP key);
static LISP lraylib_key_released(LISP key);

static LISP lraylib_mouse_button_pressed(LISP button);
static LISP lraylib_mouse_button_down(LISP button);
static LISP lraylib_mouse_position(void);

static LISP lraylib_get_frame_time(void);
static LISP lraylib_get_time(void);

/* Predefined color symbols */
static struct {
    const char *name;
    Color color;
} color_table[] = {
    {"raywhite", RAYWHITE},
    {"white", WHITE},
    {"black", BLACK},
    {"red", RED},
    {"green", GREEN},
    {"blue", BLUE},
    {"yellow", YELLOW},
    {"orange", ORANGE},
    {"pink", PINK},
    {"maroon", MAROON},
    {"lime", LIME},
    {"darkgreen", DARKGREEN},
    {"skyblue", SKYBLUE},
    {"darkblue", DARKBLUE},
    {"purple", PURPLE},
    {"violet", VIOLET},
    {"darkpurple", DARKPURPLE},
    {"beige", BEIGE},
    {"brown", BROWN},
    {"darkbrown", DARKBROWN},
    {"lightgray", LIGHTGRAY},
    {"gray", GRAY},
    {"darkgray", DARKGRAY},
    {"magenta", MAGENTA},
    {"gold", GOLD},
    {NULL, {0, 0, 0, 0}}
};

/* Convert LISP color spec to Raylib Color
 * Accepts: symbol ('red), list '(r g b a), or list '(r g b)
 */
static Color lisp_to_color(LISP lcolor) {
    if (SYMBOLP(lcolor)) {
        /* Look up predefined color */
        const char *name = PNAME(lcolor);
        for (int i = 0; color_table[i].name != NULL; i++) {
            if (strcmp(name, color_table[i].name) == 0) {
                return color_table[i].color;
            }
        }
        err("unknown color symbol", lcolor);
    } else if (CONSP(lcolor)) {
        /* Parse (r g b a) or (r g b) list */
        LISP r = car(lcolor);
        LISP g = car(cdr(lcolor));
        LISP b = car(cdr(cdr(lcolor)));
        LISP a = cdr(cdr(cdr(lcolor)));
        
        Color c;
        c.r = (unsigned char)get_c_long(r);
        c.g = (unsigned char)get_c_long(g);
        c.b = (unsigned char)get_c_long(b);
        c.a = NNULLP(a) ? (unsigned char)get_c_long(car(a)) : 255;
        
        return c;
    }
    
    err("color must be symbol or list (r g b a)", lcolor);
    return BLACK; /* unreachable */
}

/* Helper for boolean returns - works across SIOD versions */
static LISP siod_bool(int value) {
    return value ? flocons(1) : NIL;
}

/* Window Management */
static LISP lraylib_init_window(LISP width, LISP height, LISP title) {
    int w = get_c_long(width);
    int h = get_c_long(height);
    const char *t = get_c_string(title);
    
    InitWindow(w, h, t);
    return NIL;
}

static LISP lraylib_close_window(void) {
    CloseWindow();
    return NIL;
}

static LISP lraylib_window_should_close(void) {
    return siod_bool(WindowShouldClose());
}

static LISP lraylib_set_target_fps(LISP fps) {
    SetTargetFPS(get_c_long(fps));
    return NIL;
}

static LISP lraylib_get_fps(void) {
    return flocons(GetFPS());
}

/* Drawing */
static LISP lraylib_begin_drawing(void) {
    BeginDrawing();
    return NIL;
}

static LISP lraylib_end_drawing(void) {
    EndDrawing();
    return NIL;
}

static LISP lraylib_clear_background(LISP color) {
    ClearBackground(lisp_to_color(color));
    return NIL;
}

/* Drawing Primitives */
static LISP lraylib_draw_pixel(LISP x, LISP y, LISP color) {
    DrawPixel(get_c_long(x), get_c_long(y), lisp_to_color(color));
    return NIL;
}

static LISP lraylib_draw_circle(LISP centerx, LISP centery, LISP radius, LISP color) {
    DrawCircle(get_c_long(centerx), get_c_long(centery), 
               FLONM(radius), lisp_to_color(color));
    return NIL;
}

static LISP lraylib_draw_rectangle(LISP x, LISP y, LISP width, LISP height, LISP color) {
    DrawRectangle(get_c_long(x), get_c_long(y), 
                  get_c_long(width), get_c_long(height),
                  lisp_to_color(color));
    return NIL;
}

static LISP lraylib_draw_line(LISP x1, LISP y1, LISP x2, LISP y2, LISP color) {
    DrawLine(get_c_long(x1), get_c_long(y1),
             get_c_long(x2), get_c_long(y2),
             lisp_to_color(color));
    return NIL;
}

static LISP lraylib_draw_text(LISP text, LISP x, LISP y, LISP size, LISP color) {
    DrawText(get_c_string(text), 
             get_c_long(x), get_c_long(y),
             get_c_long(size),
             lisp_to_color(color));
    return NIL;
}

/* Input - Keyboard */
static LISP lraylib_key_pressed(LISP key) {
    return siod_bool(IsKeyPressed(get_c_long(key)));
}

static LISP lraylib_key_down(LISP key) {
    return siod_bool(IsKeyDown(get_c_long(key)));
}

static LISP lraylib_key_released(LISP key) {
    return siod_bool(IsKeyReleased(get_c_long(key)));
}

/* Input - Mouse */
static LISP lraylib_mouse_button_pressed(LISP button) {
    return siod_bool(IsMouseButtonPressed(get_c_long(button)));
}

static LISP lraylib_mouse_button_down(LISP button) {
    return siod_bool(IsMouseButtonDown(get_c_long(button)));
}

static LISP lraylib_mouse_position(void) {
    Vector2 pos = GetMousePosition();
    return cons(flocons(pos.x), flocons(pos.y));
}

/* Timing */
static LISP lraylib_get_frame_time(void) {
    return flocons(GetFrameTime());
}

static LISP lraylib_get_time(void) {
    return flocons(GetTime());
}

/* Module initialization */
void init_raylib(void) {
    /* Window management */
    init_subr_3("init-window", lraylib_init_window);
    init_subr_0("close-window", lraylib_close_window);
    init_subr_0("window-should-close?", lraylib_window_should_close);
    init_subr_1("set-target-fps", lraylib_set_target_fps);
    init_subr_0("get-fps", lraylib_get_fps);
    
    /* Drawing cycle */
    init_subr_0("begin-drawing", lraylib_begin_drawing);
    init_subr_0("end-drawing", lraylib_end_drawing);
    init_subr_1("clear-background", lraylib_clear_background);
    
    /* Drawing primitives */
    init_subr_3("draw-pixel", lraylib_draw_pixel);
    init_subr_4("draw-circle", lraylib_draw_circle);
    init_subr_5("draw-rectangle", lraylib_draw_rectangle);
    init_subr_5("draw-line", lraylib_draw_line);
    init_subr_5("draw-text", lraylib_draw_text);
    
    /* Keyboard input */
    init_subr_1("key-pressed?", lraylib_key_pressed);
    init_subr_1("key-down?", lraylib_key_down);
    init_subr_1("key-released?", lraylib_key_released);
    
    /* Mouse input */
    init_subr_1("mouse-button-pressed?", lraylib_mouse_button_pressed);
    init_subr_1("mouse-button-down?", lraylib_mouse_button_down);
    init_subr_0("mouse-position", lraylib_mouse_position);
    
    /* Timing */
    init_subr_0("get-frame-time", lraylib_get_frame_time);
    init_subr_0("get-time", lraylib_get_time);
    
    /* Key constants - Raylib uses same values as GLFW */
    setvar(cintern("KEY_RIGHT"), flocons(KEY_RIGHT), NIL);
    setvar(cintern("KEY_LEFT"), flocons(KEY_LEFT), NIL);
    setvar(cintern("KEY_DOWN"), flocons(KEY_DOWN), NIL);
    setvar(cintern("KEY_UP"), flocons(KEY_UP), NIL);
    setvar(cintern("KEY_SPACE"), flocons(KEY_SPACE), NIL);
    setvar(cintern("KEY_ESCAPE"), flocons(KEY_ESCAPE), NIL);
    setvar(cintern("KEY_ENTER"), flocons(KEY_ENTER), NIL);
    
    /* Add more key constants as needed */
    setvar(cintern("KEY_A"), flocons(KEY_A), NIL);
    setvar(cintern("KEY_D"), flocons(KEY_D), NIL);
    setvar(cintern("KEY_S"), flocons(KEY_S), NIL);
    setvar(cintern("KEY_W"), flocons(KEY_W), NIL);
    
    /* Mouse button constants */
    setvar(cintern("MOUSE_LEFT_BUTTON"), flocons(MOUSE_LEFT_BUTTON), NIL);
    setvar(cintern("MOUSE_RIGHT_BUTTON"), flocons(MOUSE_RIGHT_BUTTON), NIL);
    setvar(cintern("MOUSE_MIDDLE_BUTTON"), flocons(MOUSE_MIDDLE_BUTTON), NIL);
}
