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

static Color get_color(LISP color_obj) {
    /* Handle color symbols like 'red, 'blue, etc */
    if (TYPEP(color_obj, tc_symbol)) {
        char *name = PNAME(color_obj);

        /* Common colors */
        if (strcmp(name, "white") == 0) return WHITE;
        if (strcmp(name, "black") == 0) return BLACK;
        if (strcmp(name, "raywhite") == 0) return RAYWHITE;
        if (strcmp(name, "red") == 0) return RED;
        if (strcmp(name, "green") == 0) return GREEN;
        if (strcmp(name, "blue") == 0) return BLUE;
        if (strcmp(name, "yellow") == 0) return YELLOW;
        if (strcmp(name, "orange") == 0) return ORANGE;
        if (strcmp(name, "pink") == 0) return PINK;
        if (strcmp(name, "lime") == 0) return LIME;
        if (strcmp(name, "gray") == 0) return GRAY;
        if (strcmp(name, "darkgray") == 0) return DARKGRAY;
        if (strcmp(name, "lightgray") == 0) return LIGHTGRAY;
        if (strcmp(name, "skyblue") == 0) return SKYBLUE;
        if (strcmp(name, "purple") == 0) return PURPLE;
        if (strcmp(name, "violet") == 0) return VIOLET;
        if (strcmp(name, "darkblue") == 0) return DARKBLUE;
        if (strcmp(name, "darkgreen") == 0) return DARKGREEN;
        if (strcmp(name, "darkbrown") == 0) return DARKBROWN;
        if (strcmp(name, "maroon") == 0) return MAROON;
        if (strcmp(name, "gold") == 0) return GOLD;
        if (strcmp(name, "beige") == 0) return BEIGE;
        if (strcmp(name, "brown") == 0) return BROWN;
        if (strcmp(name, "magenta") == 0) return MAGENTA;

        /* Unknown color - default to white */
        return WHITE;
    }

    /* Handle RGB/RGBA list: '(r g b a) */
    if (CONSP(color_obj)) {
        int r = (int)FLONM(car(color_obj));
        int g = (int)FLONM(car(cdr(color_obj)));
        int b = (int)FLONM(car(cdr(cdr(color_obj))));
        int a = (int)FLONM(car(cdr(cdr(cdr(color_obj)))));

        Color color = {
            (unsigned char)r,
            (unsigned char)g,
            (unsigned char)b,
            (unsigned char)a
        };
        return color;
    }

    /* Fallback */
    return WHITE;
}

static RenderTexture2D g_render_texture = {0};

/* Create or recreate the render texture */
static LISP lcreate_render_texture(LISP width, LISP height) {
    int w = get_c_long(width);
    int h = get_c_long(height);
    
    /* Unload existing if present */
    if (g_render_texture.id != 0) {
        UnloadRenderTexture(g_render_texture);
    }
    
    /* Create new render texture */
    g_render_texture = LoadRenderTexture(w, h);
    
    if (g_render_texture.id == 0) {
        err("Failed to create render texture", NIL);
    }
    
    return NIL;
}

/* Begin drawing to render texture */
static LISP lbegin_texture_mode(void) {
    if (g_render_texture.id == 0) {
        err("No render texture created. Call create-render-texture first.", NIL);
    }
    BeginTextureMode(g_render_texture);
    return NIL;
}

/* End drawing to render texture */
static LISP lend_texture_mode(void) {
    EndTextureMode();
    return NIL;
}

/* Draw the render texture to screen */
static LISP ldraw_render_texture(LISP x, LISP y, LISP tint) {
    if (g_render_texture.id == 0) {
        err("No render texture to draw. Call create-render-texture first.", NIL);
    }

    int px = get_c_long(x);
    int py = get_c_long(y);

    /* Source rectangle - negative height flips texture vertically */
    Rectangle source = {
        0, 0,
        (float)g_render_texture.texture.width,
        -(float)g_render_texture.texture.height  /* Negative = flip! */
    };

    /* Destination rectangle on screen */
    Rectangle dest = {
        (float)px,
        (float)py,
        (float)g_render_texture.texture.width,
        (float)g_render_texture.texture.height
    };

    /* Draw with DrawTexturePro to handle the flip */
    DrawTexturePro(g_render_texture.texture,
                   source, dest,
                   (Vector2){0, 0},  /* origin */
                   0.0f,             /* rotation */
                   WHITE);           /* tint */

    return NIL;
}

/* Cleanup render texture */
static LISP lunload_render_texture(void) {
    if (g_render_texture.id != 0) {
        UnloadRenderTexture(g_render_texture);
        g_render_texture.id = 0;
    }
    return NIL;
}

/* ============================================
   PHASE 2: GLOBAL CAMERA2D
   ============================================ */

static Camera2D g_camera = {
    .offset = {0, 0},
    .target = {0, 0},
    .rotation = 0.0f,
    .zoom = 1.0f
};

/* Initialize camera */
static LISP linit_camera(LISP offset_x, LISP offset_y, LISP zoom) {
    g_camera.offset.x = FLONM(offset_x);
    g_camera.offset.y = FLONM(offset_y);
    g_camera.target.x = 0;
    g_camera.target.y = 0;
    g_camera.rotation = 0.0f;
    g_camera.zoom = FLONM(zoom);
    return NIL;
}

/* Set camera target (what the camera looks at) */
static LISP lcamera_set_target(LISP x, LISP y) {
    g_camera.target.x = FLONM(x);
    g_camera.target.y = FLONM(y);
    return NIL;
}

/* Move camera target by delta */
static LISP lcamera_move(LISP dx, LISP dy) {
    g_camera.target.x += FLONM(dx);
    g_camera.target.y += FLONM(dy);
    return NIL;
}

/* Set camera zoom */
static LISP lcamera_set_zoom(LISP z) {
    float zoom = FLONM(z);
    if (zoom < 0.1f) zoom = 0.1f;  /* Prevent zoom too small */
    if (zoom > 10.0f) zoom = 10.0f;  /* Prevent zoom too large */
    g_camera.zoom = zoom;
    return NIL;
}

/* Adjust camera zoom by factor */
static LISP lcamera_zoom_by(LISP factor) {
    float new_zoom = g_camera.zoom * FLONM(factor);
    if (new_zoom < 0.1f) new_zoom = 0.1f;
    if (new_zoom > 10.0f) new_zoom = 10.0f;
    g_camera.zoom = new_zoom;
    return NIL;
}

/* Get camera properties */
static LISP lcamera_get_x(void) {
    return flocons(g_camera.target.x);
}

static LISP lcamera_get_y(void) {
    return flocons(g_camera.target.y);
}

static LISP lcamera_get_zoom(void) {
    return flocons(g_camera.zoom);
}

/* Begin camera mode */
static LISP lbegin_camera_mode(void) {
    BeginMode2D(g_camera);
    return NIL;
}

/* End camera mode */
static LISP lend_camera_mode(void) {
    EndMode2D();
    return NIL;
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
    
/* Arrow Keys */
setvar(cintern("KEY_UP"), flocons(KEY_UP), NIL);
setvar(cintern("KEY_DOWN"), flocons(KEY_DOWN), NIL);
setvar(cintern("KEY_LEFT"), flocons(KEY_LEFT), NIL);
setvar(cintern("KEY_RIGHT"), flocons(KEY_RIGHT), NIL);

/* Special Keys */
setvar(cintern("KEY_SPACE"), flocons(KEY_SPACE), NIL);
setvar(cintern("KEY_ENTER"), flocons(KEY_ENTER), NIL);
setvar(cintern("KEY_ESCAPE"), flocons(KEY_ESCAPE), NIL);
setvar(cintern("KEY_BACKSPACE"), flocons(KEY_BACKSPACE), NIL);
setvar(cintern("KEY_TAB"), flocons(KEY_TAB), NIL);

/* Modifier Keys */
setvar(cintern("KEY_SHIFT"), flocons(KEY_LEFT_SHIFT), NIL);
setvar(cintern("KEY_CONTROL"), flocons(KEY_LEFT_CONTROL), NIL);
setvar(cintern("KEY_ALT"), flocons(KEY_LEFT_ALT), NIL);

/* Function Keys */
setvar(cintern("KEY_F1"), flocons(KEY_F1), NIL);
setvar(cintern("KEY_F2"), flocons(KEY_F2), NIL);
setvar(cintern("KEY_F3"), flocons(KEY_F3), NIL);
setvar(cintern("KEY_F4"), flocons(KEY_F4), NIL);
setvar(cintern("KEY_F5"), flocons(KEY_F5), NIL);
setvar(cintern("KEY_F6"), flocons(KEY_F6), NIL);
setvar(cintern("KEY_F7"), flocons(KEY_F7), NIL);
setvar(cintern("KEY_F8"), flocons(KEY_F8), NIL);
setvar(cintern("KEY_F9"), flocons(KEY_F9), NIL);
setvar(cintern("KEY_F10"), flocons(KEY_F10), NIL);
setvar(cintern("KEY_F11"), flocons(KEY_F11), NIL);
setvar(cintern("KEY_F12"), flocons(KEY_F12), NIL);

/* Letters A-Z */
setvar(cintern("KEY_A"), flocons(KEY_A), NIL);
setvar(cintern("KEY_B"), flocons(KEY_B), NIL);
setvar(cintern("KEY_C"), flocons(KEY_C), NIL);
setvar(cintern("KEY_D"), flocons(KEY_D), NIL);
setvar(cintern("KEY_E"), flocons(KEY_E), NIL);
setvar(cintern("KEY_F"), flocons(KEY_F), NIL);
setvar(cintern("KEY_G"), flocons(KEY_G), NIL);
setvar(cintern("KEY_H"), flocons(KEY_H), NIL);
setvar(cintern("KEY_I"), flocons(KEY_I), NIL);
setvar(cintern("KEY_J"), flocons(KEY_J), NIL);
setvar(cintern("KEY_K"), flocons(KEY_K), NIL);
setvar(cintern("KEY_L"), flocons(KEY_L), NIL);
setvar(cintern("KEY_M"), flocons(KEY_M), NIL);
setvar(cintern("KEY_N"), flocons(KEY_N), NIL);
setvar(cintern("KEY_O"), flocons(KEY_O), NIL);
setvar(cintern("KEY_P"), flocons(KEY_P), NIL);
setvar(cintern("KEY_Q"), flocons(KEY_Q), NIL);
setvar(cintern("KEY_R"), flocons(KEY_R), NIL);
setvar(cintern("KEY_S"), flocons(KEY_S), NIL);
setvar(cintern("KEY_T"), flocons(KEY_T), NIL);
setvar(cintern("KEY_U"), flocons(KEY_U), NIL);
setvar(cintern("KEY_V"), flocons(KEY_V), NIL);
setvar(cintern("KEY_W"), flocons(KEY_W), NIL);
setvar(cintern("KEY_X"), flocons(KEY_X), NIL);
setvar(cintern("KEY_Y"), flocons(KEY_Y), NIL);
setvar(cintern("KEY_Z"), flocons(KEY_Z), NIL);

/* Numbers 0-9 */
setvar(cintern("KEY_ZERO"), flocons(KEY_ZERO), NIL);
setvar(cintern("KEY_ONE"), flocons(KEY_ONE), NIL);
setvar(cintern("KEY_TWO"), flocons(KEY_TWO), NIL);
setvar(cintern("KEY_THREE"), flocons(KEY_THREE), NIL);
setvar(cintern("KEY_FOUR"), flocons(KEY_FOUR), NIL);
setvar(cintern("KEY_FIVE"), flocons(KEY_FIVE), NIL);
setvar(cintern("KEY_SIX"), flocons(KEY_SIX), NIL);
setvar(cintern("KEY_SEVEN"), flocons(KEY_SEVEN), NIL);
setvar(cintern("KEY_EIGHT"), flocons(KEY_EIGHT), NIL);
setvar(cintern("KEY_NINE"), flocons(KEY_NINE), NIL);

/* Symbol Keys */
setvar(cintern("KEY_EQUAL"), flocons(KEY_EQUAL), NIL);        /* = key */
setvar(cintern("KEY_MINUS"), flocons(KEY_MINUS), NIL);        /* - key */
setvar(cintern("KEY_PLUS"), flocons(KEY_EQUAL), NIL);         /* + is shift-= */
setvar(cintern("KEY_SLASH"), flocons(KEY_SLASH), NIL);        /* / key */
setvar(cintern("KEY_BACKSLASH"), flocons(KEY_BACKSLASH), NIL);
setvar(cintern("KEY_COMMA"), flocons(KEY_COMMA), NIL);
setvar(cintern("KEY_PERIOD"), flocons(KEY_PERIOD), NIL);
setvar(cintern("KEY_SEMICOLON"), flocons(KEY_SEMICOLON), NIL);
setvar(cintern("KEY_APOSTROPHE"), flocons(KEY_APOSTROPHE), NIL);

/* Mouse Button Constants */
setvar(cintern("MOUSE_LEFT_BUTTON"), flocons(MOUSE_LEFT_BUTTON), NIL);
setvar(cintern("MOUSE_RIGHT_BUTTON"), flocons(MOUSE_RIGHT_BUTTON), NIL);
setvar(cintern("MOUSE_MIDDLE_BUTTON"), flocons(MOUSE_MIDDLE_BUTTON), NIL);
    /* Render Texture Functions */
	init_subr_2("create-render-texture", lcreate_render_texture);
	init_subr_0("begin-texture-mode", lbegin_texture_mode);
	init_subr_0("end-texture-mode", lend_texture_mode);
	init_subr_3("draw-render-texture", ldraw_render_texture);
	init_subr_0("unload-render-texture", lunload_render_texture);

	/* Camera2D Functions */
	init_subr_3("init-camera", linit_camera);
	init_subr_2("camera-set-target", lcamera_set_target);
	init_subr_2("camera-move", lcamera_move);
	init_subr_1("camera-set-zoom", lcamera_set_zoom);
	init_subr_1("camera-zoom-by", lcamera_zoom_by);
	init_subr_0("camera-get-x", lcamera_get_x);
	init_subr_0("camera-get-y", lcamera_get_y);
	init_subr_0("camera-get-zoom", lcamera_get_zoom);
	init_subr_0("begin-camera-mode", lbegin_camera_mode);
	init_subr_0("end-camera-mode", lend_camera_mode);
}
