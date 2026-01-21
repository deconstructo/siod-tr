/* siod_plplot.c - PLplot bindings for SIOD-TR
 *
 * Scientific plotting library integration
 * Arrays represented as SIOD lists, converted at C boundary
 *
 * Author: SIOD-TR Project
 * License: LGPL (matching PLplot)
 */

#include <stddef.h>
#include <stdio.h>
#include "siod.h"
#include <plplot/plplot.h>
#include <stdlib.h>
#include <string.h>

/* Forward declaration */
void init_subr_plplot(void);

void init_plplot(void) {
	init_subr_plplot();
}

/* ================================================================
 * Array Conversion Utilities
 * ================================================================ */

/* Convert SIOD list to C array of PLFLT (double) */
static PLFLT *list2array(LISP lst, int *len) {
    int n = 0;
    LISP p;
    
    /* Count elements */
    for (p = lst; CONSP(p); p = cdr(p)) {
        n++;
    }
    
    if (n == 0) {
        *len = 0;
        return NULL;
    }
    
    /* Allocate and fill array */
    PLFLT *arr = malloc(n * sizeof(PLFLT));
    if (!arr) {
        *len = 0;
        return NULL;
    }
    
    p = lst;
    for (int i = 0; i < n; i++, p = cdr(p)) {
        arr[i] = (PLFLT)get_c_double(car(p));
    }
    
    *len = n;
    return arr;
}

/* Convert SIOD list to C array of PLINT (int) */
static PLINT *list2intarray(LISP lst, int *len) {
    int n = 0;
    LISP p;
    
    /* Count elements */
    for (p = lst; CONSP(p); p = cdr(p)) {
        n++;
    }
    
    if (n == 0) {
        *len = 0;
        return NULL;
    }
    
    /* Allocate and fill array */
    PLINT *arr = malloc(n * sizeof(PLINT));
    if (!arr) {
        *len = 0;
        return NULL;
    }
    
    p = lst;
    for (int i = 0; i < n; i++, p = cdr(p)) {
        arr[i] = (PLINT)get_c_long(car(p));
    }
    
    *len = n;
    return arr;
}

/* Allocate 2D array from SIOD list of lists */
static PLFLT **list2array2d(LISP lst, int *nx, int *ny) {
    int rows = 0;
    int cols = 0;
    LISP p, row;
    
    /* Count rows */
    for (p = lst; CONSP(p); p = cdr(p)) {
        rows++;
    }
    
    if (rows == 0) {
        *nx = 0;
        *ny = 0;
        return NULL;
    }
    
    /* Count columns from first row */
    row = car(lst);
    for (p = row; CONSP(p); p = cdr(p)) {
        cols++;
    }
    
    if (cols == 0) {
        *nx = 0;
        *ny = 0;
        return NULL;
    }
    
    /* Allocate 2D array */
    PLFLT **arr = malloc(rows * sizeof(PLFLT*));
    if (!arr) {
        *nx = 0;
        *ny = 0;
        return NULL;
    }
    
    for (int i = 0; i < rows; i++) {
        arr[i] = malloc(cols * sizeof(PLFLT));
        if (!arr[i]) {
            /* Free already allocated rows */
            for (int j = 0; j < i; j++) {
                free(arr[j]);
            }
            free(arr);
            *nx = 0;
            *ny = 0;
            return NULL;
        }
    }
    
    /* Fill array */
    p = lst;
    for (int i = 0; i < rows; i++, p = cdr(p)) {
        row = car(p);
        LISP cell = row;
        for (int j = 0; j < cols && CONSP(cell); j++, cell = cdr(cell)) {
            arr[i][j] = (PLFLT)get_c_double(car(cell));
        }
    }
    
    *nx = cols;
    *ny = rows;
    return arr;
}

/* Free 2D array */
static void free_array2d(PLFLT **arr, int ny) {
    if (!arr) return;
    for (int i = 0; i < ny; i++) {
        free(arr[i]);
    }
    free(arr);
}

/* ================================================================
 * Initialization and Setup
 * ================================================================ */

/* (plot-init) - Initialize PLplot */
LISP siod_plot_init(void) {
    plinit();
    return NIL;
}

/* (plot-end) - Close PLplot */
LISP siod_plot_end(void) {
    plend();
    return NIL;
}

/* (plot-device name) - Set output device 
 * Examples: "qtwidget", "pdfqt", "svgqt", "pngqt", "xwin"
 */
LISP siod_plot_device(LISP name) {
    char *device = get_c_string(name);
    plsdev(device);
    return NIL;
}

/* (plot-output filename) - Set output filename */
LISP siod_plot_output(LISP filename) {
    char *fname = get_c_string(filename);
    plsfnam(fname);
    return NIL;
}

/* (plot-font-size size) - Set character height */
LISP siod_plot_font_size(LISP size) {
    PLFLT s = (PLFLT)get_c_double(size);
    plschr(0.0, s);
    return NIL;
}

/* ================================================================
 * Plot Environment Setup
 * ================================================================ */

/* (plot-env xmin xmax ymin ymax) - Set up plot environment */
LISP siod_plot_env(LISP xmin, LISP xmax, LISP ymin, LISP ymax) {
    PLFLT x1 = (PLFLT)get_c_double(xmin);
    PLFLT x2 = (PLFLT)get_c_double(xmax);
    PLFLT y1 = (PLFLT)get_c_double(ymin);
    PLFLT y2 = (PLFLT)get_c_double(ymax);
    
    plenv(x1, x2, y1, y2, 0, 0);
    return NIL;
}

/* (plot-env-log xmin xmax ymin ymax axis-type)
 * axis-type: 0=linear, 10=log-x, 20=log-y, 30=log-log
 */
LISP siod_plot_env_log(LISP xmin, LISP xmax, LISP ymin, LISP ymax, LISP axis_type) {
    PLFLT x1 = (PLFLT)get_c_double(xmin);
    PLFLT x2 = (PLFLT)get_c_double(xmax);
    PLFLT y1 = (PLFLT)get_c_double(ymin);
    PLFLT y2 = (PLFLT)get_c_double(ymax);
    PLINT axis = (PLINT)get_c_long(axis_type);
    
    plenv(x1, x2, y1, y2, 0, axis);
    return NIL;
}

/* (plot-labels xlabel ylabel title) - Add axis labels and title */
LISP siod_plot_labels(LISP xlabel, LISP ylabel, LISP title) {
    char *x_label = get_c_string(xlabel);
    char *y_label = get_c_string(ylabel);
    char *plot_title = get_c_string(title);
    
    pllab(x_label, y_label, plot_title);
    return NIL;
}

/* (plot-box xopt yopt) - Draw box with custom options */
LISP siod_plot_box(LISP xopt, LISP yopt) {
    char *x_opt = get_c_string(xopt);
    char *y_opt = get_c_string(yopt);
    
    plbox(x_opt, 0.0, 0, y_opt, 0.0, 0);
    return NIL;
}

/* ================================================================
 * Color and Style
 * ================================================================ */

/* (plot-color index) - Set color from color map 0 (0-15) */
LISP siod_plot_color(LISP index) {
    PLINT color = (PLINT)get_c_long(index);
    plcol0(color);
    return NIL;
}

/* (plot-color-rgb r g b) - Set RGB color (0-255 each) */
LISP siod_plot_color_rgb(LISP r, LISP g, LISP b) {
    PLINT red = (PLINT)get_c_long(r);
    PLINT green = (PLINT)get_c_long(g);
    PLINT blue = (PLINT)get_c_long(b);
    
    plscol0(15, red, green, blue);  /* Set color 15 to custom RGB */
    plcol0(15);                      /* Use that color */
    return NIL;
}

/* (plot-width width) - Set line width */
LISP siod_plot_width(LISP width) {
    PLFLT w = (PLFLT)get_c_double(width);
    plwidth(w);
    return NIL;
}

/* (plot-background-color r g b) - Set background color */
LISP siod_plot_bg_color(LISP r, LISP g, LISP b) {
    PLINT red = (PLINT)get_c_long(r);
    PLINT green = (PLINT)get_c_long(g);
    PLINT blue = (PLINT)get_c_long(b);
    
    plscolbg(red, green, blue);
    return NIL;
}

/* ================================================================
 * 2D Plotting Functions
 * ================================================================ */

/* (plot-line x-list y-list) - Draw line plot */
LISP siod_plot_line(LISP x_list, LISP y_list) {
    int nx, ny;
    PLFLT *x = list2array(x_list, &nx);
    PLFLT *y = list2array(y_list, &ny);
    
    if (!x || !y) {
        free(x);
        free(y);
        err("failed to convert arrays", NIL);
    } else if (nx != ny) {
        free(x);
        free(y);
        err("x and y must have same length", NIL);
    }

    plline(nx, x, y);

    free(x);
    free(y);
    return NIL;
}

/* (plot-points x-list y-list symbol) - Draw points with symbols
 * symbol: code for point type (e.g., 1=dot, 2=+, 3=*, 17=filled circle)
 */
LISP siod_plot_points(LISP x_list, LISP y_list, LISP symbol) {
    int nx, ny;
    PLFLT *x = list2array(x_list, &nx);
    PLFLT *y = list2array(y_list, &ny);
    PLINT sym = (PLINT)get_c_long(symbol);
    
    if (!x || !y) {
        free(x);
        free(y);
        err("failed to convert arrays", NIL);
    }
    
    if (nx != ny) {
        free(x);
        free(y);
        err("x and y must have same length", NIL);
    }
    
    plpoin(nx, x, y, sym);
    
    free(x);
    free(y);
    return NIL;
}

/* (plot-histogram data-list datmin datmax nbins) - Draw histogram */
LISP siod_plot_histogram(LISP data_list, LISP datmin, LISP datmax, LISP nbins) {
    int n;
    PLFLT *data = list2array(data_list, &n);
    PLFLT dmin = (PLFLT)get_c_double(datmin);
    PLFLT dmax = (PLFLT)get_c_double(datmax);
    PLINT bins = (PLINT)get_c_long(nbins);
    
    if (!data) {
        err("failed to convert data array", NIL);
    }
    
    plhist(n, data, dmin, dmax, bins, 0);
    
    free(data);
    return NIL;
}

/* (plot-error-y x-list ymin-list ymax-list) - Y error bars */
LISP siod_plot_error_y(LISP x_list, LISP ymin_list, LISP ymax_list) {
    int nx, nymin, nymax;
    PLFLT *x = list2array(x_list, &nx);
    PLFLT *ymin = list2array(ymin_list, &nymin);
    PLFLT *ymax = list2array(ymax_list, &nymax);
    
    if (!x || !ymin || !ymax) {
        free(x);
        free(ymin);
        free(ymax);
        err("failed to convert arrays", NIL);
    }
    
    if (nx != nymin || nx != nymax) {
        free(x);
        free(ymin);
        free(ymax);
        err("arrays must have same length", NIL);
    }
    
    plerry(nx, x, ymin, ymax);
    
    free(x);
    free(ymin);
    free(ymax);
    return NIL;
}

/* (plot-error-x xmin-list xmax-list y-list) - X error bars */
LISP siod_plot_error_x(LISP xmin_list, LISP xmax_list, LISP y_list) {
    int nxmin, nxmax, ny;
    PLFLT *xmin = list2array(xmin_list, &nxmin);
    PLFLT *xmax = list2array(xmax_list, &nxmax);
    PLFLT *y = list2array(y_list, &ny);
    
    if (!xmin || !xmax || !y) {
        free(xmin);
        free(xmax);
        free(y);
        err("failed to convert arrays", NIL);
    }
    
    if (nxmin != nxmax || nxmin != ny) {
        free(xmin);
        free(xmax);
        free(y);
        err("arrays must have same length", NIL);
    }
    
    plerrx(nxmin, xmin, xmax, y);
    
    free(xmin);
    free(xmax);
    free(y);
    return NIL;
}

/* ================================================================
 * 3D Plotting Functions
 * ================================================================ */

/* (plot-3d-init xmin xmax ymin ymax zmin zmax altitude azimuth) 
 * Set up 3D plot environment
 */
LISP siod_plot_3d_init(LISP xmin, LISP xmax, LISP ymin, LISP ymax,
                       LISP zmin, LISP zmax, LISP altitude, LISP azimuth) {
    PLFLT x1 = (PLFLT)get_c_double(xmin);
    PLFLT x2 = (PLFLT)get_c_double(xmax);
    PLFLT y1 = (PLFLT)get_c_double(ymin);
    PLFLT y2 = (PLFLT)get_c_double(ymax);
    PLFLT z1 = (PLFLT)get_c_double(zmin);
    PLFLT z2 = (PLFLT)get_c_double(zmax);
    PLFLT alt = (PLFLT)get_c_double(altitude);
    PLFLT az = (PLFLT)get_c_double(azimuth);
    
    pladv(0);
    plvpor(0.0, 1.0, 0.0, 0.9);
    plwind(-1.0, 1.0, -0.9, 1.1);
    plw3d(1.0, 1.0, 1.0, x1, x2, y1, y2, z1, z2, alt, az);
    
    return NIL;
}

/* (plot-3d-box xlabel ylabel zlabel) - Draw 3D box with labels */
LISP siod_plot_3d_box(LISP xlabel, LISP ylabel, LISP zlabel) {
    char *x_label = get_c_string(xlabel);
    char *y_label = get_c_string(ylabel);
    char *z_label = get_c_string(zlabel);
    
    plbox3("bnstu", x_label, 0.0, 0,
           "bnstu", y_label, 0.0, 0,
           "bcdmnstuv", z_label, 0.0, 0);
    
    return NIL;
}

/* (plot-3d-line x-list y-list z-list) - Draw 3D line */
LISP siod_plot_3d_line(LISP x_list, LISP y_list, LISP z_list) {
    int nx, ny, nz;
    PLFLT *x = list2array(x_list, &nx);
    PLFLT *y = list2array(y_list, &ny);
    PLFLT *z = list2array(z_list, &nz);
    
    if (!x || !y || !z) {
        free(x);
        free(y);
        free(z);
        err("failed to convert arrays", NIL);
    }
    
    if (nx != ny || nx != nz) {
        free(x);
        free(y);
        free(z);
        err("arrays must have same length", NIL);
    }
    
    plline3(nx, x, y, z);
    
    free(x);
    free(y);
    free(z);
    return NIL;
}

/* (plot-3d-surface x-list y-list z-grid) 
 * z-grid is a list of lists (2D array)
 */
LISP siod_plot_3d_surface(LISP x_list, LISP y_list, LISP z_grid) {
    int nx, ny, nz_cols, nz_rows;
    PLFLT *x = list2array(x_list, &nx);
    PLFLT *y = list2array(y_list, &ny);
    PLFLT **z = list2array2d(z_grid, &nz_cols, &nz_rows);
    
    if (!x || !y || !z) {
        free(x);
        free(y);
        free_array2d(z, nz_rows);
        err("failed to convert arrays", NIL);
    }
    
    if (nx != nz_cols || ny != nz_rows) {
        free(x);
        free(y);
        free_array2d(z, nz_rows);
        err("dimension mismatch in surface plot", NIL);
    }
    
    /* Surface plot with default options */
    plsurf3d(x, y, (const PLFLT * const *)z, nx, ny, 0, NULL, 0);
    
    free(x);
    free(y);
    free_array2d(z, nz_rows);
    return NIL;
}

/* (plot-3d-mesh x-list y-list z-grid)
 * Draw 3D mesh (wireframe)
 */
LISP siod_plot_3d_mesh(LISP x_list, LISP y_list, LISP z_grid) {
    int nx, ny, nz_cols, nz_rows;
    PLFLT *x = list2array(x_list, &nx);
    PLFLT *y = list2array(y_list, &ny);
    PLFLT **z = list2array2d(z_grid, &nz_cols, &nz_rows);
    
    if (!x || !y || !z) {
        free(x);
        free(y);
        free_array2d(z, nz_rows);
        err("failed to convert arrays", NIL);
    }
    
    if (nx != nz_cols || ny != nz_rows) {
        free(x);
        free(y);
        free_array2d(z, nz_rows);
        err("dimension mismatch in mesh plot", NIL);
    }
    
    plmesh(x, y, (const PLFLT * const *)z, nx, ny, 3);
    
    free(x);
    free(y);
    free_array2d(z, nz_rows);
    return NIL;
}

/* ================================================================
 * Multi-plot Layout
 * ================================================================ */

/* (plot-subplot nx ny) - Divide page into nx by ny subplots */
LISP siod_plot_subplot(LISP nx, LISP ny) {
    PLINT x = (PLINT)get_c_long(nx);
    PLINT y = (PLINT)get_c_long(ny);
    
    plssub(x, y);
    return NIL;
}

/* (plot-advance) - Advance to next subplot */
LISP siod_plot_advance(void) {
    pladv(0);
    return NIL;
}

/* ================================================================
 * Text and Annotations
 * ================================================================ */

/* (plot-text x y text) - Draw text at position */
LISP siod_plot_text(LISP x, LISP y, LISP text) {
    PLFLT px = (PLFLT)get_c_double(x);
    PLFLT py = (PLFLT)get_c_double(y);
    char *str = get_c_string(text);
    
    plptex(px, py, 1.0, 0.0, 0.5, str);
    return NIL;
}

/* (plot-mtex side disp pos justification text) - Margin text */
LISP siod_plot_mtex(LISP side, LISP disp, LISP pos, LISP just, LISP text) {
    char *s = get_c_string(side);
    PLFLT d = (PLFLT)get_c_double(disp);
    PLFLT p = (PLFLT)get_c_double(pos);
    PLFLT j = (PLFLT)get_c_double(just);
    char *str = get_c_string(text);
    
    plmtex(s, d, p, j, str);
    return NIL;
}

/* ================================================================
 * Utility Functions
 * ================================================================ */
/* list available devices */
LISP siod_plot_list_devices(void) {
    /* PLplot doesn't have a clean API to enumerate devices at runtime.
     * The best we can do is return a list of commonly available devices.
     * The actual availability depends on which drivers are installed.
     */
    
    const char *common_devices[] = {
        "pdf", "pdfqt", "svg", "svgqt", "ps", "psc",
        "png", "pngqt", "xwin", "qtwidget", "xcairo",
        "pngcairo", "pdfcairo", "pscairo", "svgcairo",
        "epsqt", "null", "mem",
        NULL
    };
    
    LISP result = NIL;
    int i;
    
    /* Build list in reverse */
    for (i = 0; common_devices[i] != NULL; i++) {
        /* Intentionally empty - just counting */
    }
    
    /* Build in reverse order so final list is correct */
    for (i = i - 1; i >= 0; i--) {
        const char *dev = common_devices[i];
        result = cons(strcons(strlen(dev), dev), result);
    }
    
    return result;
}

/* (plot-clear) - Clear current plot */
LISP siod_plot_clear(void) {
    plclear();
    return NIL;
}

/* (plot-flush) - Flush plot output */
LISP siod_plot_flush(void) {
    plflush();
    return NIL;
}

/* (plot-version) - Get PLplot version string */
LISP siod_plot_version(void) {
    char ver[80];
    plgver(ver);
    return strcons(strlen(ver), ver);
}

/* ================================================================
 * Initialization - Register all primitives
 * ================================================================ */

void init_subr_plplot(void) {
    /* Setup and teardown */
    init_subr_0("plot-init", siod_plot_init);
    init_subr_0("plot-end", siod_plot_end);
    init_subr_1("plot-device", siod_plot_device);
    init_subr_1("plot-output", siod_plot_output);
    init_subr_1("plot-font-size", siod_plot_font_size);
    
    /* Plot environment */
    init_subr_4("plot-env", siod_plot_env);
    init_subr_5("plot-env-log", siod_plot_env_log);
    init_subr_3("plot-labels", siod_plot_labels);
    init_subr_2("plot-box", siod_plot_box);
    
    /* Color and style */
    init_subr_1("plot-color", siod_plot_color);
    init_subr_3("plot-color-rgb", siod_plot_color_rgb);
    init_subr_1("plot-width", siod_plot_width);
    init_subr_3("plot-background-color", siod_plot_bg_color);
    
    /* 2D plotting */
    init_subr_2("plot-line", siod_plot_line);
    init_subr_3("plot-points", siod_plot_points);
    init_subr_4("plot-histogram", siod_plot_histogram);
    init_subr_3("plot-error-y", siod_plot_error_y);
    init_subr_3("plot-error-x", siod_plot_error_x);
    
    /* 3D plotting */
    init_subr_8("plot-3d-init", siod_plot_3d_init);
    init_subr_3("plot-3d-box", siod_plot_3d_box);
    init_subr_3("plot-3d-line", siod_plot_3d_line);
    init_subr_3("plot-3d-surface", siod_plot_3d_surface);
    init_subr_3("plot-3d-mesh", siod_plot_3d_mesh);
    
    /* Multi-plot layout */
    init_subr_2("plot-subplot", siod_plot_subplot);
    init_subr_0("plot-advance", siod_plot_advance);
    
    /* Text and annotations */
    init_subr_3("plot-text", siod_plot_text);
    init_subr_5("plot-mtex", siod_plot_mtex);
    
    /* Utilities */
    init_subr_0("plot-list-devices",siod_plot_list_devices);
    init_subr_0("plot-clear", siod_plot_clear);
    init_subr_0("plot-flush", siod_plot_flush);
    init_subr_0("plot-version", siod_plot_version);
}
