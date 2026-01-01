/* baroque.c - Baroque Number Systems for SIOD-TR
 * 
 * Phase 1: Complex Numbers - COMPLETE!
 * 
 * Uses SIOD's struct obj union with double complex.
 * 
 * PREREQUISITES in siod.h:
 *   - struct {double complex data;} cmpnum; in union
 *   - #define CMPNUM(x) ((x)->storage_as.cmpnum.data)
 *   - #define tc_complex 51 (or 50-100)
 *   - #define COMPLEXP(x) (TYPE(x) == tc_complex)
 *   - extern LISP make_complex(double real, double imag);
 *   - extern void init_baroque(void);
 */

#include <stdio.h>
#include "siod.h"
#include <complex.h>
#include <math.h>

/* ============================================
   CONSTRUCTORS
   ============================================ */

/* Create complex number - uses SIOD's cell allocation */
LISP make_complex(double real, double imag) {
    LISP z = newcell(tc_complex);
    CMPNUM(z) = real + imag * I;
    return z;
}

/* (make-rectangular real imag) - Standard Scheme constructor */
static LISP lmake_rectangular(LISP real_part, LISP imag_part) {
    double r = get_c_double(real_part);
    double i = get_c_double(imag_part);
    return make_complex(r, i);
}

/* (make-polar magnitude angle) - Standard Scheme constructor */
static LISP lmake_polar(LISP magnitude, LISP angle) {
    double mag = get_c_double(magnitude);
    double ang = get_c_double(angle);
    
    /* Polar to rectangular: r*e^(iθ) */
    double complex z = mag * cexp(I * ang);
    
    return make_complex(creal(z), cimag(z));
}

/* ============================================
   ACCESSORS
   ============================================ */

/* (real-part z) - Extract real component */
static LISP lreal_part(LISP z) {
    if (COMPLEXP(z)) {
        return flocons(creal(CMPNUM(z)));
    } else if (FLONUMP(z)) {
        return z;  /* Real numbers are their own real part */
    } else {
        err("not a number", z);
        return NIL;
    }
}

/* (imag-part z) - Extract imaginary component */
static LISP limag_part(LISP z) {
    if (COMPLEXP(z)) {
        return flocons(cimag(CMPNUM(z)));
    } else if (FLONUMP(z)) {
        return flocons(0.0);  /* Real numbers have zero imaginary part */
    } else {
        err("not a number", z);
        return NIL;
    }
}

/* (magnitude z) - Absolute value / modulus */
static LISP lmagnitude(LISP z) {
    if (COMPLEXP(z)) {
        return flocons(cabs(CMPNUM(z)));
    } else if (FLONUMP(z)) {
        return flocons(fabs(FLONM(z)));
    } else {
        err("not a number", z);
        return NIL;
    }
}

/* (angle z) - Phase angle / argument */
static LISP langle(LISP z) {
    if (COMPLEXP(z)) {
        return flocons(carg(CMPNUM(z)));
    } else if (FLONUMP(z)) {
        double r = FLONM(z);
        return flocons(r >= 0 ? 0.0 : M_PI);
    } else {
        err("not a number", z);
        return NIL;
    }
}

/* ============================================
   PREDICATES
   ============================================ */

/* (complex? x) - Is x a complex number? */
static LISP lcomplexp(LISP x) {
    return COMPLEXP(x) ? cintern("t") : NIL;
}

/* ============================================
   PRINTING
   ============================================ */

/* Print complex number as #C(real imag) */
static void complex_prin1(LISP ptr, struct gen_printio *f) {
    double complex c = CMPNUM(ptr);
    char buf[128];
    
    snprintf(buf, sizeof(buf), "#C(%g %g)", creal(c), cimag(c));
    gput_st(f, buf);
}

/* ============================================
   INITIALIZATION
   ============================================ */

void init_baroque(void) {
    /* Set print hook */
    set_print_hooks(tc_complex, complex_prin1);
    
    /* Register constructors - Standard Scheme names */
    init_subr_2("make-rectangular", lmake_rectangular);
    init_subr_2("make-polar", lmake_polar);
    
    /* Register accessors - Standard Scheme names */
    init_subr_1("real-part", lreal_part);
    init_subr_1("imag-part", limag_part);
    init_subr_1("magnitude", lmagnitude);
    init_subr_1("angle", langle);
    
    /* Register predicates */
    init_subr_1("complex?", lcomplexp);
}
