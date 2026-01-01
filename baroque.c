/* baroque.c - Baroque Number Systems for SIOD-TR
 * 
 * Phase 1: Complex Numbers (COMPLETE)
 * Phase 2: Polymorphic Operators (COMPLETE)
 * 
 * Makes +, -, *, /, sqrt, sin, cos, etc. work with complex numbers!
 * 
 * PREREQUISITES in siod.h:
 *   - struct {double complex data;} cmpnum; in union
 *   - #define CMPNUM(x) ((x)->storage_as.cmpnum.data)
 *   - extern long tc_complex;
 *   - #define COMPLEXP(x) (TYPE(x) == tc_complex)
 *   - extern LISP make_complex(double real, double imag);
 *   - extern void init_baroque(void);
 */

#include <stdio.h>
#include "siod.h"
#include <complex.h>
#include <math.h>

/* ============================================
   PHASE 1: CONSTRUCTORS
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
   PHASE 1: ACCESSORS
   ============================================ */

/* (real-part z) - Extract real component */
static LISP lreal_part(LISP z) {
    if (COMPLEXP(z)) {
        return flocons(creal(CMPNUM(z)));
    } else if (FLONUMP(z)) {
        return z;
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
        return flocons(0.0);
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
   PHASE 1: PREDICATES
   ============================================ */

/* (complex? x) - Is x a complex number? */
static LISP lcomplexp(LISP x) {
    return COMPLEXP(x) ? cintern("t") : NIL;
}

/* ============================================
   PHASE 1: PRINTING
   ============================================ */

/* Print complex number as #C(real imag) */
static void complex_prin1(LISP ptr, struct gen_printio *f) {
    double complex c = CMPNUM(ptr);
    char buf[128];
    
    snprintf(buf, sizeof(buf), "#C(%g %g)", creal(c), cimag(c));
    gput_st(f, buf);
}

/* ============================================
   PHASE 2: HELPER FUNCTIONS
   ============================================ */

/* Convert LISP number to C complex (works for real or complex) */
static double complex to_complex(LISP x) {
    if (COMPLEXP(x)) {
        return CMPNUM(x);
    } else {
        return get_c_double(x) + 0.0 * I;
    }
}

/* Convert C complex to LISP (returns real if imaginary part is zero) */
static LISP from_complex(double complex z) {
    double im = cimag(z);
    
    /* If imaginary part is effectively zero, return real */
    if (fabs(im) < 1e-15) {
        return flocons(creal(z));
    }
    
    return make_complex(creal(z), im);
}

/* ============================================
   PHASE 2: POLYMORPHIC ARITHMETIC
   ============================================ */

/* Polymorphic + */
static LISP lplus_complex(LISP args) {
    double complex sum = 0.0 + 0.0 * I;
    LISP tmp;
    
    for (tmp = args; NNULLP(tmp); tmp = cdr(tmp)) {
        sum += to_complex(car(tmp));
    }
    
    return from_complex(sum);
}

/* Polymorphic - */
static LISP ldifference_complex(LISP args) {
    LISP first = car(args);
    LISP rest = cdr(args);
    
    /* Unary minus: (- x) */
    if (NULLP(rest)) {
        double complex z = to_complex(first);
        return from_complex(-z);
    }
    
    /* Binary/n-ary: (- x y z ...) */
    double complex result = to_complex(first);
    LISP tmp;
    
    for (tmp = rest; NNULLP(tmp); tmp = cdr(tmp)) {
        result -= to_complex(car(tmp));
    }
    
    return from_complex(result);
}

/* Polymorphic * */
static LISP ltimes_complex(LISP args) {
    double complex product = 1.0 + 0.0 * I;
    LISP tmp;
    
    for (tmp = args; NNULLP(tmp); tmp = cdr(tmp)) {
        product *= to_complex(car(tmp));
    }
    
    return from_complex(product);
}

/* Polymorphic / */
static LISP ldivide_complex(LISP args) {
    LISP first = car(args);
    LISP rest = cdr(args);
    
    /* Reciprocal: (/ x) */
    if (NULLP(rest)) {
        double complex z = to_complex(first);
        return from_complex(1.0 / z);
    }
    
    /* Division: (/ x y z ...) */
    double complex result = to_complex(first);
    LISP tmp;
    
    for (tmp = rest; NNULLP(tmp); tmp = cdr(tmp)) {
        result /= to_complex(car(tmp));
    }
    
    return from_complex(result);
}

/* ============================================
   PHASE 2: POLYMORPHIC MATH FUNCTIONS
   ============================================ */

/* Polymorphic sqrt - THE BIG ONE! */
static LISP lsqrt_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = csqrt(z);
    return from_complex(result);
}

/* Polymorphic exp */
static LISP lexp_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = cexp(z);
    return from_complex(result);
}

/* Polymorphic log */
static LISP llog_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = clog(z);
    return from_complex(result);
}

/* Polymorphic sin */
static LISP lsin_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = csin(z);
    return from_complex(result);
}

/* Polymorphic cos */
static LISP lcos_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = ccos(z);
    return from_complex(result);
}

/* Polymorphic tan */
static LISP ltan_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = ctan(z);
    return from_complex(result);
}

static LISP lconj_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = conj(z);
    return from_complex(result);
}

static LISP labs_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = cabs(z);
    return from_complex(result);
}

/* inverse trig functions */

static LISP lasin_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = casin(z);
    return from_complex(result);
}

static LISP lacos_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = cacos(z);
    return from_complex(result);
}

static LISP latan_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = catan(z);
    return from_complex(result);
}

/* hyperbolic trig functions */
static LISP lsinh_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = csinh(z);
    return from_complex(result);
}
static LISP lcosh_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = ccosh(z);
    return from_complex(result);
}

static LISP ltanh_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = ctanh(z);
    return from_complex(result);
}

static LISP lasinh_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = casinh(z);
    return from_complex(result);
}

static LISP lacosh_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = cacosh(z);
    return from_complex(result);
}

static LISP latanh_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = catanh(z);
    return from_complex(result);
}

static LISP lproj_complex(LISP x) {
    double complex z = to_complex(x);
    double complex result = cproj(z);
    return from_complex(result);
}





/* ============================================
   INITIALIZATION
   ============================================ */

void init_baroque(void) {
    /* Set print hooks (Phase 1 setup) */
    set_print_hooks(tc_complex, complex_prin1);
    
    /* ========== PHASE 1: Basic Functions ========== */
    
    init_subr_2("make-rectangular", lmake_rectangular);
    init_subr_2("make-polar", lmake_polar);
    
    init_subr_1("real-part", lreal_part);
    init_subr_1("imag-part", limag_part);
    init_subr_1("magnitude", lmagnitude);
    init_subr_1("angle", langle);
    
    init_subr_1("complex?", lcomplexp);
    
    /* ========== PHASE 2: Polymorphic Operators ========== */
    
    /* Replace standard operators with polymorphic versions */
    init_lsubr("+", lplus_complex);
    init_lsubr("-", ldifference_complex);
    init_lsubr("*", ltimes_complex);
    init_lsubr("/", ldivide_complex);
    
    init_subr_1("sqrt", lsqrt_complex);
    init_subr_1("exp", lexp_complex);
    init_subr_1("log", llog_complex);
    init_subr_1("sin", lsin_complex);
    init_subr_1("cos", lcos_complex);
    init_subr_1("tan", ltan_complex);
    init_subr_1("asin", lasin_complex);
    init_subr_1("acos", lacos_complex);
    init_subr_1("atan", latan_complex);

    init_subr_1("sinh", lsinh_complex);
    init_subr_1("cosh", lcosh_complex);
    init_subr_1("tanh", ltanh_complex);
    init_subr_1("asinh", lasinh_complex);
    init_subr_1("acosh", lacosh_complex);
    init_subr_1("atanh", latanh_complex);


    init_subr_1("conj", lconj_complex);
    init_subr_1("proj", lproj_complex);
    init_subr_1("abs", labs_complex);

}
