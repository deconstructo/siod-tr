/* baroque.c - Baroque Number Systems for SIOD-TR
 * 
 * Phase 1: Complex Numbers (COMPLETE)
 * Phase 2: Polymorphic Operators (COMPLETE)
 * Phase 3: Quaternions (NEW)
 * 
 * Makes +, -, *, /, sqrt, sin, cos, etc. work with complex AND quaternion numbers!
 * 
 * PREREQUISITES in siod.h:
 *   Complex:
 *     - struct {double complex data;} cmpnum; in union
 *     - #define CMPNUM(x) ((x)->storage_as.cmpnum.data)
 *     - extern long tc_complex;
 *     - #define COMPLEXP(x) (TYPE(x) == tc_complex)
 *     - extern LISP make_complex(double real, double imag);
 *   Quaternion:
 *     - struct {double w, x, y, z;} quatnum; in union
 *     - #define QUATW(x) ((x)->storage_as.quatnum.w)
 *     - #define QUATX(x) ((x)->storage_as.quatnum.x)
 *     - #define QUATY(x) ((x)->storage_as.quatnum.y)
 *     - #define QUATZ(x) ((x)->storage_as.quatnum.z)
 *     - extern long tc_quaternion;
 *     - #define QUATERNIONP(x) (TYPE(x) == tc_quaternion)
 *     - extern LISP make_quaternion(double w, double x, double y, double z);
 *   General:
 *     - extern void init_baroque(void);
 */

#include <stdio.h>
#include "siod.h"
#include <complex.h>
#include <math.h>
#include <string.h>
#include <cqrlib.h>

/* ============================================
   COMPLEX NUMBERS - PHASE 1: CONSTRUCTORS
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
   QUATERNIONS - PHASE 3: CONSTRUCTORS
   ============================================ */

/* Create quaternion - uses SIOD's cell allocation */
LISP make_quaternion(double w, double x, double y, double z) {
    LISP q = newcell(tc_quaternion);
    CQRQuaternion *qptr = QUATPTR(q);
    qptr->w = w;
    qptr->x = x;
    qptr->y = y;
    qptr->z = z;
    return q;
}

/* (make-quaternion w x y z) or (quat w x y z) */
static LISP lmake_quaternion(LISP w, LISP x, LISP y, LISP z) {
    return make_quaternion(get_c_double(w),
                          get_c_double(x),
                          get_c_double(y),
                          get_c_double(z));
}

/* (quat-from-axis-angle axis angle) - Create rotation quaternion */
static LISP lquat_from_axis_angle(LISP axis, LISP angle) {
    double ax[3];
    double ang;
    CQRQuaternion result;
    
    /* Extract axis components */
    if (!CONSP(axis) || nlength(axis) < 3) {
        err("axis must be a list of 3 numbers", axis);
    }
    
    ax[0] = get_c_double(car(axis));
    ax[1] = get_c_double(cadr(axis));
    ax[2] = get_c_double(caddr(axis));
    ang = get_c_double(angle);
    
    /* Create rotation quaternion using CQRlib */
    CQRAxis2Quaternion(&result, ax, ang);
    
    return make_quaternion(result.w, result.x, result.y, result.z);
}

/* ============================================
   COMPLEX - PHASE 1: ACCESSORS
   ============================================ */

/* (real-part z) - Extract real component */
static LISP lreal_part(LISP z) {
    if (COMPLEXP(z)) {
        return flocons(creal(CMPNUM(z)));
    } else if (QUATERNIONP(z)) {
        return flocons(QUATW(z));  /* Real part of quaternion */
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
        err("not a complex number", z);
        return NIL;
    }
}

/* (magnitude z) - Absolute value / modulus */
static LISP lmagnitude(LISP z) {
    if (COMPLEXP(z)) {
        return flocons(cabs(CMPNUM(z)));
    } else if (QUATERNIONP(z)) {
        double norm_val;
        CQRNorm(&norm_val, QUATPTR(z));
        return flocons(norm_val);
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
        err("not a complex number", z);
        return NIL;
    }
}

/* ============================================
   QUATERNIONS - PHASE 3: ACCESSORS
   ============================================ */

static LISP lquat_w(LISP q) {
    if (!QUATERNIONP(q)) err("not a quaternion", q);
    return flocons(QUATW(q));
}

static LISP lquat_x(LISP q) {
    if (!QUATERNIONP(q)) err("not a quaternion", q);
    return flocons(QUATX(q));
}

static LISP lquat_y(LISP q) {
    if (!QUATERNIONP(q)) err("not a quaternion", q);
    return flocons(QUATY(q));
}

static LISP lquat_z(LISP q) {
    if (!QUATERNIONP(q)) err("not a quaternion", q);
    return flocons(QUATZ(q));
}

/* ============================================
   PREDICATES
   ============================================ */

/* (complex? x) - Is x a complex number? */
static LISP lcomplexp(LISP x) {
    return COMPLEXP(x) ? cintern("t") : NIL;
}

/* (quaternion? x) - Is x a quaternion? */
static LISP lquaternionp(LISP x) {
    return QUATERNIONP(x) ? cintern("t") : NIL;
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

/* Print quaternion as #Q(w x y z) */
static void quaternion_prin1(LISP ptr, struct gen_printio *f) {
    char buf[256];
    
    snprintf(buf, sizeof(buf), "#Q(%g %g %g %g)", 
             QUATW(ptr), QUATX(ptr), QUATY(ptr), QUATZ(ptr));
    gput_st(f, buf);
}

/* ============================================
   HELPER FUNCTIONS FOR POLYMORPHISM
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

/* Check if any argument is quaternion */
static int has_quaternion(LISP args) {
    LISP tmp;
    for (tmp = args; NNULLP(tmp); tmp = cdr(tmp)) {
        if (QUATERNIONP(car(tmp))) return 1;
    }
    return 0;
}

/* Convert any number (real, complex, or quaternion) to quaternion */
static LISP to_quaternion(LISP x) {
    if (QUATERNIONP(x)) {
        return x;
    } else if (COMPLEXP(x)) {
        /* Complex a+bi becomes quaternion (a, b, 0, 0) */
        double complex c = CMPNUM(x);
        return make_quaternion(creal(c), cimag(c), 0.0, 0.0);
    } else {
        /* Real becomes quaternion (r, 0, 0, 0) */
        double r = get_c_double(x);
        return make_quaternion(r, 0.0, 0.0, 0.0);
    }
}

/* ============================================
   QUATERNION ARITHMETIC (using CQRlib)
   ============================================ */

/* Add two quaternions */
static LISP quat_add(LISP q1, LISP q2) {
    CQRQuaternion result;
    
    CQRAdd(&result, QUATPTR(q1), QUATPTR(q2));
    
    /* If result is pure real (x=y=z=0), return just the real part */
    if (result.x == 0.0 && result.y == 0.0 && result.z == 0.0) {
        return flocons(result.w);
    }
    
    return make_quaternion(result.w, result.x, result.y, result.z);
}

/* Subtract two quaternions */
static LISP quat_subtract(LISP q1, LISP q2) {
    CQRQuaternion result;
    
    CQRSubtract(&result, QUATPTR(q1), QUATPTR(q2));
    
    /* If result is pure real (x=y=z=0), return just the real part */
    if (result.x == 0.0 && result.y == 0.0 && result.z == 0.0) {
        return flocons(result.w);
    }
    
    return make_quaternion(result.w, result.x, result.y, result.z);
}

/* Multiply two quaternions (non-commutative!) */
static LISP quat_multiply(LISP q1, LISP q2) {
    CQRQuaternion result;
    
    CQRMultiply(&result, QUATPTR(q1), QUATPTR(q2));
    
    /* If result is pure real (x=y=z=0), return just the real part */
    if (result.x == 0.0 && result.y == 0.0 && result.z == 0.0) {
        return flocons(result.w);
    }
    
    return make_quaternion(result.w, result.x, result.y, result.z);
}

/* Divide two quaternions */
static LISP quat_divide(LISP q1, LISP q2) {
    CQRQuaternion result;
    
    CQRDivide(&result, QUATPTR(q1), QUATPTR(q2));
    
    /* If result is pure real (x=y=z=0), return just the real part */
    if (result.x == 0.0 && result.y == 0.0 && result.z == 0.0) {
        return flocons(result.w);
    }
    
    return make_quaternion(result.w, result.x, result.y, result.z);
}

/* Scalar-quaternion operations */
static LISP quat_scale(LISP q, double s) {
    return make_quaternion(QUATW(q) * s,
                          QUATX(q) * s,
                          QUATY(q) * s,
                          QUATZ(q) * s);
}

static LISP quat_add_scalar(LISP q, double s) {
    return make_quaternion(QUATW(q) + s,
                          QUATX(q),
                          QUATY(q),
                          QUATZ(q));
}

/* ============================================
   POLYMORPHIC ARITHMETIC
   ============================================ */

/* Polymorphic + (handles real, complex, quaternion) */
static LISP lplus_baroque(LISP args) {
    /* Check for quaternions first */
    if (has_quaternion(args)) {
        LISP result = NIL;
        LISP tmp;
        
        for (tmp = args; NNULLP(tmp); tmp = cdr(tmp)) {
            LISP arg = car(tmp);
            
            if (NULLP(result)) {
                /* First argument - convert to quaternion */
                result = to_quaternion(arg);
            } else {
                /* Add next argument */
                result = quat_add(result, to_quaternion(arg));
            }
        }
        return result;
    }
    
    /* No quaternions - use complex arithmetic */
    double complex sum = 0.0 + 0.0 * I;
    LISP tmp;
    
    for (tmp = args; NNULLP(tmp); tmp = cdr(tmp)) {
        sum += to_complex(car(tmp));
    }
    
    return from_complex(sum);
}

/* Polymorphic - */
static LISP ldifference_baroque(LISP args) {
    LISP first = car(args);
    LISP rest = cdr(args);
    
    /* Check for quaternions */
    if (has_quaternion(args)) {
        /* Convert first arg to quaternion */
        LISP result = to_quaternion(first);
        
        /* Unary minus: (- q) */
        if (NULLP(rest)) {
            double w = -QUATW(result);
            double x = -QUATX(result);
            double y = -QUATY(result);
            double z = -QUATZ(result);
            
            /* If result is pure real (x=y=z=0), return just the real part */
            if (x == 0.0 && y == 0.0 && z == 0.0) {
                return flocons(w);
            }
            
            return make_quaternion(w, x, y, z);
        }
        
        /* Subtract remaining args */
        LISP tmp;
        for (tmp = rest; NNULLP(tmp); tmp = cdr(tmp)) {
            result = quat_subtract(result, to_quaternion(car(tmp)));
        }
        return result;
    }
    
    /* No quaternions - complex arithmetic */
    if (NULLP(rest)) {
        double complex z = to_complex(first);
        return from_complex(-z);
    }
    
    double complex result = to_complex(first);
    LISP tmp;
    
    for (tmp = rest; NNULLP(tmp); tmp = cdr(tmp)) {
        result -= to_complex(car(tmp));
    }
    
    return from_complex(result);
}

/* Polymorphic * */
static LISP ltimes_baroque(LISP args) {
    /* Check for quaternions */
    if (has_quaternion(args)) {
        LISP result = NIL;
        LISP tmp;
        
        for (tmp = args; NNULLP(tmp); tmp = cdr(tmp)) {
            LISP arg = car(tmp);
            
            if (NULLP(result)) {
                /* First argument - convert to quaternion */
                result = to_quaternion(arg);
            } else {
                /* Multiply by next argument */
                result = quat_multiply(result, to_quaternion(arg));
            }
        }
        return result;
    }
    
    /* No quaternions - complex arithmetic */
    double complex product = 1.0 + 0.0 * I;
    LISP tmp;
    
    for (tmp = args; NNULLP(tmp); tmp = cdr(tmp)) {
        product *= to_complex(car(tmp));
    }
    
    return from_complex(product);
}

/* Polymorphic / */
static LISP ldivide_baroque(LISP args) {
    LISP first = car(args);
    LISP rest = cdr(args);
    
    /* Check for quaternions */
    if (has_quaternion(args)) {
        /* Convert first arg to quaternion */
        LISP result = to_quaternion(first);
        
        /* Reciprocal: (/ q) */
        if (NULLP(rest)) {
            CQRQuaternion inv;
            CQRInverse(&inv, QUATPTR(result));
            
            /* If result is pure real (x=y=z=0), return just the real part */
            if (inv.x == 0.0 && inv.y == 0.0 && inv.z == 0.0) {
                return flocons(inv.w);
            }
            
            return make_quaternion(inv.w, inv.x, inv.y, inv.z);
        }
        
        /* Division */
        LISP tmp;
        for (tmp = rest; NNULLP(tmp); tmp = cdr(tmp)) {
            result = quat_divide(result, to_quaternion(car(tmp)));
        }
        return result;
    }
    
    /* No quaternions - complex arithmetic */
    if (NULLP(rest)) {
        double complex z = to_complex(first);
        return from_complex(1.0 / z);
    }
    
    double complex result = to_complex(first);
    LISP tmp;
    
    for (tmp = rest; NNULLP(tmp); tmp = cdr(tmp)) {
        result /= to_complex(car(tmp));
    }
    
    return from_complex(result);
}

/* ============================================
   POLYMORPHIC MATH FUNCTIONS
   ============================================ */

/* Polymorphic sqrt */
static LISP lsqrt_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* For quaternions, just use CQRlib's square root if available,
         * or implement using exponential/logarithm method */
        err("sqrt not yet implemented for quaternions", x);
        return NIL;
    }
    
    double complex z = to_complex(x);
    double complex result = csqrt(z);
    return from_complex(result);
}

/* Polymorphic exp */
static LISP lexp_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        CQRQuaternion result;
        CQRExp(&result, QUATPTR(x));
        return make_quaternion(result.w, result.x, result.y, result.z);
    }
    
    double complex z = to_complex(x);
    double complex result = cexp(z);
    return from_complex(result);
}

/* Polymorphic log */
static LISP llog_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("log not yet implemented for quaternions", x);
        return NIL;
    }
    
    double complex z = to_complex(x);
    double complex result = clog(z);
    return from_complex(result);
}

/* Polymorphic sin */
static LISP lsin_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("sin not yet implemented for quaternions", x);
        return NIL;
    }
    
    double complex z = to_complex(x);
    double complex result = csin(z);
    return from_complex(result);
}

/* Polymorphic cos */
static LISP lcos_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("cos not yet implemented for quaternions", x);
        return NIL;
    }
    
    double complex z = to_complex(x);
    double complex result = ccos(z);
    return from_complex(result);
}

/* Polymorphic tan */
static LISP ltan_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("tan not yet implemented for quaternions", x);
        return NIL;
    }
    
    double complex z = to_complex(x);
    double complex result = ctan(z);
    return from_complex(result);
}

static LISP lconj_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        CQRQuaternion result;
        CQRConjugate(&result, QUATPTR(x));
        return make_quaternion(result.w, result.x, result.y, result.z);
    }
    
    double complex z = to_complex(x);
    double complex result_complex = conj(z);
    return from_complex(result_complex);
}

static LISP labs_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        double norm_val;
        CQRNorm(&norm_val, QUATPTR(x));
        return flocons(norm_val);
    }
    
    double complex z = to_complex(x);
    return flocons(cabs(z));
}

/* Inverse trig functions */
static LISP lasin_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("asin not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = casin(z);
    return from_complex(result);
}

static LISP lacos_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("acos not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = cacos(z);
    return from_complex(result);
}

static LISP latan_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("atan not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = catan(z);
    return from_complex(result);
}

/* Hyperbolic trig functions */
static LISP lsinh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("sinh not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = csinh(z);
    return from_complex(result);
}

static LISP lcosh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("cosh not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = ccosh(z);
    return from_complex(result);
}

static LISP ltanh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("tanh not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = ctanh(z);
    return from_complex(result);
}

static LISP lasinh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("asinh not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = casinh(z);
    return from_complex(result);
}

static LISP lacosh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("acosh not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = cacosh(z);
    return from_complex(result);
}

static LISP latanh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("atanh not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = catanh(z);
    return from_complex(result);
}

static LISP lproj_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        err("proj not implemented for quaternions", x);
        return NIL;
    }
    double complex z = to_complex(x);
    double complex result = cproj(z);
    return from_complex(result);
}

/* ============================================
   QUATERNION-SPECIFIC OPERATIONS
   ============================================ */

/* (quat-normalize q) - Return unit quaternion */
static LISP lquat_normalize(LISP q) {
    if (!QUATERNIONP(q)) err("not a quaternion", q);
    
    double norm_val;
    CQRNorm(&norm_val, QUATPTR(q));
    
    if (norm_val < 1e-15) err("cannot normalize zero quaternion", q);
    
    return make_quaternion(QUATW(q) / norm_val,
                          QUATX(q) / norm_val,
                          QUATY(q) / norm_val,
                          QUATZ(q) / norm_val);
}

/* (quat-inverse q) - Return multiplicative inverse */
static LISP lquat_inverse(LISP q) {
    if (!QUATERNIONP(q)) err("not a quaternion", q);
    
    CQRQuaternion result;
    CQRInverse(&result, QUATPTR(q));
    
    return make_quaternion(result.w, result.x, result.y, result.z);
}

/* (quat-to-axis-angle q) - Return (axis angle) */
static LISP lquat_to_axis_angle(LISP quat) {
    if (!QUATERNIONP(quat)) err("not a quaternion", quat);
    
    double w = QUATW(quat);
    double x = QUATX(quat);
    double y = QUATY(quat);
    double z = QUATZ(quat);
    
    /* Normalize first */
    double norm = sqrt(w*w + x*x + y*y + z*z);
    if (norm < 1e-15) err("cannot convert zero quaternion to axis-angle", quat);
    
    w /= norm; x /= norm; y /= norm; z /= norm;
    
    /* Extract angle: 2 * acos(w) */
    double angle = 2.0 * acos(w);
    
    /* Extract axis */
    double s = sqrt(1.0 - w*w);
    double axis[3];
    
    if (s < 1e-10) {
        /* Angle near 0 or 2pi - axis is arbitrary */
        axis[0] = 0; axis[1] = 0; axis[2] = 1;
    } else {
        axis[0] = x / s;
        axis[1] = y / s;
        axis[2] = z / s;
    }
    
    LISP axis_list = cons(flocons(axis[0]),
                          cons(flocons(axis[1]),
                               cons(flocons(axis[2]), NIL)));
    
    return cons(axis_list, cons(flocons(angle), NIL));
}

/* (quat-rotate-vector q v) - Rotate vector by quaternion */
static LISP lquat_rotate_vector(LISP q, LISP v) {
    if (!QUATERNIONP(q)) err("not a quaternion", q);
    
    double vec[3], result[3];
    
    if (!CONSP(v) || nlength(v) < 3) {
        err("vector must be a list of 3 numbers", v);
    }
    
    vec[0] = get_c_double(car(v));
    vec[1] = get_c_double(cadr(v));
    vec[2] = get_c_double(caddr(v));
    
    CQRRotateByQuaternion(result, QUATPTR(q), vec);
    
    return cons(flocons(result[0]),
                cons(flocons(result[1]),
                     cons(flocons(result[2]), NIL)));
}

/* (quat-slerp q1 q2 t) - Spherical linear interpolation */
static LISP lquat_slerp(LISP q1, LISP q2, LISP t) {
    if (!QUATERNIONP(q1)) err("first argument not a quaternion", q1);
    if (!QUATERNIONP(q2)) err("second argument not a quaternion", q2);
    
    CQRQuaternion result;
    double tval = get_c_double(t);
    
    /* CQRSLERP takes two weights instead of single t parameter */
    double w1 = 1.0 - tval;  /* Weight for q1 */
    double w2 = tval;         /* Weight for q2 */
    
    CQRSLERP(&result, QUATPTR(q1), QUATPTR(q2), w1, w2);
    
    return make_quaternion(result.w, result.x, result.y, result.z);
}

/* ============================================
   INITIALIZATION
   ============================================ */

void init_baroque(void) {
    /* Set print hooks */
    set_print_hooks(tc_complex, complex_prin1);
    set_print_hooks(tc_quaternion, quaternion_prin1);
    
    /* ========== COMPLEX: Basic Functions ========== */
    init_subr_2("make-rectangular", lmake_rectangular);
    init_subr_2("make-polar", lmake_polar);
    
    init_subr_1("real-part", lreal_part);
    init_subr_1("imag-part", limag_part);
    init_subr_1("magnitude", lmagnitude);
    init_subr_1("angle", langle);
    
    init_subr_1("complex?", lcomplexp);
    
    /* ========== QUATERNIONS: Basic Functions ========== */
    init_subr_4("make-quaternion", lmake_quaternion);
    init_subr_4("quat", lmake_quaternion);  /* Shorter alias */
    init_subr_2("quat-from-axis-angle", lquat_from_axis_angle);
    
    init_subr_1("quat-w", lquat_w);
    init_subr_1("quat-x", lquat_x);
    init_subr_1("quat-y", lquat_y);
    init_subr_1("quat-z", lquat_z);
    
    init_subr_1("quaternion?", lquaternionp);
    
    init_subr_1("quat-normalize", lquat_normalize);
    init_subr_1("quat-inverse", lquat_inverse);
    init_subr_1("quat-to-axis-angle", lquat_to_axis_angle);
    init_subr_2("quat-rotate-vector", lquat_rotate_vector);
    init_subr_3("quat-slerp", lquat_slerp);
    
    /* ========== POLYMORPHIC OPERATORS (work with real/complex/quaternion) ========== */
    
    /* Replace standard operators with baroque versions */
    init_lsubr("+", lplus_baroque);
    init_lsubr("-", ldifference_baroque);
    init_lsubr("*", ltimes_baroque);
    init_lsubr("/", ldivide_baroque);
    
    init_subr_1("sqrt", lsqrt_baroque);
    init_subr_1("exp", lexp_baroque);
    init_subr_1("log", llog_baroque);
    init_subr_1("sin", lsin_baroque);
    init_subr_1("cos", lcos_baroque);
    init_subr_1("tan", ltan_baroque);
    init_subr_1("asin", lasin_baroque);
    init_subr_1("acos", lacos_baroque);
    init_subr_1("atan", latan_baroque);
    init_subr_1("sinh", lsinh_baroque);
    init_subr_1("cosh", lcosh_baroque);
    init_subr_1("tanh", ltanh_baroque);
    init_subr_1("asinh", lasinh_baroque);
    init_subr_1("acosh", lacosh_baroque);
    init_subr_1("atanh", latanh_baroque);
    init_subr_1("conj", lconj_baroque);
    init_subr_1("proj", lproj_baroque);
    init_subr_1("abs", labs_baroque);
}
