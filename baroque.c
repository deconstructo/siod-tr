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
#include <cqrlib.h> 	// Quaternions
#include <octonion.h>	// Octonions

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
   OCTONIONS - CONSTRUCTORS
   ============================================ */

/* Create octonion - uses SIOD's cell allocation */
LISP make_octonion(double e0, double e1, double e2, double e3,
                   double e4, double e5, double e6, double e7) {
    LISP o = newcell(tc_octonion);
    octonion *optr = OCTPTR(o);
    oct_make(optr, e0, e1, e2, e3, e4, e5, e6, e7);
    return o;
}

/* (make-octonion e0 e1 e2 e3 e4 e5 e6 e7) or (oct e0 e1 e2 e3 e4 e5 e6 e7) */
static LISP lmake_octonion(LISP e0, LISP e1, LISP e2, LISP e3,
                           LISP e4, LISP e5, LISP e6, LISP e7) {
    return make_octonion(get_c_double(e0), get_c_double(e1),
                        get_c_double(e2), get_c_double(e3),
                        get_c_double(e4), get_c_double(e5),
                        get_c_double(e6), get_c_double(e7));
}


/* ============================================
   COMPLEX - ACCESSORS
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
   QUATERNIONS - ACCESSORS
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
   OCTONIONS - ACCESSORS
   ============================================ */

static LISP loct_real(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    return flocons(OCTPTR(o)->e[0]);
}

static LISP loct_e1(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    return flocons(OCTPTR(o)->e[1]);
}

static LISP loct_e2(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    return flocons(OCTPTR(o)->e[2]);
}

static LISP loct_e3(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    return flocons(OCTPTR(o)->e[3]);
}

static LISP loct_e4(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    return flocons(OCTPTR(o)->e[4]);
}

static LISP loct_e5(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    return flocons(OCTPTR(o)->e[5]);
}

static LISP loct_e6(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    return flocons(OCTPTR(o)->e[6]);
}

static LISP loct_e7(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    return flocons(OCTPTR(o)->e[7]);
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

/* (octonion? x) - Is x an octonion? */
static LISP loctonionp(LISP x) {
    return OCTONIONP(x) ? cintern("t") : NIL;
}

/* (number? x) - Is x any kind of number (float, complex, quaternion, octonion)? */
static LISP lnumberp_baroque(LISP x) {
    return (FLONUMP(x) || COMPLEXP(x) || QUATERNIONP(x) ||
		    OCTONIONP(x)) ? cintern("t") : NIL;
}

/* ============================================
   PRINTING
   ============================================ */

/* Print complex number as #C(real imag) */
static void complex_prin1(LISP ptr, struct gen_printio *f) {
    double complex c = CMPNUM(ptr);
    char buf[128];
    
    snprintf(buf, sizeof(buf), "#ℂ(%g %g)", creal(c), cimag(c));
    gput_st(f, buf);
}

/* Print quaternion as #Q(w x y z) */
static void quaternion_prin1(LISP ptr, struct gen_printio *f) {
    char buf[256];
    
    snprintf(buf, sizeof(buf), "#ℍ(%g %g %g %g)", 
             QUATW(ptr), QUATX(ptr), QUATY(ptr), QUATZ(ptr));
    gput_st(f, buf);
}

/* Print octonion as #O(e0 e1 e2 e3 e4 e5 e6 e7) */
static void octonion_prin1(LISP ptr, struct gen_printio *f) {
    char buf[512];
    octonion *o = OCTPTR(ptr);
    
    snprintf(buf, sizeof(buf), "#𝕆(%g %g %g %g %g %g %g %g)", 
             o->e[0], o->e[1], o->e[2], o->e[3],
             o->e[4], o->e[5], o->e[6], o->e[7]);
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
        double w = QUATW(x);
        double qx = QUATX(x);
        double qy = QUATY(x);
        double qz = QUATZ(x);
        
        /* Pure real case */
        if (qx == 0.0 && qy == 0.0 && qz == 0.0) {
            if (w >= 0.0) {
                return flocons(sqrt(w));
            } else {
                /* sqrt of negative real is pure imaginary quaternion */
                return make_quaternion(0.0, sqrt(-w), 0.0, 0.0);
            }
        }
        
        /* General quaternion case: sqrt(q) = sqrt((|q| + w)/2) + (x,y,z)/(2*sqrt((|q|+w)/2)) */
        double norm = sqrt(w*w + qx*qx + qy*qy + qz*qz);
        double s = sqrt((norm + w) / 2.0);
        double factor = 1.0 / (2.0 * s);
        
        return make_quaternion(s, qx * factor, qy * factor, qz * factor);
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
        double w = QUATW(x);
        double qx = QUATX(x);
        double qy = QUATY(x);
        double qz = QUATZ(x);
        
        /* Check for zero */
        double norm = sqrt(w*w + qx*qx + qy*qy + qz*qz);
        if (norm < 1e-15) {
            err("log of zero quaternion", x);
            return NIL;
        }
        
        /* Pure real case */
        if (qx == 0.0 && qy == 0.0 && qz == 0.0) {
            if (w > 0.0) {
                return flocons(log(w));
            } else {
                /* log(-r) = log(r) + πi */
                return make_quaternion(log(-w), M_PI, 0.0, 0.0);
            }
        }
        
        /* General case: log(q) = log(|q|) + (v/|v|) * acos(w/|q|)
         * where v = (x,y,z) is the vector part */
        double v_norm = sqrt(qx*qx + qy*qy + qz*qz);
        double theta = acos(w / norm);
        double factor = theta / v_norm;
        
        return make_quaternion(log(norm), 
                              qx * factor, 
                              qy * factor, 
                              qz * factor);
    }
    
    double complex z = to_complex(x);
    double complex result = clog(z);
    return from_complex(result);
}

/* Polymorphic sin */
static LISP lsin_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* Slice-regular formula: sin(q) = sin(a)·cosh(|v|) + u·cos(a)·sinh(|v|)
         * where q = a + v, a = scalar part, v = vector part, u = v/|v| */
        
        double a = QUATW(x);  /* Scalar part */
        double vx = QUATX(x);
        double vy = QUATY(x);
        double vz = QUATZ(x);
        
        /* Compute |v| */
        double v_norm = sqrt(vx*vx + vy*vy + vz*vz);
        
        if (v_norm < 1e-15) {
            /* Pure real quaternion: sin(a + 0) = sin(a) */
            return flocons(sin(a));
        }
        
        /* Unit vector u = v/|v| */
        double ux = vx / v_norm;
        double uy = vy / v_norm;
        double uz = vz / v_norm;
        
        /* sin(q) = sin(a)·cosh(|v|) + u·cos(a)·sinh(|v|) */
        double sin_a = sin(a);
        double cos_a = cos(a);
        double cosh_v = cosh(v_norm);
        double sinh_v = sinh(v_norm);
        
        double w = sin_a * cosh_v;
        double scale = cos_a * sinh_v;
        
        return make_quaternion(w, ux * scale, uy * scale, uz * scale);
    }
    
    double complex z = to_complex(x);
    double complex result = csin(z);
    return from_complex(result);
}

/* Polymorphic cos */
static LISP lcos_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* Slice-regular formula: cos(q) = cos(a)·cosh(|v|) - u·sin(a)·sinh(|v|)
         * where q = a + v, a = scalar part, v = vector part, u = v/|v| */
        
        double a = QUATW(x);  /* Scalar part */
        double vx = QUATX(x);
        double vy = QUATY(x);
        double vz = QUATZ(x);
        
        /* Compute |v| */
        double v_norm = sqrt(vx*vx + vy*vy + vz*vz);
        
        if (v_norm < 1e-15) {
            /* Pure real quaternion: cos(a + 0) = cos(a) */
            return flocons(cos(a));
        }
        
        /* Unit vector u = v/|v| */
        double ux = vx / v_norm;
        double uy = vy / v_norm;
        double uz = vz / v_norm;
        
        /* cos(q) = cos(a)·cosh(|v|) - u·sin(a)·sinh(|v|) */
        double sin_a = sin(a);
        double cos_a = cos(a);
        double cosh_v = cosh(v_norm);
        double sinh_v = sinh(v_norm);
        
        double w = cos_a * cosh_v;
        double scale = -sin_a * sinh_v;
        
        return make_quaternion(w, ux * scale, uy * scale, uz * scale);
    }
    
    double complex z = to_complex(x);
    double complex result = ccos(z);
    return from_complex(result);
}

/* Polymorphic tan */
static LISP ltan_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* tan(q) = sin(q) / cos(q) */
        LISP sin_q = lsin_baroque(x);
        LISP cos_q = lcos_baroque(x);
        
        /* Divide sin by cos */
        if (QUATERNIONP(sin_q) || QUATERNIONP(cos_q)) {
            LISP sin_quat = QUATERNIONP(sin_q) ? sin_q : to_quaternion(sin_q);
            LISP cos_quat = QUATERNIONP(cos_q) ? cos_q : to_quaternion(cos_q);
            return quat_divide(sin_quat, cos_quat);
        } else {
            /* Both simplified to real/complex */
            double complex sin_c = to_complex(sin_q);
            double complex cos_c = to_complex(cos_q);
            return from_complex(sin_c / cos_c);
        }
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
        /* asin(q) = -u * log(u*q + sqrt(1 - q²))
         * where u is the quaternion's own imaginary unit direction */
        
        //double w = QUATW(x);
        double vx = QUATX(x);
        double vy = QUATY(x);
        double vz = QUATZ(x);
        
        /* Get the imaginary unit direction */
        double v_norm = sqrt(vx*vx + vy*vy + vz*vz);
        
        LISP u;
        if (v_norm < 1e-15) {
            /* Pure real - use i as default */
            u = make_quaternion(0, 1, 0, 0);
        } else {
            /* Use quaternion's own direction */
            u = make_quaternion(0, vx/v_norm, vy/v_norm, vz/v_norm);
        }
        
        LISP one = make_quaternion(1, 0, 0, 0);
        
        /* q² */
        LISP q_squared = quat_multiply(x, x);
        
        /* 1 - q² */
        LISP one_minus_q2 = quat_subtract(one, q_squared);
        
        /* sqrt(1 - q²) */
        LISP sqrt_part = lsqrt_baroque(one_minus_q2);
        
        /* u*q */
        LISP uq = quat_multiply(u, x);
        
        /* u*q + sqrt(1 - q²) */
        LISP sum = QUATERNIONP(sqrt_part) ? quat_add(uq, sqrt_part) : quat_add(uq, to_quaternion(sqrt_part));
        
        /* log(...) */
        LISP log_result = llog_baroque(sum);
        
        /* -u */
        LISP neg_u = make_quaternion(0, -QUATX(u), -QUATY(u), -QUATZ(u));
        
        /* -u * log(...) */
        LISP result = QUATERNIONP(log_result) ? quat_multiply(neg_u, log_result) : quat_multiply(neg_u, to_quaternion(log_result));
        
        return result;
    }
    double complex z = to_complex(x);
    double complex result = casin(z);
    return from_complex(result);
}

static LISP lacos_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* acos(q) = -u * log(q + sqrt(q² - 1))
         * where u is the quaternion's own imaginary unit direction */
        
        //double w = QUATW(x);
        double vx = QUATX(x);
        double vy = QUATY(x);
        double vz = QUATZ(x);
        
        /* Get the imaginary unit direction */
        double v_norm = sqrt(vx*vx + vy*vy + vz*vz);
        
        LISP u;
        if (v_norm < 1e-15) {
            /* Pure real - use i as default */
            u = make_quaternion(0, 1, 0, 0);
        } else {
            /* Use quaternion's own direction */
            u = make_quaternion(0, vx/v_norm, vy/v_norm, vz/v_norm);
        }
        
        LISP one = make_quaternion(1, 0, 0, 0);
        
        /* q² */
        LISP q_squared = quat_multiply(x, x);
        
        /* q² - 1 */
        LISP q2_minus_one = quat_subtract(q_squared, one);
        
        /* sqrt(q² - 1) */
        LISP sqrt_part = lsqrt_baroque(q2_minus_one);
        
        /* q + sqrt(q² - 1) */
        LISP sum = QUATERNIONP(sqrt_part) ? quat_add(x, sqrt_part) : quat_add(x, to_quaternion(sqrt_part));
        
        /* log(...) */
        LISP log_result = llog_baroque(sum);
        
        /* -u */
        LISP neg_u = make_quaternion(0, -QUATX(u), -QUATY(u), -QUATZ(u));
        
        /* -u * log(...) */
        LISP result = QUATERNIONP(log_result) ? quat_multiply(neg_u, log_result) : quat_multiply(neg_u, to_quaternion(log_result));
        
        return result;
    }
    double complex z = to_complex(x);
    double complex result = cacos(z);
    return from_complex(result);
}

static LISP latan_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* atan(q) = (u/2) * log((u+q)/(u-q))
         * where u is the quaternion's own imaginary unit direction */
        
        //double w = QUATW(x);
        double vx = QUATX(x);
        double vy = QUATY(x);
        double vz = QUATZ(x);
        
        /* Get the imaginary unit direction */
        double v_norm = sqrt(vx*vx + vy*vy + vz*vz);
        
        LISP u;
        if (v_norm < 1e-15) {
            /* Pure real - use i as default */
            u = make_quaternion(0, 1, 0, 0);
        } else {
            /* Use quaternion's own direction */
            u = make_quaternion(0, vx/v_norm, vy/v_norm, vz/v_norm);
        }
        
        /* u + q */
        LISP u_plus_q = quat_add(u, x);
        
        /* u - q */
        LISP u_minus_q = quat_subtract(u, x);
        
        /* (u+q)/(u-q) */
        LISP ratio = quat_divide(u_plus_q, u_minus_q);
        
        /* log(...) */
        LISP log_result = llog_baroque(ratio);
        
        /* u/2 */
        LISP u_over_2 = make_quaternion(0, QUATX(u)*0.5, QUATY(u)*0.5, QUATZ(u)*0.5);
        
        /* (u/2) * log(...) */
        LISP result = QUATERNIONP(log_result) ? quat_multiply(u_over_2, log_result) : quat_multiply(u_over_2, to_quaternion(log_result));
        
        return result;
    }
    double complex z = to_complex(x);
    double complex result = catan(z);
    return from_complex(result);
}

/* Hyperbolic trig functions */
static LISP lsinh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* Slice-regular formula: sinh(q) = sinh(a)·cos(|v|) + u·cosh(a)·sin(|v|)
         * where q = a + v, a = scalar part, v = vector part, u = v/|v| */
        
        double a = QUATW(x);  /* Scalar part */
        double vx = QUATX(x);
        double vy = QUATY(x);
        double vz = QUATZ(x);
        
        /* Compute |v| */
        double v_norm = sqrt(vx*vx + vy*vy + vz*vz);
        
        if (v_norm < 1e-15) {
            /* Pure real quaternion: sinh(a + 0) = sinh(a) */
            return flocons(sinh(a));
        }
        
        /* Unit vector u = v/|v| */
        double ux = vx / v_norm;
        double uy = vy / v_norm;
        double uz = vz / v_norm;
        
        /* sinh(q) = sinh(a)·cos(|v|) + u·cosh(a)·sin(|v|) */
        double sinh_a = sinh(a);
        double cosh_a = cosh(a);
        double cos_v = cos(v_norm);
        double sin_v = sin(v_norm);
        
        double w = sinh_a * cos_v;
        double scale = cosh_a * sin_v;
        
        return make_quaternion(w, ux * scale, uy * scale, uz * scale);
    }
    double complex z = to_complex(x);
    double complex result = csinh(z);
    return from_complex(result);
}

static LISP lcosh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* Slice-regular formula: cosh(q) = cosh(a)·cos(|v|) + u·sinh(a)·sin(|v|)
         * where q = a + v, a = scalar part, v = vector part, u = v/|v| */
        
        double a = QUATW(x);  /* Scalar part */
        double vx = QUATX(x);
        double vy = QUATY(x);
        double vz = QUATZ(x);
        
        /* Compute |v| */
        double v_norm = sqrt(vx*vx + vy*vy + vz*vz);
        
        if (v_norm < 1e-15) {
            /* Pure real quaternion: cosh(a + 0) = cosh(a) */
            return flocons(cosh(a));
        }
        
        /* Unit vector u = v/|v| */
        double ux = vx / v_norm;
        double uy = vy / v_norm;
        double uz = vz / v_norm;
        
        /* cosh(q) = cosh(a)·cos(|v|) + u·sinh(a)·sin(|v|) */
        double sinh_a = sinh(a);
        double cosh_a = cosh(a);
        double cos_v = cos(v_norm);
        double sin_v = sin(v_norm);
        
        double w = cosh_a * cos_v;
        double scale = sinh_a * sin_v;
        
        return make_quaternion(w, ux * scale, uy * scale, uz * scale);
    }
    double complex z = to_complex(x);
    double complex result = ccosh(z);
    return from_complex(result);
}

static LISP ltanh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* tanh(q) = sinh(q) / cosh(q) */
        LISP sinh_q = lsinh_baroque(x);
        LISP cosh_q = lcosh_baroque(x);
        
        /* Divide sinh by cosh */
        if (QUATERNIONP(sinh_q) || QUATERNIONP(cosh_q)) {
            LISP sinh_quat = QUATERNIONP(sinh_q) ? sinh_q : to_quaternion(sinh_q);
            LISP cosh_quat = QUATERNIONP(cosh_q) ? cosh_q : to_quaternion(cosh_q);
            return quat_divide(sinh_quat, cosh_quat);
        } else {
            /* Both simplified to real/complex */
            double complex sinh_c = to_complex(sinh_q);
            double complex cosh_c = to_complex(cosh_q);
            return from_complex(sinh_c / cosh_c);
        }
    }
    double complex z = to_complex(x);
    double complex result = ctanh(z);
    return from_complex(result);
}

static LISP lasinh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* asinh(q) = log(q + sqrt(q² + 1))
         * This one doesn't need direction adjustment - it's already correct */
        
        LISP one = make_quaternion(1, 0, 0, 0);
        
        /* q² */
        LISP q_squared = quat_multiply(x, x);
        
        /* q² + 1 */
        LISP q2_plus_one = quat_add(q_squared, one);
        
        /* sqrt(q² + 1) */
        LISP sqrt_part = lsqrt_baroque(q2_plus_one);
        
        /* q + sqrt(q² + 1) */
        LISP sum = QUATERNIONP(sqrt_part) ? quat_add(x, sqrt_part) : quat_add(x, to_quaternion(sqrt_part));
        
        /* log(...) */
        return llog_baroque(sum);
    }
    double complex z = to_complex(x);
    double complex result = casinh(z);
    return from_complex(result);
}

static LISP lacosh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* acosh(q) = log(q + sqrt(q² - 1)) */
        
        LISP one = make_quaternion(1, 0, 0, 0);
        
        /* q² */
        LISP q_squared = quat_multiply(x, x);
        
        /* q² - 1 */
        LISP q2_minus_one = quat_subtract(q_squared, one);
        
        /* sqrt(q² - 1) */
        LISP sqrt_part = lsqrt_baroque(q2_minus_one);
        
        /* q + sqrt(q² - 1) */
        LISP sum = QUATERNIONP(sqrt_part) ? quat_add(x, sqrt_part) : quat_add(x, to_quaternion(sqrt_part));
        
        /* log(...) */
        return llog_baroque(sum);
    }
    double complex z = to_complex(x);
    double complex result = cacosh(z);
    return from_complex(result);
}

static LISP latanh_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* atanh(q) = (1/2) * log((1+q)/(1-q)) */
        
        LISP one = make_quaternion(1, 0, 0, 0);
        
        /* 1 + q */
        LISP one_plus_q = quat_add(one, x);
        
        /* 1 - q */
        LISP one_minus_q = quat_subtract(one, x);
        
        /* (1+q)/(1-q) */
        LISP ratio = quat_divide(one_plus_q, one_minus_q);
        
        /* log(...) */
        LISP log_result = llog_baroque(ratio);
        
        /* (1/2) * log(...) */
        LISP result = QUATERNIONP(log_result) ? quat_scale(log_result, 0.5) : flocons(0.5 * get_c_double(log_result));
        
        return result;
    }
    double complex z = to_complex(x);
    double complex result = catanh(z);
    return from_complex(result);
}

static LISP lproj_baroque(LISP x) {
    if (QUATERNIONP(x)) {
        /* Projection onto Riemann sphere
         * Returns q if finite, or normalized infinity if infinite */
        double w = QUATW(x);
        double qx = QUATX(x);
        double qy = QUATY(x);
        double qz = QUATZ(x);
        
        /* Check if any component is infinite */
        if (isinf(w) || isinf(qx) || isinf(qy) || isinf(qz)) {
            /* Return normalized infinity: (inf, 0, 0, 0) */
            return make_quaternion(INFINITY, 0.0, 0.0, 0.0);
        }
        
        /* Finite quaternion, return as-is */
        return x;
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
   OCTONIONS - ARITHMETIC
   ============================================ */

/* Add two octonions */
static LISP oct_add_lisp(LISP o1, LISP o2) {
    if (!OCTONIONP(o1)) err("not an octonion", o1);
    if (!OCTONIONP(o2)) err("not an octonion", o2);
    
    octonion result;
    oct_add(&result, *OCTPTR(o1), *OCTPTR(o2));
    
    return make_octonion(result.e[0], result.e[1], result.e[2], result.e[3],
                        result.e[4], result.e[5], result.e[6], result.e[7]);
}

/* Subtract two octonions */
static LISP oct_sub_lisp(LISP o1, LISP o2) {
    if (!OCTONIONP(o1)) err("not an octonion", o1);
    if (!OCTONIONP(o2)) err("not an octonion", o2);
    
    octonion result;
    oct_sub(&result, *OCTPTR(o1), *OCTPTR(o2));
    
    return make_octonion(result.e[0], result.e[1], result.e[2], result.e[3],
                        result.e[4], result.e[5], result.e[6], result.e[7]);
}

/* Multiply two octonions (non-associative!) */
static LISP oct_multiply_lisp(LISP o1, LISP o2) {
    if (!OCTONIONP(o1)) err("not an octonion", o1);
    if (!OCTONIONP(o2)) err("not an octonion", o2);
    
    octonion result;
    oct_multiply(&result, *OCTPTR(o1), *OCTPTR(o2));
    
    return make_octonion(result.e[0], result.e[1], result.e[2], result.e[3],
                        result.e[4], result.e[5], result.e[6], result.e[7]);
}

/* Conjugate an octonion */
static LISP loct_conjugate(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    
    octonion result;
    oct_conjugate(&result, *OCTPTR(o));
    
    return make_octonion(result.e[0], result.e[1], result.e[2], result.e[3],
                        result.e[4], result.e[5], result.e[6], result.e[7]);
}

/* Norm (magnitude) of an octonion */
static LISP loct_norm(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    
    return flocons(oct_norm(*OCTPTR(o)));
}

/* Norm squared of an octonion */
static LISP loct_norm_squared(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    
    return flocons(oct_norm_squared(*OCTPTR(o)));
}

/* Scale octonion by scalar */
static LISP loct_scale(LISP o, LISP s) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    
    double scalar = get_c_double(s);
    octonion result;
    oct_scale(&result, *OCTPTR(o), scalar);
    
    return make_octonion(result.e[0], result.e[1], result.e[2], result.e[3],
                        result.e[4], result.e[5], result.e[6], result.e[7]);
}

/* Normalize an octonion to unit length */
static LISP loct_normalize(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    
    octonion result;
    oct_normalise(&result, *OCTPTR(o));
    
    return make_octonion(result.e[0], result.e[1], result.e[2], result.e[3],
                        result.e[4], result.e[5], result.e[6], result.e[7]);
}

/* Inverse of an octonion */
static LISP loct_inverse(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    
    octonion result;
    oct_inverse(&result, *OCTPTR(o));
    
    return make_octonion(result.e[0], result.e[1], result.e[2], result.e[3],
                        result.e[4], result.e[5], result.e[6], result.e[7]);
}

/* Negate an octonion */
static LISP loct_negate(LISP o) {
    if (!OCTONIONP(o)) err("not an octonion", o);
    
    octonion result;
    oct_negate(&result, *OCTPTR(o));
    
    return make_octonion(result.e[0], result.e[1], result.e[2], result.e[3],
                        result.e[4], result.e[5], result.e[6], result.e[7]);
}

/* ============================================
   INITIALIZATION
   ============================================ */

void init_baroque(void) {
    /* Set print hooks */
    set_print_hooks(tc_complex, complex_prin1);
    set_print_hooks(tc_quaternion, quaternion_prin1);
    set_print_hooks(tc_octonion, octonion_prin1);    
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
    init_subr_1("number?", lnumberp_baroque);  /* Polymorphic: float, complex, or quaternion */
    
    init_subr_1("quat-normalize", lquat_normalize);
    init_subr_1("quat-inverse", lquat_inverse);
    init_subr_1("quat-to-axis-angle", lquat_to_axis_angle);
    init_subr_2("quat-rotate-vector", lquat_rotate_vector);
    init_subr_3("quat-slerp", lquat_slerp);

    /* ========== OCTONIONS: Basic Functions ========== */
    init_subr_8("make-octonion", lmake_octonion);
    init_subr_8("oct", lmake_octonion);  // Shorter alias
    
    init_subr_1("oct-real", loct_real);
    init_subr_1("oct-e1", loct_e1);
    init_subr_1("oct-e2", loct_e2);
    init_subr_1("oct-e3", loct_e3);
    init_subr_1("oct-e4", loct_e4);
    init_subr_1("oct-e5", loct_e5);
    init_subr_1("oct-e6", loct_e6);
    init_subr_1("oct-e7", loct_e7);
    
    init_subr_1("octonion?", loctonionp);
    
    init_subr_2("oct-add", oct_add_lisp);
    init_subr_2("oct-sub", oct_sub_lisp);
    init_subr_2("oct-multiply", oct_multiply_lisp);
    init_subr_2("oct*", oct_multiply_lisp);  // Shorter alias
    
    init_subr_1("oct-conjugate", loct_conjugate);
    init_subr_1("oct-norm", loct_norm);
    init_subr_1("oct-norm-squared", loct_norm_squared);
    init_subr_2("oct-scale", loct_scale);
    init_subr_1("oct-normalize", loct_normalize);
    init_subr_1("oct-inverse", loct_inverse);
    init_subr_1("oct-negate", loct_negate);
    
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
