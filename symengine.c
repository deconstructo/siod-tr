/* symengine.c - SymEngine bindings for SIOD-TR
 *
 * Complete SymEngine integration in a single file.
 * 
 * Phase 1: Core functionality
 * - Expression creation from S-expressions
 * - Basic arithmetic
 * - Differentiation
 * - Expansion
 * - String conversion
 *
 * Usage: (require-so "symengine.so")
 *
 * Author: SIOD-TR Project
 * License: MIT (matching SymEngine)
 */

#include <stdio.h>
#include "siod.h"
#include <symengine/cwrapper.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

/* ================================================================
 * QUATERNION GUARDS - Prevent quaternions from entering SymEngine
 * ================================================================ */

/* Recursively check if expression contains quaternions */
static int contains_quaternion(LISP expr) {
    if (NULLP(expr)) {
        return 0;
    }
    
    /* Check if this node is a quaternion */
    if (QUATERNIONP(expr)) {
        return 1;
    }
    
    /* Recursively check cons cells */
    if (CONSP(expr)) {
        return contains_quaternion(car(expr)) || 
               contains_quaternion(cdr(expr));
    }
    
    /* Not a quaternion */
    return 0;
}

/* Check for any unsupported type (quaternions, octonions in future) */
static int contains_unsupported_type(LISP expr) {
    return contains_quaternion(expr);
    /* Future: || contains_octonion(expr) */
}

/* Display helpful error message and throw error */
static void symengine_type_error(const char *operation, LISP expr) {
    fputs("\nSymEngine Error: Expression contains quaternions.\n", stderr);
    fputs("Quaternions cannot be used in symbolic operations.\n\n", stderr);
    fputs("Use numeric evaluation instead:\n", stderr);
    fprintf(stderr, "  Instead of: (%s ...)\n", operation);
    fputs("  Use:        Direct numeric operations like (+, -, *, /)\n\n", stderr);
    fputs("Example:\n", stderr);
    fputs("  Don't:  (sym '(+ q1 q2))      ; Symbolic - won't work\n", stderr);
    fputs("  Do:     (+ q1 q2)              ; Numeric - works!\n\n", stderr);
    
    err("quaternions not supported in SymEngine", expr);
}

/* Guard wrapper - call this at the start of each SymEngine function */
static void guard_symengine_args(const char *fname, LISP expr) {
    if (contains_unsupported_type(expr)) {
        symengine_type_error(fname, expr);
    }
}

/* ================================================================
 * Memory Management Helpers
 * ================================================================ */

/* Note: basic_new_heap and basic_free_heap not needed for Phase 1 */

/* ================================================================
 * Type Checking
 * ================================================================ */

/* Check if LISP object is a number - SIOD only has floats (and complex) */
static int is_number(LISP x) {
    return TYPEP(x, tc_flonum);
    /* TODO Phase 2: Add tc_c_complex_1 support for complex numbers
     * This will enable symbolic manipulation of complex expressions
     * using SIOD's native complex number type (a+bi format)
     */
}

/* ================================================================
 * S-Expression to SymEngine Conversion
 * ================================================================ */

/* Forward declaration */
static void lisp2basic(basic_struct *result, LISP expr);

/* Convert LISP number to SymEngine basic */
static void lisp_number_to_basic(basic_struct *result, LISP n) {
    double val = FLONM(n);
    
    /* Check if it's actually an integer value for cleaner symbolic output */
    if (val == (long)val && val >= LONG_MIN && val <= LONG_MAX) {
        /* Store as integer: 2.0 -> "2" instead of "2.0" */
        integer_set_si(result, (long)val);
    } else {
        /* Store as real double */
        real_double_set_d(result, val);
    }
}

/* Convert LISP symbol to SymEngine symbol */
static void lisp_symbol_to_basic(basic_struct *result, LISP sym) {
    char *name = get_c_string(sym);
    
    /* Check for special constants */
    if (strcmp(name, "pi") == 0) {
        basic_const_pi(result);
    } else if (strcmp(name, "e") == 0) {
        basic_const_E(result);
    } else if (strcmp(name, "i") == 0) {
        basic_const_I(result);
    } else {
        symbol_set(result, name);
    }
}

/* Convert list (op args...) to SymEngine expression */
static void lisp_list_to_basic(basic_struct *result, LISP lst) {
    LISP op = car(lst);
    LISP args = cdr(lst);
    
    if (!SYMBOLP(op)) {
        err("symbolic operator must be a symbol", op);
    }
    
    char *op_name = get_c_string(op);
    
    /* Create temporary basics on stack */
    basic arg1_s, arg2_s, tmp_s;
    basic_new_stack(arg1_s);
    basic_new_stack(arg2_s);
    basic_new_stack(tmp_s);
    
    /* Binary operators */
    if (strcmp(op_name, "+") == 0) {
        if (NULLP(args) || NULLP(cdr(args))) {
            err("+ requires at least 2 arguments", lst);
        }
        lisp2basic(arg1_s, car(args));
        lisp2basic(arg2_s, cadr(args));
        basic_add(result, arg1_s, arg2_s);
        
        /* Handle more than 2 arguments */
        LISP rest = cddr(args);
        while (NNULLP(rest)) {
            lisp2basic(tmp_s, car(rest));
            basic_add(result, result, tmp_s);
            rest = cdr(rest);
        }
    }
    else if (strcmp(op_name, "-") == 0) {
        lisp2basic(arg1_s, car(args));
        if (NULLP(cdr(args))) {
            /* Unary minus */
            basic_neg(result, arg1_s);
        } else {
            lisp2basic(arg2_s, cadr(args));
            basic_sub(result, arg1_s, arg2_s);
        }
    }
    else if (strcmp(op_name, "*") == 0) {
        lisp2basic(arg1_s, car(args));
        lisp2basic(arg2_s, cadr(args));
        basic_mul(result, arg1_s, arg2_s);
    }
    else if (strcmp(op_name, "/") == 0) {
        lisp2basic(arg1_s, car(args));
        lisp2basic(arg2_s, cadr(args));
        basic_div(result, arg1_s, arg2_s);
    }
    else if (strcmp(op_name, "^") == 0 || strcmp(op_name, "pow") == 0) {
        lisp2basic(arg1_s, car(args));
        lisp2basic(arg2_s, cadr(args));
        basic_pow(result, arg1_s, arg2_s);
    }
    /* Unary functions */
    else if (strcmp(op_name, "sin") == 0) {
        lisp2basic(arg1_s, car(args));
        basic_sin(result, arg1_s);
    }
    else if (strcmp(op_name, "cos") == 0) {
        lisp2basic(arg1_s, car(args));
        basic_cos(result, arg1_s);
    }
    else if (strcmp(op_name, "tan") == 0) {
        lisp2basic(arg1_s, car(args));
        basic_tan(result, arg1_s);
    }
    else if (strcmp(op_name, "exp") == 0) {
        lisp2basic(arg1_s, car(args));
        basic_exp(result, arg1_s);
    }
    else if (strcmp(op_name, "log") == 0) {
        lisp2basic(arg1_s, car(args));
        basic_log(result, arg1_s);
    }
    else if (strcmp(op_name, "sqrt") == 0) {
        lisp2basic(arg1_s, car(args));
        basic_sqrt(result, arg1_s);
    }
    else {
        err("unknown symbolic operator", op);
    }
    
    /* Stack-allocated basics are automatically freed */
    basic_free_stack(arg1_s);
    basic_free_stack(arg2_s);
    basic_free_stack(tmp_s);
}

/* Main conversion function: LISP -> SymEngine basic */
static void lisp2basic(basic_struct *result, LISP expr) {
    if (is_number(expr)) {
        lisp_number_to_basic(result, expr);
    }
    else if (SYMBOLP(expr)) {
        lisp_symbol_to_basic(result, expr);
    }
    else if (CONSP(expr)) {
        lisp_list_to_basic(result, expr);
    }
    else {
        err("cannot convert to symbolic expression", expr);
    }
}

/* ================================================================
 * SymEngine to S-Expression Conversion
 * ================================================================ */

/* Convert SymEngine basic to LISP string (for now) */
static LISP basic2lisp_string(basic_struct *b) {
    char *str = basic_str(b);
    LISP result = strcons(strlen(str), str);
    basic_str_free(str);
    return result;
}

/* ================================================================
 * SIOD Primitive Functions
 * ================================================================ */

/* (sym expr) - Create symbolic expression from S-expression */
LISP siod_sym(LISP expr) {
    guard_symengine_args("sym", expr);
    
    basic b;
    basic_new_stack(b);
    lisp2basic(b, expr);
    LISP result = basic2lisp_string(b);
    basic_free_stack(b);
    return result;
}

/* (sym-diff expr var) - Differentiate expression with respect to variable */
LISP siod_sym_diff(LISP expr, LISP var) {
    guard_symengine_args("sym-diff", expr);
    guard_symengine_args("sym-diff", var);
    
    if (!SYMBOLP(var)) {
        err("differentiation variable must be a symbol", var);
    }
    
    basic e, v, result;
    basic_new_stack(e);
    basic_new_stack(v);
    basic_new_stack(result);
    
    lisp2basic(e, expr);
    lisp2basic(v, var);
    basic_diff(result, e, v);
    
    LISP lisp_result = basic2lisp_string(result);
    
    basic_free_stack(e);
    basic_free_stack(v);
    basic_free_stack(result);
    
    return lisp_result;
}

/* (sym+ expr1 expr2) - Add two symbolic expressions */
LISP siod_sym_add(LISP expr1, LISP expr2) {
    guard_symengine_args("sym+", expr1);
    guard_symengine_args("sym+", expr2);
    
    basic e1, e2, result;
    basic_new_stack(e1);
    basic_new_stack(e2);
    basic_new_stack(result);
    
    lisp2basic(e1, expr1);
    lisp2basic(e2, expr2);
    basic_add(result, e1, e2);
    
    LISP lisp_result = basic2lisp_string(result);
    
    basic_free_stack(e1);
    basic_free_stack(e2);
    basic_free_stack(result);
    
    return lisp_result;
}

/* (sym- expr1 expr2) - Subtract symbolic expressions */
LISP siod_sym_sub(LISP expr1, LISP expr2) {
    guard_symengine_args("sym-", expr1);
    guard_symengine_args("sym-", expr2);
    
    basic e1, e2, result;
    basic_new_stack(e1);
    basic_new_stack(e2);
    basic_new_stack(result);
    
    lisp2basic(e1, expr1);
    lisp2basic(e2, expr2);
    basic_sub(result, e1, e2);
    
    LISP lisp_result = basic2lisp_string(result);
    
    basic_free_stack(e1);
    basic_free_stack(e2);
    basic_free_stack(result);
    
    return lisp_result;
}

/* (sym* expr1 expr2) - Multiply symbolic expressions */
LISP siod_sym_mul(LISP expr1, LISP expr2) {
    guard_symengine_args("sym*", expr1);
    guard_symengine_args("sym*", expr2);
    
    basic e1, e2, result;
    basic_new_stack(e1);
    basic_new_stack(e2);
    basic_new_stack(result);
    
    lisp2basic(e1, expr1);
    lisp2basic(e2, expr2);
    basic_mul(result, e1, e2);
    
    LISP lisp_result = basic2lisp_string(result);
    
    basic_free_stack(e1);
    basic_free_stack(e2);
    basic_free_stack(result);
    
    return lisp_result;
}

/* (sym/ expr1 expr2) - Divide symbolic expressions */
LISP siod_sym_div(LISP expr1, LISP expr2) {
    guard_symengine_args("sym/", expr1);
    guard_symengine_args("sym/", expr2);
    
    basic e1, e2, result;
    basic_new_stack(e1);
    basic_new_stack(e2);
    basic_new_stack(result);
    
    lisp2basic(e1, expr1);
    lisp2basic(e2, expr2);
    basic_div(result, e1, e2);
    
    LISP lisp_result = basic2lisp_string(result);
    
    basic_free_stack(e1);
    basic_free_stack(e2);
    basic_free_stack(result);
    
    return lisp_result;
}

/* (sym-pow expr power) - Raise expression to power */
LISP siod_sym_pow(LISP expr, LISP power) {
    guard_symengine_args("sym-pow", expr);
    guard_symengine_args("sym-pow", power);
    
    basic e, p, result;
    basic_new_stack(e);
    basic_new_stack(p);
    basic_new_stack(result);
    
    lisp2basic(e, expr);
    lisp2basic(p, power);
    basic_pow(result, e, p);
    
    LISP lisp_result = basic2lisp_string(result);
    
    basic_free_stack(e);
    basic_free_stack(p);
    basic_free_stack(result);
    
    return lisp_result;
}

/* (sym->string expr) - Convert symbolic expression to string */
LISP siod_sym_to_string(LISP expr) {
    guard_symengine_args("sym->string", expr);
    
    basic b;
    basic_new_stack(b);
    lisp2basic(b, expr);
    char *str = basic_str(b);
    LISP result = strcons(strlen(str), str);
    basic_str_free(str);
    basic_free_stack(b);
    return result;
}

/* (sym-expand expr) - Expand symbolic expression */
LISP siod_sym_expand(LISP expr) {
    guard_symengine_args("sym-expand", expr);
    
    basic e, result;
    basic_new_stack(e);
    basic_new_stack(result);
    
    lisp2basic(e, expr);
    basic_expand(result, e);
    
    LISP lisp_result = basic2lisp_string(result);
    
    basic_free_stack(e);
    basic_free_stack(result);
    
    return lisp_result;
}

/* ================================================================
 * Initialization - Register all primitives
 * ================================================================ */

void init_subr_symengine(void) {
    /* Core expression creation */
    init_subr_1("sym", siod_sym);
    init_subr_1("sym->string", siod_sym_to_string);
    
    /* Arithmetic operations */
    init_subr_2("sym+", siod_sym_add);
    init_subr_2("sym-", siod_sym_sub);
    init_subr_2("sym*", siod_sym_mul);
    init_subr_2("sym/", siod_sym_div);
    init_subr_2("sym-pow", siod_sym_pow);
    
    /* Calculus */
    init_subr_2("sym-diff", siod_sym_diff);
    
    /* Algebraic manipulation */
    init_subr_1("sym-expand", siod_sym_expand);
}

/* ================================================================
 * Module Entry Point
 * ================================================================ */

/* Called by SIOD when loading symengine.so */
void init_symengine(void) {
    init_subr_symengine();
}

