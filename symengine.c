/* symengine.c - SymEngine module wrapper for SIOD-TR
 *
 * This file provides the module entry point for the SymEngine extension.
 * The actual implementation is in siod_symengine.c
 *
 * To load: (require-so "symengine.so") or (load "symengine.so")
 */

#include <stdio.h>
#include <stddef.h>
#include "siod.h"

/* Forward declaration - implemented in siod_symengine.c */
extern void init_subr_symengine(void);

/* Module entry point - called by SIOD when loading symengine.so */
void init_symengine(void) {
    init_subr_symengine();
}
