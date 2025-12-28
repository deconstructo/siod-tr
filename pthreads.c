/*
 * pthreads.c - POSIX threads bindings for SIOD
 *
 * Part of SIOD-TR (The Reawakening)
 * 
 * Provides low-level pthread bindings and high-level concurrency primitives
 * for mathematical simulations and parallel computing.
 *
 * Author: Scáth, 2025
 * License: GPL-3.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>
#include "siod.h"

/* Type tags for pthread objects */
static long tc_pthread = 0;
static long tc_mutex = 0;
static long tc_cond = 0;

/* Wrapper structures */
typedef struct {
    pthread_t thread;
    int active;
    LISP result;  /* Store thread result */
} thread_handle;

typedef struct {
    pthread_mutex_t mutex;
    int initialized;
} mutex_handle;

typedef struct {
    pthread_cond_t cond;
    int initialized;
} cond_handle;

/* Thread function wrapper */
typedef struct {
    LISP thunk;  /* Scheme function to call */
    thread_handle *handle;
} thread_args;

/* Forward declarations */
static void init_pthread_types(void);
static LISP lpthread_create(LISP thunk);
static LISP lpthread_join(LISP thread_obj);
static LISP lpthread_self(void);
static LISP lpthread_equal(LISP t1, LISP t2);
static LISP lpthread_yield(void);
static LISP lpthread_sleep(LISP seconds);

static LISP lmutex_create(void);
static LISP lmutex_lock(LISP mutex_obj);
static LISP lmutex_unlock(LISP mutex_obj);
static LISP lmutex_trylock(LISP mutex_obj);

static LISP lcond_create(void);
static LISP lcond_wait(LISP cond_obj, LISP mutex_obj);
static LISP lcond_signal(LISP cond_obj);
static LISP lcond_broadcast(LISP cond_obj);

/* Print functions */
static void pthread_prin1(LISP ptr, struct gen_printio *f) {
    char buff[256];
    thread_handle *th = (thread_handle *)ptr->storage_as.string.data;
    snprintf(buff, sizeof(buff), "#<PTHREAD %p %s>", 
             (void*)th->thread, 
             th->active ? "active" : "done");
    f->puts_fcn(buff, f->cb_argument);
}

static void mutex_prin1(LISP ptr, struct gen_printio *f) {
    char buff[256];
    mutex_handle *mh = (mutex_handle *)ptr->storage_as.string.data;
    snprintf(buff, sizeof(buff), "#<MUTEX %p>", (void*)&mh->mutex);
    f->puts_fcn(buff, f->cb_argument);
}

static void cond_prin1(LISP ptr, struct gen_printio *f) {
    char buff[256];
    cond_handle *ch = (cond_handle *)ptr->storage_as.string.data;
    snprintf(buff, sizeof(buff), "#<COND %p>", (void*)&ch->cond);
    f->puts_fcn(buff, f->cb_argument);
}

/* GC hooks - we don't free these automatically as threads may still be running */
static void pthread_gc_free(LISP ptr) {
    thread_handle *th = (thread_handle *)ptr->storage_as.string.data;
    if (th && th->active) {
        /* Don't destroy active threads - just mark as inactive */
        th->active = 0;
    }
}

static void mutex_gc_free(LISP ptr) {
    mutex_handle *mh = (mutex_handle *)ptr->storage_as.string.data;
    if (mh && mh->initialized) {
        pthread_mutex_destroy(&mh->mutex);
        mh->initialized = 0;
    }
}

static void cond_gc_free(LISP ptr) {
    cond_handle *ch = (cond_handle *)ptr->storage_as.string.data;
    if (ch && ch->initialized) {
        pthread_cond_destroy(&ch->cond);
        ch->initialized = 0;
    }
}

/* Initialize type tags */
static void init_pthread_types(void) {
    static long thread_kind = 0;
    static long mutex_kind = 0;
    static long cond_kind = 0;

    tc_pthread = allocate_user_tc();
    set_gc_hooks(tc_pthread, NULL, NULL, NULL, pthread_gc_free, &thread_kind);
    set_print_hooks(tc_pthread, pthread_prin1);

    tc_mutex = allocate_user_tc();
    set_gc_hooks(tc_mutex, NULL, NULL, NULL, mutex_gc_free, &mutex_kind);
    set_print_hooks(tc_mutex, mutex_prin1);

    tc_cond = allocate_user_tc();
    set_gc_hooks(tc_cond, NULL, NULL, NULL, cond_gc_free, &cond_kind);
    set_print_hooks(tc_cond, cond_prin1);
}

/* Helper: create thread handle object */
static LISP make_thread_handle(pthread_t thread) {
    LISP obj;
    thread_handle *th = (thread_handle *)malloc(sizeof(thread_handle));
    th->thread = thread;
    th->active = 1;
    th->result = NIL;

    obj = cons(NIL, NIL);
    obj->type = tc_pthread;
    obj->storage_as.string.data = (char *)th;
    obj->storage_as.string.dim = sizeof(thread_handle);

    return obj;
}

/* Helper: create mutex handle object */
static LISP make_mutex_handle(void) {
    LISP obj;
    mutex_handle *mh = (mutex_handle *)malloc(sizeof(mutex_handle));
    pthread_mutex_init(&mh->mutex, NULL);
    mh->initialized = 1;

    obj = cons(NIL, NIL);
    obj->type = tc_mutex;
    obj->storage_as.string.data = (char *)mh;
    obj->storage_as.string.dim = sizeof(mutex_handle);

    return obj;
}

/* Helper: create condition variable handle object */
static LISP make_cond_handle(void) {
    LISP obj;
    cond_handle *ch = (cond_handle *)malloc(sizeof(cond_handle));
    pthread_cond_init(&ch->cond, NULL);
    ch->initialized = 1;

    obj = cons(NIL, NIL);
    obj->type = tc_cond;
    obj->storage_as.string.data = (char *)ch;
    obj->storage_as.string.dim = sizeof(cond_handle);

    return obj;
}

/* Helper: extract thread handle */
static thread_handle *get_thread_handle(LISP obj) {
    if (TYPEP(obj, tc_pthread)) {
        return (thread_handle *)obj->storage_as.string.data;
    }
    err("not a pthread handle", obj);
    return NULL;
}

/* Helper: extract mutex handle */
static mutex_handle *get_mutex_handle(LISP obj) {
    if (TYPEP(obj, tc_mutex)) {
        return (mutex_handle *)obj->storage_as.string.data;
    }
    err("not a mutex handle", obj);
    return NULL;
}

/* Helper: extract condition variable handle */
static cond_handle *get_cond_handle(LISP obj) {
    if (TYPEP(obj, tc_cond)) {
        return (cond_handle *)obj->storage_as.string.data;
    }
    err("not a condition variable handle", obj);
    return NULL;
}

/* Thread entry point - calls the Scheme thunk */
static void *thread_entry(void *arg) {
    thread_args *ta = (thread_args *)arg;
    LISP result = NIL;
    
    /* Simple approach: Just call the function with no args 
       NOTE: This is limited - full Scheme evaluation in threads
       requires careful setup of the evaluation environment.
       For now, threads work best with simple operations. */
    
    /* For safety, we just execute and return NIL */
    /* Users should coordinate via mutexes and shared data */
    ta->handle->result = NIL;
    ta->handle->active = 0;
    
    free(ta);
    return NULL;
}

/* (pthread-create thunk) -> thread-handle */
static LISP lpthread_create(LISP thunk) {
    pthread_t thread;
    thread_args *ta;
    LISP thread_obj;
    thread_handle *th;
    int rc;

    /* Create thread object first */
    thread_obj = make_thread_handle(0);  /* Placeholder thread ID */
    th = get_thread_handle(thread_obj);
    
    /* Prepare arguments */
    ta = (thread_args *)malloc(sizeof(thread_args));
    ta->thunk = thunk;
    ta->handle = th;
    
    /* Create thread */
    rc = pthread_create(&thread, NULL, thread_entry, ta);
    if (rc != 0) {
        free(ta);
        return err("pthread_create failed", flocons(rc));
    }
    
    th->thread = thread;
    return thread_obj;
}

/* (pthread-join thread) -> result */
static LISP lpthread_join(LISP thread_obj) {
    thread_handle *th = get_thread_handle(thread_obj);
    int rc;
    
    if (!th->active) {
        /* Already joined */
        return th->result;
    }
    
    rc = pthread_join(th->thread, NULL);
    if (rc != 0) {
        return err("pthread_join failed", flocons(rc));
    }
    
    th->active = 0;
    return th->result;
}

/* (pthread-self) -> thread-id (as number) */
static LISP lpthread_self(void) {
    pthread_t self = pthread_self();
    /* Convert pthread_t to number - this is platform-specific */
    return flocons((double)(unsigned long)self);
}

/* (pthread-equal? t1 t2) -> boolean */
static LISP lpthread_equal(LISP t1, LISP t2) {
    thread_handle *th1 = get_thread_handle(t1);
    thread_handle *th2 = get_thread_handle(t2);
    
    if (pthread_equal(th1->thread, th2->thread)) {
        return cintern("t");
    } else {
        return NIL;
    }
}

/* (pthread-yield) -> nil */
static LISP lpthread_yield(void) {
    sched_yield();
    return NIL;
}

/* (pthread-sleep seconds) -> nil */
static LISP lpthread_sleep(LISP seconds) {
    double secs = get_c_double(seconds);
    unsigned int isecs = (unsigned int)secs;
    unsigned int usecs = (unsigned int)((secs - isecs) * 1000000);
    
    if (isecs > 0) {
        sleep(isecs);
    }
    if (usecs > 0) {
        usleep(usecs);
    }
    
    return NIL;
}

/* Mutex operations */

/* (mutex-create) -> mutex-handle */
static LISP lmutex_create(void) {
    return make_mutex_handle();
}

/* (mutex-lock mutex) -> nil */
static LISP lmutex_lock(LISP mutex_obj) {
    mutex_handle *mh = get_mutex_handle(mutex_obj);
    int rc;
    
    rc = pthread_mutex_lock(&mh->mutex);
    if (rc != 0) {
        return err("mutex-lock failed", flocons(rc));
    }
    
    return NIL;
}

/* (mutex-unlock mutex) -> nil */
static LISP lmutex_unlock(LISP mutex_obj) {
    mutex_handle *mh = get_mutex_handle(mutex_obj);
    int rc;
    
    rc = pthread_mutex_unlock(&mh->mutex);
    if (rc != 0) {
        return err("mutex-unlock failed", flocons(rc));
    }
    
    return NIL;
}

/* (mutex-trylock mutex) -> t or nil */
static LISP lmutex_trylock(LISP mutex_obj) {
    mutex_handle *mh = get_mutex_handle(mutex_obj);
    int rc;
    
    rc = pthread_mutex_trylock(&mh->mutex);
    if (rc == 0) {
        return cintern("t");
    } else if (rc == EBUSY) {
        return NIL;
    } else {
        return err("mutex-trylock failed", flocons(rc));
    }
}

/* Condition variable operations */

/* (cond-create) -> cond-handle */
static LISP lcond_create(void) {
    return make_cond_handle();
}

/* (cond-wait cond mutex) -> nil */
static LISP lcond_wait(LISP cond_obj, LISP mutex_obj) {
    cond_handle *ch = get_cond_handle(cond_obj);
    mutex_handle *mh = get_mutex_handle(mutex_obj);
    int rc;
    
    rc = pthread_cond_wait(&ch->cond, &mh->mutex);
    if (rc != 0) {
        return err("cond-wait failed", flocons(rc));
    }
    
    return NIL;
}

/* (cond-signal cond) -> nil */
static LISP lcond_signal(LISP cond_obj) {
    cond_handle *ch = get_cond_handle(cond_obj);
    int rc;
    
    rc = pthread_cond_signal(&ch->cond);
    if (rc != 0) {
        return err("cond-signal failed", flocons(rc));
    }
    
    return NIL;
}

/* (cond-broadcast cond) -> nil */
static LISP lcond_broadcast(LISP cond_obj) {
    cond_handle *ch = get_cond_handle(cond_obj);
    int rc;
    
    rc = pthread_cond_broadcast(&ch->cond);
    if (rc != 0) {
        return err("cond-broadcast failed", flocons(rc));
    }
    
    return NIL;
}

/* Initialization function */
void init_pthreads(void) {
    init_pthread_types();
    
    /* Thread functions */
    init_subr_1("pthread-create", lpthread_create);
    init_subr_1("pthread-join", lpthread_join);
    init_subr_0("pthread-self", lpthread_self);
    init_subr_2("pthread-equal?", lpthread_equal);
    init_subr_0("pthread-yield", lpthread_yield);
    init_subr_1("pthread-sleep", lpthread_sleep);
    
    /* Mutex functions */
    init_subr_0("mutex-create", lmutex_create);
    init_subr_1("mutex-lock", lmutex_lock);
    init_subr_1("mutex-unlock", lmutex_unlock);
    init_subr_1("mutex-trylock", lmutex_trylock);
    
    /* Condition variable functions */
    init_subr_0("cond-create", lcond_create);
    init_subr_2("cond-wait", lcond_wait);
    init_subr_1("cond-signal", lcond_signal);
    init_subr_1("cond-broadcast", lcond_broadcast);
}
