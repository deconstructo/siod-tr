/*
 * siod_json.c - JSON parsing and generation for SIOD
 * 
 * Part of siod-tr (SIOD - The Reawakening)
 * Modernization by Scáth, 2025
 * 
 * Provides JSON parsing and generation capabilities using cJSON library.
 * Integrates modern JSON support into the vintage SIOD Scheme interpreter.
 * 
 * License: GPL-3.0 (matching SIOD's original license)
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include "siod.h"
#include "siodp.h"
#include <cJSON.h>

/* Forward declarations */
static LISP cjson_to_lisp(cJSON *json);
static cJSON *lisp_to_cjson(LISP obj);

/*
 * Convert cJSON object to LISP structure
 * 
 * Mapping:
 *   JSON object  -> association list ((key . value) ...)
 *   JSON array   -> list (item1 item2 ...)
 *   JSON string  -> string
 *   JSON number  -> flonum or fixnum
 *   JSON true    -> #t
 *   JSON false   -> #f
 *   JSON null    -> NIL
 */
static LISP cjson_to_lisp(cJSON *json) {
    if (json == NULL) {
        return NIL;
    }
    
    switch (json->type & 0xFF) {
        case cJSON_NULL:
            return NIL;
            
        case cJSON_False:
            return NIL;  /* #f in SIOD */
            
        case cJSON_True:
            return sym_t;  /* #t in SIOD */
            
        case cJSON_Number:
            /* Use flonum if there's a fractional part, else fixnum */
            if (json->valuedouble != (double)json->valueint) {
                return flocons(json->valuedouble);
            } else {
                return flocons((double)json->valueint);  /* SIOD uses flonums predominantly */
            }
            
        case cJSON_String:
            return strcons(strlen(json->valuestring), json->valuestring);
            
        case cJSON_Array: {
            LISP result = NIL;
            LISP tail = NIL;
            cJSON *item = NULL;
            
            cJSON_ArrayForEach(item, json) {
                LISP elem = cjson_to_lisp(item);
                
                if (NULLP(result)) {
                    result = cons(elem, NIL);
                    tail = result;
                } else {
                    CDR(tail) = cons(elem, NIL);
                    tail = CDR(tail);
                }
            }
            return result;
        }
            
        case cJSON_Object: {
            LISP result = NIL;
            LISP tail = NIL;
            cJSON *item = NULL;
            
            cJSON_ArrayForEach(item, json) {
                LISP key = strcons(strlen(item->string), item->string);
                LISP value = cjson_to_lisp(item);
                LISP pair = cons(key, value);
                
                if (NULLP(result)) {
                    result = cons(pair, NIL);
                    tail = result;
                } else {
                    CDR(tail) = cons(pair, NIL);
                    tail = CDR(tail);
                }
            }
            return result;
        }
            
        default:
            return NIL;
    }
}

/*
 * Convert LISP structure to cJSON object
 * 
 * Determines type by examining LISP object structure:
 *   - String -> JSON string
 *   - Number -> JSON number  
 *   - NIL -> JSON null
 *   - List of pairs (alist) -> JSON object
 *   - List -> JSON array
 */
static cJSON *lisp_to_cjson(LISP obj) {
    if (NULLP(obj)) {
        return cJSON_CreateNull();
    }
    
    if (TYPEP(obj, tc_flonum)) {
        return cJSON_CreateNumber(FLONM(obj));
    }
    
    if (TYPEP(obj, tc_string)) {
        return cJSON_CreateString(obj->storage_as.string.data);
    }
    
    if (TYPEP(obj, tc_symbol)) {
        /* Treat symbols as strings in JSON */
        return cJSON_CreateString(PNAME(obj));
    }
    
    if (CONSP(obj)) {
        /* Check if this looks like an association list (for JSON object) */
        /* An alist has pairs where CAR is a string/symbol (key) */
        LISP first = CAR(obj);
        
        if (CONSP(first) && (TYPEP(CAR(first), tc_string) || TYPEP(CAR(first), tc_symbol))) {
            /* Looks like an alist -> JSON object */
            cJSON *json_obj = cJSON_CreateObject();
            LISP item = obj;
            
            while (CONSP(item)) {
                LISP pair = CAR(item);
                if (CONSP(pair)) {
                    LISP key = CAR(pair);
                    LISP value = CDR(pair);
                    
                    char *key_str;
                    if (TYPEP(key, tc_string)) {
                        key_str = key->storage_as.string.data;
                    } else if (TYPEP(key, tc_symbol)) {
                        key_str = PNAME(key);
                    } else {
                        /* Skip non-string/symbol keys */
                        item = CDR(item);
                        continue;
                    }
                    
                    cJSON *json_value = lisp_to_cjson(value);
                    cJSON_AddItemToObject(json_obj, key_str, json_value);
                }
                item = CDR(item);
            }
            return json_obj;
        } else {
            /* Regular list -> JSON array */
            cJSON *json_array = cJSON_CreateArray();
            LISP item = obj;
            
            while (CONSP(item)) {
                cJSON *elem = lisp_to_cjson(CAR(item));
                cJSON_AddItemToArray(json_array, elem);
                item = CDR(item);
            }
            return json_array;
        }
    }
    
    /* Default: null for unknown types */
    return cJSON_CreateNull();
}

/*
 * (json-parse json-string)
 * 
 * Parse a JSON string and return SIOD data structure.
 * Returns NIL on parse error.
 */
LISP json_parse(LISP json_str) {
    char *str;
    cJSON *json;
    LISP result;
    
    if (NULLP(json_str)) {
        return err("json-parse: requires string argument", NIL);
    }
    
    if (!TYPEP(json_str, tc_string)) {
        return err("json-parse: argument must be a string", json_str);
    }
    
    str = json_str->storage_as.string.data;
    json = cJSON_Parse(str);
    
    if (json == NULL) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "JSON parse error before: %s\n", error_ptr);
        }
        return err("json-parse: invalid JSON", json_str);
    }
    
    result = cjson_to_lisp(json);
    cJSON_Delete(json);
    
    return result;
}

/*
 * (json-generate lisp-object)
 * 
 * Generate JSON string from SIOD data structure.
 * Returns string containing formatted JSON.
 */
LISP json_generate(LISP obj) {
    cJSON *json;
    char *json_str;
    LISP result;
    
    json = lisp_to_cjson(obj);
    json_str = cJSON_Print(json);
    
    if (json_str == NULL) {
        cJSON_Delete(json);
        return err("json-generate: failed to generate JSON", obj);
    }
    
    result = strcons(strlen(json_str), json_str);
    
    free(json_str);
    cJSON_Delete(json);
    
    return result;
}

/*
 * (json-generate-compact lisp-object)
 * 
 * Generate compact JSON string (no whitespace) from SIOD data structure.
 */
LISP json_generate_compact(LISP obj) {
    cJSON *json;
    char *json_str;
    LISP result;
    
    json = lisp_to_cjson(obj);
    json_str = cJSON_PrintUnformatted(json);
    
    if (json_str == NULL) {
        cJSON_Delete(json);
        return err("json-generate-compact: failed to generate JSON", obj);
    }
    
    result = strcons(strlen(json_str), json_str);
    
    free(json_str);
    cJSON_Delete(json);
    
    return result;
}

/*
 * (json-read-file filename)
 * 
 * Read and parse JSON from a file.
 * Returns parsed SIOD data structure.
 */
LISP json_read_file(LISP filename) {
    FILE *fp;
    long file_size;
    char *buffer;
    LISP result;
    
    if (!TYPEP(filename, tc_string)) {
        return err("json-read-file: filename must be a string", filename);
    }
    
    fp = fopen(filename->storage_as.string.data, "r");
    if (fp == NULL) {
        return err("json-read-file: cannot open file", filename);
    }
    
    /* Get file size */
    fseek(fp, 0, SEEK_END);
    file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    /* Read entire file */
    buffer = (char *)malloc(file_size + 1);
    if (buffer == NULL) {
        fclose(fp);
        return err("json-read-file: memory allocation failed", NIL);
    }
    
    if (fread(buffer, 1, file_size, fp) != (size_t)file_size) {
        free(buffer);
        fclose(fp);
        return err("json-read-file: failed to read file", filename);
    }
    buffer[file_size] = '\0';
    fclose(fp);
    
    /* Parse JSON */
    result = json_parse(strcons(strlen(buffer), buffer));
    free(buffer);
    
    return result;
}

/*
 * (json-write-file filename lisp-object [compact?])
 * 
 * Write SIOD data structure to file as JSON.
 * If compact? is true, writes compact JSON without whitespace.
 * Returns #t on success, NIL on failure.
 */
LISP json_write_file(LISP args) {
    FILE *fp;
    LISP json_str;
    LISP filename, obj, compact;
    
    /* Extract arguments from list */
    if (NULLP(args)) {
        return err("json-write-file: requires at least 2 arguments", NIL);
    }
    filename = CAR(args);
    args = CDR(args);
    
    if (NULLP(args)) {
        return err("json-write-file: requires at least 2 arguments", NIL);
    }
    obj = CAR(args);
    args = CDR(args);
    
    compact = NULLP(args) ? NIL : CAR(args);
    
    if (!TYPEP(filename, tc_string)) {
        return err("json-write-file: filename must be a string", filename);
    }
    
    /* Generate JSON string */
    if (NULLP(compact)) {
        json_str = json_generate(obj);
    } else {
        json_str = json_generate_compact(obj);
    }
    
    if (NULLP(json_str)) {
        return NIL;
    }
    
    /* Write to file */
    fp = fopen(filename->storage_as.string.data, "w");
    if (fp == NULL) {
        return err("json-write-file: cannot open file for writing", filename);
    }
    
    fprintf(fp, "%s", json_str->storage_as.string.data);
    fclose(fp);
    
    return sym_t;
}

/*
 * (json-object-get json-object key [default])
 * 
 * Get value from JSON object (association list) by key.
 * Returns default if key not found (or NIL if no default provided).
 */
LISP json_object_get(LISP args) {
    LISP obj, key, default_val;
    LISP item;
    char *key_str;
    
    /* Extract arguments from list */
    if (NULLP(args)) {
        return err("json-object-get: requires at least 2 arguments", NIL);
    }
    obj = CAR(args);
    args = CDR(args);
    
    if (NULLP(args)) {
        return err("json-object-get: requires at least 2 arguments", NIL);
    }
    key = CAR(args);
    args = CDR(args);
    
    default_val = NULLP(args) ? NIL : CAR(args);
    
    if (NULLP(obj)) {
        return NULLP(default_val) ? NIL : default_val;
    }
    
    if (!CONSP(obj)) {
        return err("json-object-get: first argument must be a list", obj);
    }
    
    if (TYPEP(key, tc_string)) {
        key_str = key->storage_as.string.data;
    } else if (TYPEP(key, tc_symbol)) {
        key_str = PNAME(key);
    } else {
        return err("json-object-get: key must be a string or symbol", key);
    }
    
    /* Search association list */
    item = obj;
    while (CONSP(item)) {
        LISP pair = CAR(item);
        if (CONSP(pair)) {
            LISP pair_key = CAR(pair);
            char *pair_key_str;
            
            if (TYPEP(pair_key, tc_string)) {
                pair_key_str = pair_key->storage_as.string.data;
            } else if (TYPEP(pair_key, tc_symbol)) {
                pair_key_str = PNAME(pair_key);
            } else {
                item = CDR(item);
                continue;
            }
            
            if (strcmp(key_str, pair_key_str) == 0) {
                return CDR(pair);
            }
        }
        item = CDR(item);
    }
    
    return NULLP(default_val) ? NIL : default_val;
}

/*
 * (json-array-length json-array)
 * 
 * Return length of JSON array (list).
 */
LISP json_array_length(LISP arr) {
    long count = 0;
    LISP item;
    
    if (NULLP(arr)) {
        return flocons(0.0);
    }
    
    if (!CONSP(arr)) {
        return err("json-array-length: argument must be a list", arr);
    }
    
    item = arr;
    while (CONSP(item)) {
        count++;
        item = CDR(item);
    }
    
    return flocons((double)count);
}

/*
 * (json-array-ref json-array index)
 * 
 * Get element from JSON array by index (0-based).
 */
LISP json_array_ref(LISP arr, LISP index) {
    long idx, i;
    LISP item;
    
    if (NULLP(arr)) {
        return err("json-array-ref: array is null", NIL);
    }
    
    if (!CONSP(arr)) {
        return err("json-array-ref: first argument must be a list", arr);
    }
    
    if (!TYPEP(index, tc_flonum)) {
        return err("json-array-ref: index must be a number", index);
    }
    
    idx = (long)FLONM(index);
    
    if (idx < 0) {
        return err("json-array-ref: index must be non-negative", index);
    }
    
    item = arr;
    for (i = 0; i < idx && CONSP(item); i++) {
        item = CDR(item);
    }
    
    if (!CONSP(item)) {
        return err("json-array-ref: index out of bounds", index);
    }
    
    return CAR(item);
}

/*
 * Initialize JSON module - register all functions
 */
void init_json_module(void) {
    /* Core parsing and generation */
    init_subr_1("json-parse", json_parse);
    init_subr_1("json-generate", json_generate);
    init_subr_1("json-generate-compact", json_generate_compact);
    
    /* File I/O */
    init_subr_1("json-read-file", json_read_file);
    init_lsubr("json-write-file", json_write_file);
    
    /* Utility functions */
    init_lsubr("json-object-get", json_object_get);
    init_subr_1("json-array-length", json_array_length);
    init_subr_2("json-array-ref", json_array_ref);
}
