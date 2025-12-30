# SIOD JSON Module Documentation

**Part of siod-tr (SIOD - The Reawakening)**  
**Modernization by Scáth, 2025**

## Overview

The JSON module provides comprehensive JSON parsing and generation capabilities for SIOD, bringing modern data interchange to this vintage Scheme interpreter. Built on the lightweight cJSON library (MIT licensed), this module enables seamless conversion between JSON and SIOD's native data structures.

### Why JSON for SIOD?

JSON emerged in the early 2000s, well after SIOD's 1990s origins. Adding JSON support modernizes SIOD for:
- **Data interchange** with modern web services and APIs
- **Configuration files** in a widely-supported format
- **Structured data storage** complementing SQLite3
- **Integration** with contemporary software ecosystems

## Installation

### Prerequisites

- cJSON library (single-file, MIT licensed)
- SIOD base system
- Standard C compiler (gcc/clang)

### Building

```bash
# If cJSON isn't already available, download it
wget https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.c
wget https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.h

# Compile the JSON module
gcc -c siod_json.c -o siod_json.o

# Link with SIOD
gcc -o siod siod.o siod_json.o cJSON.o [other_objects...] -lm
```

### Initialization

Add to your SIOD initialization code (typically in `slib.c` or `siod.c`):

```c
#include "siod_json.h"

/* In your init function */
init_json_module();
```

## Type Mapping

JSON and SIOD types are mapped bidirectionally:

| JSON Type | → | SIOD Type | Example |
|-----------|---|-----------|---------|
| object    | → | association list | `((key . value) ...)` |
| array     | → | list | `(item1 item2 ...)` |
| string    | → | string | `"hello"` |
| number    | → | flonum | `42.0` or `3.14159` |
| true      | → | `#t` | `#t` |
| false     | → | `#f` | `()` (NIL) |
| null      | → | `NIL` | `()` |

### Important Notes

- **SIOD predominantly uses flonums** for all numeric values, so JSON numbers become SIOD flonums
- **JSON objects** become association lists where each property is a `(key . value)` pair
- **Symbols** are treated as strings when generating JSON
- **Unknown types** default to JSON null

## Core Functions

### json-parse

Parse a JSON string into SIOD data structures.

```scheme
(json-parse json-string) => lisp-object
```

**Parameters:**
- `json-string`: String containing valid JSON

**Returns:**
- SIOD data structure representing the JSON
- Signals error on invalid JSON

**Examples:**

```scheme
; Parse JSON object
(define obj (json-parse "{\"name\": \"Scáth\", \"level\": 42}"))
; => ((name . "Scáth") (level . 42.0))

; Parse JSON array
(define arr (json-parse "[1, 2, 3, 4, 5]"))
; => (1.0 2.0 3.0 4.0 5.0)

; Parse nested structures
(define data (json-parse "{\"user\": {\"name\": \"Alice\", \"scores\": [98, 87, 92]}}"))
; => ((user . ((name . "Alice") (scores . (98.0 87.0 92.0)))))

; Parse JSON primitives
(json-parse "true")     ; => #t
(json-parse "false")    ; => ()
(json-parse "null")     ; => ()
(json-parse "\"hello\"") ; => "hello"
(json-parse "3.14159")  ; => 3.14159
```

**Error Handling:**

```scheme
; Invalid JSON triggers error
(json-parse "{invalid}")
; ERROR: json-parse: invalid JSON
```

### json-generate

Generate formatted JSON string from SIOD data structure.

```scheme
(json-generate lisp-object) => json-string
```

**Parameters:**
- `lisp-object`: SIOD data structure to convert

**Returns:**
- String containing pretty-printed JSON with indentation

**Examples:**

```scheme
; Generate JSON object from association list
(json-generate '((name . "Scáth") (chaos . 9001)))
; => "{
;       \"name\": \"Scáth\",
;       \"chaos\": 9001
;     }"

; Generate JSON array
(json-generate '(1 2 3 4 5))
; => "[1, 2, 3, 4, 5]"

; Generate nested structures
(json-generate '((user . ((name . "Bob") (active . #t)))))
; => "{
;       \"user\": {
;         \"name\": \"Bob\",
;         \"active\": true
;       }
;     }"

; Symbols become strings
(json-generate '((type . goblin) (class . chaos)))
; => "{
;       \"type\": \"goblin\",
;       \"class\": \"chaos\"
;     }"
```

### json-generate-compact

Generate compact JSON string (no whitespace) from SIOD data structure.

```scheme
(json-generate-compact lisp-object) => json-string
```

**Parameters:**
- `lisp-object`: SIOD data structure to convert

**Returns:**
- String containing minified JSON without formatting

**Examples:**

```scheme
(json-generate-compact '((name . "Scáth") (level . 42)))
; => "{\"name\":\"Scáth\",\"level\":42}"

; Useful for network transmission or storage optimization
(define compact (json-generate-compact my-data))
(string-length compact)  ; Smaller than formatted version
```

## File I/O Functions

### json-read-file

Read and parse JSON from a file.

```scheme
(json-read-file filename) => lisp-object
```

**Parameters:**
- `filename`: String path to JSON file

**Returns:**
- Parsed SIOD data structure
- Signals error if file cannot be opened or JSON is invalid

**Examples:**

```scheme
; Read configuration file
(define config (json-read-file "config.json"))
(json-object-get config "database")
; => ((host . "localhost") (port . 5432.0))

; Read data file
(define users (json-read-file "users.json"))
; Process the data...
```

### json-write-file

Write SIOD data structure to file as JSON.

```scheme
(json-write-file filename lisp-object [compact?]) => #t or NIL
```

**Parameters:**
- `filename`: String path for output file
- `lisp-object`: SIOD data structure to write
- `compact?`: Optional; if provided (non-NIL), writes compact JSON

**Returns:**
- `#t` on success
- `NIL` on failure

**Examples:**

```scheme
; Write formatted JSON
(define data '((name . "Scáth") (projects . ("siod-tr" "chaos-engine"))))
(json-write-file "output.json" data)
; => #t
; File contains pretty-printed JSON

; Write compact JSON
(json-write-file "compact.json" data #t)
; => #t
; File contains minified JSON

; Error handling
(if (json-write-file "/invalid/path.json" data)
    (display "Success!")
    (display "Failed to write"))
```

## Utility Functions

### json-object-get

Get value from JSON object (association list) by key.

```scheme
(json-object-get json-object key [default]) => value or default
```

**Parameters:**
- `json-object`: Association list representing JSON object
- `key`: String or symbol key to look up
- `default`: Optional default value if key not found

**Returns:**
- Value associated with key
- Default value if key not found (or `NIL` if no default)

**Examples:**

```scheme
(define user '((name . "Scáth") (level . 42) (active . #t)))

; Basic lookup
(json-object-get user "name")
; => "Scáth"

; Symbol keys work too
(json-object-get user 'level)
; => 42.0

; Default value
(json-object-get user "missing" "not-found")
; => "not-found"

; Nested access
(define data '((user . ((name . "Alice") (age . 30)))))
(define user-obj (json-object-get data "user"))
(json-object-get user-obj "name")
; => "Alice"
```

### json-array-length

Get length of JSON array (list).

```scheme
(json-array-length json-array) => number
```

**Parameters:**
- `json-array`: List representing JSON array

**Returns:**
- Number of elements (as flonum)

**Examples:**

```scheme
(define scores '(98 87 92 100 85))
(json-array-length scores)
; => 5.0

; Empty array
(json-array-length '())
; => 0.0

; Use for iteration
(define arr '("a" "b" "c" "d"))
(define len (json-array-length arr))
(begin
  (define i 0)
  (while (< i len)
    (begin
      (display (json-array-ref arr i))
      (newline)
      (set! i (+ i 1)))))
```

### json-array-ref

Get element from JSON array by index (0-based).

```scheme
(json-array-ref json-array index) => element
```

**Parameters:**
- `json-array`: List representing JSON array
- `index`: Numeric index (0-based, as flonum)

**Returns:**
- Element at specified index
- Signals error if index out of bounds

**Examples:**

```scheme
(define colors '("red" "green" "blue"))

; Access by index
(json-array-ref colors 0)  ; => "red"
(json-array-ref colors 1)  ; => "green"
(json-array-ref colors 2)  ; => "blue"

; Out of bounds error
(json-array-ref colors 10)
; ERROR: json-array-ref: index out of bounds

; Iterate through array
(define arr '(10 20 30 40 50))
(define i 0)
(define len (json-array-length arr))
(while (< i len)
  (begin
    (display (json-array-ref arr i))
    (newline)
    (set! i (+ i 1))))
```

## Common Patterns

### Working with Configuration Files

```scheme
; Load configuration
(define config (json-read-file "config.json"))

; Extract values
(define db-host (json-object-get config "database-host" "localhost"))
(define db-port (json-object-get config "database-port" 5432))
(define debug-mode (json-object-get config "debug" #f))

; Update and save
(define updated-config 
  (cons '(last-modified . "2025-12-28")
        config))
(json-write-file "config.json" updated-config)
```

### Processing Arrays

```scheme
; Load data
(define users (json-read-file "users.json"))

; Process each user
(define (process-user user)
  (let ((name (json-object-get user "name"))
        (email (json-object-get user "email")))
    (display name)
    (display ": ")
    (display email)
    (newline)))

; Iterate
(define i 0)
(define len (json-array-length users))
(while (< i len)
  (begin
    (process-user (json-array-ref users i))
    (set! i (+ i 1))))
```

### Building JSON Structures

```scheme
; Create user object
(define (make-user name email age)
  (list (cons 'name name)
        (cons 'email email)
        (cons 'age age)
        (cons 'created (system-time))))

; Use it
(define new-user (make-user "Scáth" "scath@chaos.ie" 3000))
(json-generate new-user)

; Build array of objects
(define users 
  (list (make-user "Alice" "alice@example.com" 30)
        (make-user "Bob" "bob@example.com" 25)
        (make-user "Carol" "carol@example.com" 35)))

(json-write-file "users.json" users)
```

### JSON and SQLite Integration

```scheme
; Store JSON in SQLite
(sql.prepare db "INSERT INTO data (json_blob) VALUES (?)")
(sql.bind-text 1 (json-generate my-data))
(sql.execute)

; Retrieve and parse
(sql.prepare db "SELECT json_blob FROM data WHERE id = ?")
(sql.bind-int 1 42)
(sql.step)
(define retrieved (json-parse (sql.column-text 0)))
```

## Performance Considerations

### Memory Management

The JSON module uses cJSON's memory management internally. Large JSON documents will temporarily allocate memory during parsing/generation. Consider:

```scheme
; For large files, process in chunks if possible
; Or work with compact JSON to reduce string overhead
(define data (json-read-file "large.json"))
; Process data
; Let SIOD's GC reclaim memory when done
```

### Compact vs. Formatted

```scheme
; Formatted JSON (human-readable, debugging)
(json-write-file "debug.json" data)

; Compact JSON (production, storage, network)
(json-write-file "prod.json" data #t)

; Compact can save 20-40% space for deeply nested structures
```

### Best Practices

1. **Validate JSON early**: Parse JSON as soon as you receive it to catch errors
2. **Use defaults**: Provide sensible defaults with `json-object-get`
3. **Handle errors**: Check return values from file operations
4. **Type consistency**: Remember SIOD converts numbers to flonums
5. **Key names**: Use string keys consistently in association lists

## Error Handling

The JSON module signals errors for:

- **Invalid JSON syntax** during parsing
- **File I/O failures** (permissions, missing files)
- **Type mismatches** (non-string filename, etc.)
- **Out of bounds** array access

```scheme
; Wrap in error handler if needed
(define (safe-json-read filename)
  (if (file-exists? filename)
      (json-read-file filename)
      '()))  ; Return empty list as fallback
```

## Integration Example

Complete example combining JSON with other SIOD modules:

```scheme
; Load configuration
(define config (json-read-file "app-config.json"))
(define db-path (json-object-get config "database" "app.db"))

; Open database
(define db (sql.open db-path))

; Create table for JSON data
(sql.exec db "CREATE TABLE IF NOT EXISTS entities (
                id INTEGER PRIMARY KEY,
                data TEXT,
                created REAL)")

; Insert JSON data
(define entity '((type . "chaos-goblin")
                 (name . "Scáth")
                 (powers . ("recursion" "modernization" "mischief"))))

(sql.exec db (string-append
  "INSERT INTO entities (data, created) VALUES ('"
  (json-generate-compact entity)
  "', "
  (number->string (system-time))
  ")"))

; Query and parse
(sql.prepare db "SELECT data FROM entities WHERE id = 1")
(sql.step)
(define retrieved (json-parse (sql.column-text 0)))
(display (json-object-get retrieved "name"))  ; => "Scáth"

; Cleanup
(sql.close db)
```

## Version History

- **2025-12-28**: Initial implementation by Scáth
  - cJSON integration
  - Core parsing/generation functions
  - File I/O support
  - Utility functions for object/array access

## License

GPL-3.0, matching SIOD's original license.

Uses cJSON library (MIT license) - Copyright (c) 2009-2017 Dave Gamble and cJSON contributors.

## See Also

- `SQLITE3.md` - SQLite3 module for data persistence
- `GD.md` - Graphics module for image generation
- Original SIOD documentation for Scheme fundamentals

---

**"Bringing JSON to the '90s, one association list at a time."** 🗲
