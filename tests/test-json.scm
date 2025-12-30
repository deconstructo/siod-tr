;;; test_json.scm - Test suite for SIOD JSON module
;;;
;;; Part of siod-tr (SIOD - The Reawakening)
;;; Tests by Scáth, 2025
;;;
;;; Comprehensive tests for JSON parsing, generation, and utilities

(puts "=== SIOD JSON Module Test Suite ===\n")
(puts "Testing JSON functionality...\n\n")

;;; Test counter
(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Helper to print numbers
(define (putn n)
  (puts (number->string n)))

;;; Test helper
(define (test name condition)
  (set! test-count (+ test-count 1))
  (puts "Test ")
  (putn test-count)
  (puts ": ")
  (puts name)
  (puts " ... ")
  (if condition
      (begin
        (set! pass-count (+ pass-count 1))
        (puts "PASS\n"))
      (begin
        (set! fail-count (+ fail-count 1))
        (puts "FAIL\n"))))

(puts "--- Basic Parsing Tests ---\n")

;;; Test 1: Parse JSON null
(test "Parse JSON null"
      (null? (json-parse "null")))

;;; Test 2: Parse JSON true
(test "Parse JSON true"
      (eq? (json-parse "true") 't))

;;; Test 3: Parse JSON false
(test "Parse JSON false"
      (null? (json-parse "false")))

;;; Test 4: Parse JSON number (integer)
(define num1 (json-parse "42"))
(test "Parse JSON integer"
      (and (number? num1) (= num1 42)))

;;; Test 5: Parse JSON number (float)
(define num2 (json-parse "3.14159"))
(test "Parse JSON float"
      (and (number? num2) (> num2 3.14) (< num2 3.15)))

;;; Test 6: Parse JSON string
(define str1 (json-parse "\"hello world\""))
(test "Parse JSON string"
      (and (string? str1) (equal? str1 "hello world")))

;;; Test 7: Parse empty JSON array
(define arr1 (json-parse "[]"))
(test "Parse empty JSON array"
      (null? arr1))

;;; Test 8: Parse simple JSON array
(define arr2 (json-parse "[1, 2, 3, 4, 5]"))
(test "Parse simple JSON array"
      (and (pair? arr2)
           (= (car arr2) 1)
           (= (car (cdr arr2)) 2)
           (= (car (cdr (cdr arr2))) 3)))

;;; Test 9: Parse JSON array with mixed types
(define arr3 (json-parse "[1, \"hello\", true, null]"))
(test "Parse mixed type array"
      (and (pair? arr3)
           (= (car arr3) 1)
           (equal? (car (cdr arr3)) "hello")))

;;; Test 10: Parse empty JSON object
(define obj1 (json-parse "{}"))
(test "Parse empty JSON object"
      (null? obj1))

;;; Test 11: Parse simple JSON object
(define obj2 (json-parse "{\"name\": \"Scáth\", \"level\": 42}"))
(test "Parse simple JSON object"
      (and (pair? obj2)
           (pair? (car obj2))
           (equal? (car (car obj2)) "name")))

;;; Test 12: Parse nested JSON object
(define obj3 (json-parse "{\"user\": {\"name\": \"Alice\", \"age\": 30}}"))
(test "Parse nested JSON object"
      (and (pair? obj3)
           (pair? (cdr (car obj3)))))

(puts "\n--- Generation Tests ---\n")

;;; Test 13: Generate JSON from number
(define gen1 (json-generate 42))
(test "Generate JSON number"
      (string? gen1))

;;; Test 14: Generate JSON from string
(define gen2 (json-generate "hello"))
(test "Generate JSON string"
      (string? gen2))

;;; Test 15: Generate JSON from empty list
(define gen3 (json-generate '()))
(test "Generate JSON null from empty list"
      (string? gen3))

;;; Test 16: Generate JSON array
(define gen4 (json-generate '(1 2 3 4 5)))
(test "Generate JSON array"
      (string? gen4))

;;; Test 17: Generate JSON object
(define gen5 (json-generate '((name . "Scáth") (level . 42))))
(test "Generate JSON object"
      (string? gen5))

;;; Test 18: Generate compact JSON
(define gen6 (json-generate-compact '((name . "Bob") (active . #t))))
(test "Generate compact JSON"
      (string? gen6))

;;; Test 19: Round-trip: parse then generate
(define original "{\"test\": 123}")
(define parsed (json-parse original))
(define generated (json-generate-compact parsed))
(define reparsed (json-parse generated))
(test "Round-trip JSON conversion"
      (pair? reparsed))

(puts "\n--- Utility Function Tests ---\n")

;;; Test 20: json-object-get basic
(define test-obj '((name . "Scáth") (level . 42) (active . #t)))
(test "json-object-get: basic lookup"
      (equal? (json-object-get test-obj "name") "Scáth"))

;;; Test 21: json-object-get with symbol key
(test "json-object-get: symbol key"
      (= (json-object-get test-obj 'level) 42))

;;; Test 22: json-object-get with default
(test "json-object-get: default value"
      (equal? (json-object-get test-obj "missing" "default") "default"))

;;; Test 23: json-object-get missing key returns NIL
(test "json-object-get: missing key without default"
      (null? (json-object-get test-obj "nonexistent")))

;;; Test 24: json-array-length empty
(test "json-array-length: empty array"
      (= (json-array-length '()) 0))

;;; Test 25: json-array-length non-empty
(define test-arr '(1 2 3 4 5))
(test "json-array-length: 5 elements"
      (= (json-array-length test-arr) 5))

;;; Test 26: json-array-ref first element
(test "json-array-ref: first element"
      (= (json-array-ref test-arr 0) 1))

;;; Test 27: json-array-ref last element
(test "json-array-ref: last element"
      (= (json-array-ref test-arr 4) 5))

;;; Test 28: json-array-ref middle element
(test "json-array-ref: middle element"
      (= (json-array-ref test-arr 2) 3))

(puts "\n--- File I/O Tests ---\n")

;;; Test 29: Write and read JSON file
(define file-data '((name . "Test") (value . 123) (items . (1 2 3))))
(json-write-file "/tmp/test_siod_json.json" file-data)
(define read-data (json-read-file "/tmp/test_siod_json.json"))
(test "File I/O: write and read"
      (and (pair? read-data)
           (equal? (json-object-get read-data "name") "Test")))

;;; Test 30: Write compact JSON file
(json-write-file "/tmp/test_siod_json_compact.json" file-data #t)
(define read-compact (json-read-file "/tmp/test_siod_json_compact.json"))
(test "File I/O: compact write and read"
      (and (pair? read-compact)
           (= (json-object-get read-compact "value") 123)))

(puts "\n--- Complex Structure Tests ---\n")

;;; Test 31: Deeply nested object
(define deep-obj (json-parse "{\"a\": {\"b\": {\"c\": {\"d\": 42}}}}"))
(define level1 (json-object-get deep-obj "a"))
(define level2 (json-object-get level1 "b"))
(define level3 (json-object-get level2 "c"))
(test "Deep nesting: access nested value"
      (= (json-object-get level3 "d") 42))

;;; Test 32: Array of objects
(define arr-objs (json-parse "[{\"name\": \"Alice\"}, {\"name\": \"Bob\"}, {\"name\": \"Carol\"}]"))
(test "Array of objects: length"
      (= (json-array-length arr-objs) 3))

;;; Test 33: Array of objects - access elements
(define first-user (json-array-ref arr-objs 0))
(test "Array of objects: access element"
      (equal? (json-object-get first-user "name") "Alice"))

;;; Test 34: Object with array property
(define obj-arr (json-parse "{\"scores\": [98, 87, 92, 100]}"))
(define scores (json-object-get obj-arr "scores"))
(test "Object with array: get array"
      (= (json-array-length scores) 4))

;;; Test 35: Complex mixed structure
(define complex-json "{
  \"user\": {
    \"name\": \"Scáth\",
    \"roles\": [\"chaos-goblin\", \"modernizer\"],
    \"metadata\": {
      \"level\": 9001,
      \"active\": true
    }
  }
}")
(define complex (json-parse complex-json))
(define user-data (json-object-get complex "user"))
(define roles (json-object-get user-data "roles"))
(test "Complex structure: navigate nested data"
      (equal? (json-array-ref roles 0) "chaos-goblin"))

(puts "\n--- Edge Cases ---\n")

;;; Test 36: Unicode in strings
(define unicode-str (json-parse "\"Scáth ó Looney\""))
(test "Unicode in strings"
      (string? unicode-str))

(puts "\n--- Practical Example Tests ---\n")

;;; Test 41: Configuration file pattern
(define config '((database . ((host . "localhost") (port . 5432)))
                 (debug . #t)
                 (log-level . "info")))
(json-write-file "/tmp/test_config.json" config)
(define loaded-config (json-read-file "/tmp/test_config.json"))
(define db-config (json-object-get loaded-config "database"))
(test "Config pattern: nested config"
      (equal? (json-object-get db-config "host") "localhost"))

;;; Test 42: User data pattern
(define users '(((name . "Alice") (email . "alice@example.com") (age . 30))
                ((name . "Bob") (email . "bob@example.com") (age . 25))))
(json-write-file "/tmp/test_users.json" users)
(define loaded-users (json-read-file "/tmp/test_users.json"))
(test "User data pattern: array of users"
      (= (json-array-length loaded-users) 2))

;;; Test 43: Iterate through array
(define sum 0)
(define nums '(10 20 30 40 50))
(define i 0)
(define len (json-array-length nums))
(while (< i len)
  (begin
    (set! sum (+ sum (json-array-ref nums i)))
    (set! i (+ i 1))))
(test "Array iteration: sum elements"
      (= sum 150))

;;; Test 44: Build object programmatically
(define (make-entity type name)
  (list (cons 'type type)
        (cons 'name name)
        (cons 'created (realtime))))

(define entity (make-entity "chaos-goblin" "Scáth"))
(define entity-json (json-generate-compact entity))
(define parsed-entity (json-parse entity-json))
(test "Build object: programmatic creation"
      (equal? (json-object-get parsed-entity "type") "chaos-goblin"))

;;; Test 45: Merge objects (simple approach)
(define obj-a '((a . 1) (b . 2)))
(define obj-b '((c . 3) (d . 4)))
(define merged (append obj-a obj-b))
(test "Object merge: append lists"
      (= (json-array-length merged) 4))

(puts "\n=== Test Summary ===\n")
(puts "Total tests: ")
(putn test-count)
(puts "\n")
(puts "Passed: ")
(putn pass-count)
(puts "\n")
(puts "Failed: ")
(putn fail-count)
(puts "\n")

(if (= fail-count 0)
    (puts "\n✓ All tests passed! The JSON module is working grand.\n")
    (begin
      (puts "\n✗ Some tests failed. Check the output above.\n")
      (puts "Failures: ")
      (putn fail-count)
      (puts "\n")))

(puts "\n=== JSON Module Test Complete ===\n")
