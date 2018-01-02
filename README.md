cl-dawg
========

- A map implementation based on DAWG (Directed Acyclic Word Graph)
  - Maps are serialized to sequences of bytes using double array trie format
- This takes static key set, and assigns the unique identifiers for the each keys
  - Note that the input keys must be unique and lexically ordered
  - The identifier assigned to a key is the zero-origin index of the key in the input sequence
- This library aims to provide a handy way for building maps which have tens of millions of elements in Common Lisp

Build
-----

```lisp
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :dawg)
```

Example
-------

```lisp
(dawg:build :input "/usr/share/dict/words" :output "words.dawg")
(defparameter *dawg* (dawg:load "words.dawg"))

(dawg:member? "hello" *dawg*)
T

(dawg:get-id "hello" *dawg*)
50195

(dawg:each-common-prefix (id end) ("hello" *dawg*)
  (print (list id (subseq "hello" 0 end))))
(49012 "h")
(49845 "he")
(50183 "hell")
(50195 "hello")
```

API
---

### `dawg:build (&key input output byte-order show-progress) => t`

Builds a DAWG index file from the input key set.

- `input`:
  - The pathname of a key set file or a list of keys
    - "key set file" is line delimitered plain text file (a line represents a key)
  - Restrictions:
    - The input keys must be unique and lexically ordered
    - A key cannot contain null characters
  - Type: `(or string pathname list)`
- `output`:
  - The pathname of the resulting DAWG index file
  - Type: `(or string pathname)`
- `byte-order`:
  - The endianness of the output file
  - Type: `(member :native :little :big)`
  - Default: `:native`
- `show-progress`:
  - Indicates whether or not to show the progress
  - Type: `boolean`
  - Default: `nil`

### `dawg:load (index-path &key byte-order) => dawg:dawg`

Loads the DAWG map from the specified index file.

- `index-path`:
  - The pathname of an index file that built via `dawg:build` function
  - Type: `(or string pathname file-stream)`
- `byte-order`:
  - The endianness of the input file
  - Type: `(member :native :little :big)`
  - Default: `:native`

### `dawg:member? (key dawg &key start end) => boolean`

Returns `t` if `dawg` contains the given key, otherwise `nil`.

- `key`:
  - Type: `(simple-array character)`
- `dawg`:
  - Type: `dawg:dawg`
- `start`:
  - The start position in `key`
  - Type: `positive-fixnum`
  - Default: `0`
- `end`:
  - The end position in `key`
  - Type: `positive-fixnum`
  - Default: `(length key)`

### `dawg:get-id (key dawg &key start end) => (or null fixnum)`

Returns the identifier assigned to the given key.
If the key does not exist in `dawg`, this function will return `nil`.

- `key`:
  - Type: `(simple-array character)`
- `dawg`:
  - Type: `dawg:dawg`
- `start`:
  - The start position in `key`
  - Type: `positive-fixnum`
  - Default: `0`
- `end`:
  - The end position in `key`
  - Type: `positive-fixnum`
  - Default: `(length key)`
　
### `dawg:each-common-prefix ((match-id match-end) (key dawg &key start end) &body body)`

Executes common-prefix search for the given key.

For each key in `dawg` that matches the prefix part of `key`, `match-id` and `match-end` are bound then `body` is exeucted.

By using the `return` function, it is possible to break the loop halfway.

- `match-id`:
  - The identifier of the key matched with the prefix part of the input `key`
  - Type: `positive-fixnum`
- `match-end`:
  - The end position of the matched part in the input `key` (i.e., the length of the matched key)
  - Type: `positive-fixnum`
- `key`:
  - Type: `(simple-array character)`
- `dawg`:
  - Type: `dawg:dawg`
- `start`:
  - The start position in `key`
  - Type: `positive-fixnum`
  - Default: `0`
- `end`:
  - The end position in `key`
  - Type: `positive-fixnum`
  - Default: `(length key)`
- `body`:
  - The expression to be executed in each iteration

### `dawg:each-predictive ((match-id) (key dawg &key start end) &body body)`

A simplified version of `dawg:each-common-prefix` that does not bind the `match-end` variable in each iteration.
