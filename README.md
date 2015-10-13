globber: globbing searches in clojure
=====================================

### Installation

```clojure
   [spootnik/globber "0.4.0"]
```

### Usage

Perform globbing, matching the supplied `expression` against
the list of supplied `candidates`.

- `candidates` is a collection of strings and must be seqable.
- `expression` adheres mostly to the bash notion of globbing

Globbing syntax:

- Any stray character is matched exactly
- Wildcards ('*') mean any number (including zero) of arbitrary chars
- Anyones ('?') mean a single arbitrary char
- Character classes are enclosed in square brackets and may contain
  arbitrary list of characters to match. If a character class begins
  with a ('!') or ('^') character, the class will be negated, i.e:
  will only match characters absent from the class. Empty charclasses,
  i.e: ('[]'), ('[!]'), and ('[^]') match their representation, not
  their content.
- Trees match any of their branches. Trees are delimited by curly
  brackets and content are separated by commas. Empty trees, i.e:
  ('{}'), ('{,}'), ('{,,}') match their representation, not their
  content.

### Examples

```clojure
 (glob \"foobar\" [\"foobar\"])  ;; => (\"foobar\")
 (glob \"fo[a-z]\" [\"foobar\"]) ;; => (\"foobar\")
```
