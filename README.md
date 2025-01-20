# ocaml-arrow
A native OCaml library that implements the Apache Arrow columnar memory format.
This library provides a high-performance, memory-efficient way to work with columnar data in OCaml without relying on bindings to any other language implementations of Apache Arrow.

[!NOTE]
This project is currently in an experimental stage.
To run some examples, follows the instructions below.

## Building

```shell
# Clone this respository
git clone https://github.com/koji-m/ocaml-arrow.git

# We need Flatbuffers library for OCaml
git clone https://github.com/dmitrig/flatbuffers.git

cd ocaml-arrow
opam switch create 5.2.1 .
eval $(opam env)

# Install Flatbuffers library for OCaml first 
opam install ../flatbuffers/

# Install dependencies
opam install . --deps-only --with-test

dune build
```

## Run examples

```shell
cd examples

# We need pyarrow to verify examples to work
python -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt

# Example 1. print_record_batch
# Print RecordBatch (two int32 columns) received from pyarrow to shell.
python util.py write | dune exec print_record_batch

# i32_array: [
#   1,
#   2,
#   null,
#   4,
# ]
# i32_array: [
#   1,
#   2,
#   null,
#   4,
# ]

# Example 2. write_back_record_batch
# Receive RecordBatch (two int32 columns) from pyarrow and squared all values,
# write back to stdout, print out the RecordBatch by pyarrow.
python util.py write | dune exec write_back_record_batch | python util.py read

#    col1  col2
# 0   1.0   1.0
# 1   4.0   4.0
# 2   NaN   NaN
# 3  16.0  16.0

```
