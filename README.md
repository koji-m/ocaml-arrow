# ocaml-arrow
A native OCaml library that implements the Apache Arrow columnar memory format.
This library provides a high-performance, memory-efficient way to work with columnar data in OCaml without relying on bindings to any other language implementations of Apache Arrow and is based on the implementation of [arrow-rs](https://github.com/apache/arrow-rs).

> [!NOTE]
> This project is currently in an experimental stage.
> To run some examples, follows the instructions below.

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
# i64_array: [
#   10,
#   20,
#   null,
#   40,
# ]
# f64_array: [
#   3.140000,
#   null,
#   2.718000,
#   1.618000,
# ]
# date32_array: [
#   2022-01-01 00:00:00,
#   2023-04-10 00:00:00,
#   2024-08-15 00:00:00,
#   2025-02-09 00:00:00,
# ]
# date64_array: [
#   2022-01-01 00:00:00,
#   2023-04-10 00:00:00,
#   2024-08-15 00:00:00,
#   2025-02-09 00:00:00,
# ]
# ...

# Example 2. write_back_record_batch
# Receive RecordBatch (two int32 columns) from pyarrow and squared all values,
# write back to stdout, print out the RecordBatch by pyarrow.
python util.py write | dune exec write_back_record_batch | python util.py read

#    col1    col2      col3
# 0   1.0   100.0  9.859600
# 1   4.0   400.0       NaN
# 2   NaN     NaN  7.387524
# 3  16.0  1600.0  2.617924

# Example 3. write_record_batch_to_file
# Write RecordBatch (columns of int32, utf8, int32-list, struct, boolean) to file
# and print out the RecordBatch by pyarrow
dune exec write_record_batch_to_file

python util.py read_file

#     i32   i64    f64      date32  ...    str1                    i32_list                                      struct1  bool1
# 0   1.0  10.0  3.140  2025-02-06  ...    apple     [10.0, 20.0, nan, 40.0]  {'struct_num1': 10.0, 'struct_num2': 100.0}   True
# 1   NaN  20.0  2.718  2025-02-07  ...    None  [100.0, nan, 300.0, 400.0]   {'struct_num1': 20.0, 'struct_num2': None}  False
# 2   3.0   NaN    NaN        None  ...    orange     [10.0, 20.0, nan, 40.0]  {'struct_num1': None, 'struct_num2': 300.0}   None
# 3   4.0  40.0  1.618  2025-02-08  ...    banana  [100.0, nan, 300.0, 400.0]  {'struct_num1': 40.0, 'struct_num2': 400.0}   True
```
