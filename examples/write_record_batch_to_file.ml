let i32_array =
  Arrow_array.Primitive_array.Int32_array.of_array
    [| Some 1l; None; Some 3l; Some 4l |]
in
let i64_array =
  Arrow_array.Primitive_array.Int64_array.of_array
    [| Some 10L; Some 20L; None; Some 40L |]
in
let f64_array =
  Arrow_array.Primitive_array.Float64_array.of_array
    [| Some 3.14; Some 2.718; None; Some 1.618 |]
in
let str_array =
  Arrow_array.Generic_byte_array.of_string_array
    [| Some "apple"; None; Some "orange"; Some "banana" |]
in
let i32_list_array =
  Arrow_array.Generic_list_array.of_int32_array_array
    [|
      Some [| Some 10l; Some 20l; None; Some 40l |];
      Some [| Some 100l; None; Some 300l; Some 400l |];
      Some [| Some 10l; Some 20l; None; Some 40l |];
      Some [| Some 100l; None; Some 300l; Some 400l |];
    |]
in
let struct_fields =
  [|
    Arrow_schema.Schema.Field.
      { type_ = Arrow_schema.Datatype.Int32; name = "struct_num1" };
    Arrow_schema.Schema.Field.
      { type_ = Arrow_schema.Datatype.Int32; name = "struct_num2" };
  |]
in
let field_arrays =
  [|
    Arrow_array.Array_intf.Array
      ( (module Arrow_array.Primitive_array.Int32_array),
        Arrow_array.Primitive_array.Int32_array.of_array
          [| Some 10l; Some 20l; None; Some 40l |] );
    Arrow_array.Array_intf.Array
      ( (module Arrow_array.Primitive_array.Int32_array),
        Arrow_array.Primitive_array.Int32_array.of_array
          [| Some 100l; None; Some 300l; Some 400l |] );
  |]
in
let nulls = None in
let struct_array =
  Arrow_array.Struct_array.make struct_fields field_arrays nulls
in
let boolean_array =
  Arrow_array.Boolean_array.of_boolean_array
    [| Some true; Some false; None; Some true |]
in
let batch =
  Arrow_array.Record_batch.
    {
      schema =
        {
          fields =
            [|
              { type_ = Arrow_schema.Datatype.Int32; name = "i32" };
              { type_ = Arrow_schema.Datatype.Int64; name = "i64" };
              { type_ = Arrow_schema.Datatype.Float64; name = "f64" };
              { type_ = Arrow_schema.Datatype.Utf8; name = "str1" };
              {
                type_ =
                  Arrow_schema.Datatype.List
                    Arrow_schema.Field.
                      { type_ = Arrow_schema.Datatype.Int32; name = "inner" };
                name = "i32_list";
              };
              {
                type_ = Arrow_schema.Datatype.Struct struct_fields;
                name = "struct1";
              };
              { type_ = Arrow_schema.Datatype.Boolean; name = "bool1" };
            |];
        };
      columns =
        [|
          Arrow_array.Array_intf.Array
            ((module Arrow_array.Primitive_array.Int32_array), i32_array);
          Arrow_array.Array_intf.Array
            ((module Arrow_array.Primitive_array.Int64_array), i64_array);
          Arrow_array.Array_intf.Array
            ((module Arrow_array.Primitive_array.Float64_array), f64_array);
          Arrow_array.Array_intf.Array
            ((module Arrow_array.Generic_byte_array), str_array);
          Arrow_array.Array_intf.Array
            ((module Arrow_array.Generic_list_array), i32_list_array);
          Arrow_array.Array_intf.Array
            ((module Arrow_array.Struct_array), struct_array);
          Arrow_array.Array_intf.Array
            ((module Arrow_array.Boolean_array), boolean_array);
        |];
    }
in

let fd = Unix.(openfile "example.arrow" [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644) in
let fw = Arrow_ipc.Writer.(Writer.create fd |> FileWriter.create) in
Arrow_ipc.Writer.FileWriter.write fw batch
