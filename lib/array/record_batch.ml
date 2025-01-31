open Arrow_schema

type t = { schema : Schema.t; columns : Array_intf.t array }

let schema r = r.schema
let columns r = r.columns

let num_rows r =
  let col = r.columns.(0) in
  let (Array_intf.Array ((module A), arr)) = col in
  A.length arr
