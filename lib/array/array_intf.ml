open Arrow_buffer
open Arrow_data
open Arrow_schema

module type Array = sig
  type t

  val data_type : t -> Datatype.t
  val length : t -> int
  val values : t -> Buffer.t
  val nulls : t -> Null_buffer.t option
  val to_data : t -> Array_data.t
end

type t = Array : (module Array with type t = 't) * 't -> t

let to_data a =
  let (Array ((module A), a)) = a in
  A.to_data a
