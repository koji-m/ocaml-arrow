module FbMessageRt = Arrow_ipc_gen.Message.Rt
module FbMessage = Arrow_ipc_gen.Message.Org.Apache.Arrow.Flatbuf
module FbFileRt = Arrow_ipc_gen.File.Rt
module FbFile = Arrow_ipc_gen.File.Org.Apache.Arrow.Flatbuf

type time_unit = Second | Millisecond | Microsecond | Nanosecond

module rec Datatype : sig
  type t =
    | Int32
    | Int64
    | Float64
    | Date64
    | Date32
    | Time32 of time_unit
    | Boolean
    | Utf8
    | List of Field.t
    | Struct of Field.t array

  exception NotSupported
end = struct
  type t =
    | Int32
    | Int64
    | Float64
    | Date64
    | Date32
    | Time32 of time_unit
    | Boolean
    | Utf8
    | List of Field.t
    | Struct of Field.t array

  exception NotSupported
end

and Field : sig
  type t = { type_ : Datatype.t; name : string }

  val type_ : t -> Datatype.t
  val name : t -> string

  module Message : sig
    val build_field :
      FbMessageRt.Builder.t -> Field.t -> FbMessage.Field.t FbMessageRt.wip
  end

  module File : sig
    val build_field :
      FbFileRt.Builder.t -> Field.t -> FbFile.Field.t FbFileRt.wip
  end

  exception NotSupported
end = struct
  type t = { type_ : Datatype.t; name : string }

  exception NotSupported

  let type_ f = f.type_
  let name f = f.name

  module Message = struct
    let get_int_field_type b ~signed ~bit_width =
      let children = FbMessage.Field.Vector.create b [||] in
      let int_type =
        FbMessage.Int.Builder.(
          start b |> add_is_signed signed |> add_bit_width bit_width |> finish)
      in
      (int_type, children)

    let get_f64_field_type b =
      let children = FbMessage.Field.Vector.create b [||] in
      let f64_type =
        FbMessage.FloatingPoint.Builder.(
          start b |> add_precision FbMessage.Precision.double |> finish)
      in
      (f64_type, children)

    let get_date_field_type b ~date_unit =
      let children = FbMessage.Field.Vector.create b [||] in
      let date_type =
        FbMessage.Date.Builder.(start b |> add_unit date_unit |> finish)
      in
      (date_type, children)

    let get_time_field_type b ~time_unit =
      let tm_unit, bit_width =
        match time_unit with
        | Second -> (FbMessage.TimeUnit.second, 32l)
        | Millisecond -> (FbMessage.TimeUnit.millisecond, 32l)
        | Microsecond -> (FbMessage.TimeUnit.microsecond, 64l)
        | Nanosecond -> (FbMessage.TimeUnit.nanosecond, 64l)
      in
      let children = FbMessage.Field.Vector.create b [||] in
      let time_type =
        FbMessage.Time.Builder.(
          start b |> add_unit tm_unit |> add_bit_width bit_width |> finish)
      in
      (time_type, children)

    let get_bool_field_type b =
      let children = FbMessage.Field.Vector.create b [||] in
      let bool_type = FbMessage.Bool.Builder.(start b |> finish) in
      (bool_type, children)

    let get_utf8_field_type b =
      let children = FbMessage.Field.Vector.create b [||] in
      let utf8_type = FbMessage.Utf8.Builder.(start b |> finish) in
      (utf8_type, children)

    let rec get_int32_list_field_type b child =
      let child_type = build_field b child in
      let children = FbMessage.Field.Vector.create b [| child_type |] in
      let list_type = FbMessage.List.Builder.(start b |> finish) in
      (list_type, children)

    and get_struct_field_type b children =
      let child_types = Array.map (fun f -> build_field b f) children in
      let children = FbMessage.Field.Vector.create b child_types in
      let struct_type = FbMessage.Struct_.Builder.(start b |> finish) in
      (struct_type, children)

    and build_field b field =
      let field_name = FbMessageRt.String.create b field.name in
      match field.type_ with
      | Datatype.Int32 ->
          let field_type, _children =
            get_int_field_type b ~signed:true ~bit_width:32l
          in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__int field_type |> finish)
      | Datatype.Int64 ->
          let field_type, _children =
            get_int_field_type b ~signed:true ~bit_width:64l
          in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__int field_type |> finish)
      | Datatype.Float64 ->
          let field_type, _children = get_f64_field_type b in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__floating_point field_type
            |> finish)
      | Datatype.Date32 ->
          let field_type, _children =
            get_date_field_type b ~date_unit:FbMessage.DateUnit.day
          in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__date field_type |> finish)
      | Datatype.Date64 ->
          let field_type, _children =
            get_date_field_type b ~date_unit:FbMessage.DateUnit.millisecond
          in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__date field_type |> finish)
      | Datatype.Time32 tm_unit ->
          let field_type, _children =
            get_time_field_type b ~time_unit:tm_unit
          in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__time field_type |> finish)
      | Datatype.Boolean ->
          let field_type, _children = get_bool_field_type b in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__bool field_type |> finish)
      | Datatype.Utf8 ->
          let field_type, _children = get_utf8_field_type b in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__utf8 field_type |> finish)
      | Datatype.List child ->
          let field_type, children = get_int32_list_field_type b child in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__list field_type |> add_children children |> finish)
      | Datatype.Struct children ->
          let field_type, children = get_struct_field_type b children in
          FbMessage.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__struct_ field_type
            |> add_children children |> finish)
  end

  module File = struct
    let get_int_field_type b ~signed ~bit_width =
      let children = FbFile.Field.Vector.create b [||] in
      let int_type =
        FbFile.Int.Builder.(
          start b |> add_is_signed signed |> add_bit_width bit_width |> finish)
      in
      (int_type, children)

    let get_f64_field_type b =
      let children = FbFile.Field.Vector.create b [||] in
      let f64_type =
        FbFile.FloatingPoint.Builder.(
          start b |> add_precision FbFile.Precision.double |> finish)
      in
      (f64_type, children)

    let get_date_field_type b ~date_unit =
      let children = FbFile.Field.Vector.create b [||] in
      let date_type =
        FbFile.Date.Builder.(start b |> add_unit date_unit |> finish)
      in
      (date_type, children)

    let get_time_field_type b ~time_unit =
      let tm_unit, bit_width =
        match time_unit with
        | Second -> (FbFile.TimeUnit.second, 32l)
        | Millisecond -> (FbFile.TimeUnit.millisecond, 32l)
        | Microsecond -> (FbFile.TimeUnit.microsecond, 64l)
        | Nanosecond -> (FbFile.TimeUnit.nanosecond, 64l)
      in
      let children = FbFile.Field.Vector.create b [||] in
      let time_type =
        FbFile.Time.Builder.(
          start b |> add_unit tm_unit |> add_bit_width bit_width |> finish)
      in
      (time_type, children)

    let get_bool_field_type b =
      let children = FbFile.Field.Vector.create b [||] in
      let bool_type = FbFile.Bool.Builder.(start b |> finish) in
      (bool_type, children)

    let get_utf8_field_type b =
      let children = FbFile.Field.Vector.create b [||] in
      let utf8_type = FbFile.Utf8.Builder.(start b |> finish) in
      (utf8_type, children)

    let rec get_int32_list_field_type b child =
      let child_type = build_field b child in
      let children = FbFile.Field.Vector.create b [| child_type |] in
      let list_type = FbFile.List.Builder.(start b |> finish) in
      (list_type, children)

    and get_struct_field_type b children =
      let child_types = Array.map (fun f -> build_field b f) children in
      let children = FbFile.Field.Vector.create b child_types in
      let struct_type = FbFile.Struct_.Builder.(start b |> finish) in
      (struct_type, children)

    and build_field b field =
      let field_name = FbFileRt.String.create b field.name in
      match field.type_ with
      | Datatype.Int32 ->
          let field_type, _children =
            get_int_field_type b ~signed:true ~bit_width:32l
          in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__int field_type |> finish)
      | Datatype.Int64 ->
          let field_type, _children =
            get_int_field_type b ~signed:true ~bit_width:64l
          in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__int field_type |> finish)
      | Datatype.Float64 ->
          let field_type, _children = get_f64_field_type b in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__floating_point field_type
            |> finish)
      | Datatype.Date32 ->
          let field_type, _children =
            get_date_field_type b ~date_unit:FbFile.DateUnit.day
          in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__date field_type |> finish)
      | Datatype.Date64 ->
          let field_type, _children =
            get_date_field_type b ~date_unit:FbFile.DateUnit.millisecond
          in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__date field_type |> finish)
      | Datatype.Time32 tm_unit ->
          let field_type, _children =
            get_time_field_type b ~time_unit:tm_unit
          in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__time field_type |> finish)
      | Datatype.Boolean ->
          let field_type, _children = get_bool_field_type b in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__bool field_type |> finish)
      | Datatype.Utf8 ->
          let field_type, _children = get_utf8_field_type b in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__utf8 field_type |> finish)
      | Datatype.List child ->
          let field_type, children = get_int32_list_field_type b child in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__list field_type |> add_children children |> finish)
      | Datatype.Struct children ->
          let field_type, children = get_struct_field_type b children in
          FbFile.Field.Builder.(
            start b |> add_name field_name |> add_nullable true
            |> add_type__struct_ field_type
            |> add_children children |> finish)
  end
end

type t = { fields : Field.t array }

let fields s = s.fields
