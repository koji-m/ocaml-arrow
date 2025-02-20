(** Automatically generated by the FlatBuffers compiler

    root type: org.apache.arrow.flatbuf.Footer (//File.fbs) flatc version:
    23.3.3 *)

[@@@warning "-32"]

module Rt = Flatbuffers.Runtime

module Struct = struct
  let set_block__0 b i (offset_, meta_data_length_, body_length_) =
    Rt.Builder.set_scalar TLong b (i + 0) offset_;
    Rt.Builder.set_scalar TInt b (i + 8) meta_data_length_;
    Rt.Builder.set_padding b (i + 12) 4;
    Rt.Builder.set_scalar TLong b (i + 16) body_length_

  and set_buffer__1 b i (offset_, length_) =
    Rt.Builder.set_scalar TLong b (i + 0) offset_;
    Rt.Builder.set_scalar TLong b (i + 8) length_
end

module Union_ = struct
  let read_table_type___2 ?none ?null ?int ?floating_point ?binary ?utf8 ?bool
      ?decimal ?date ?time ?timestamp ?interval ?list ?struct_ ?union
      ?fixed_size_binary ?fixed_size_list ?map ?duration ?large_binary
      ?large_utf8 ?large_list ?run_end_encoded ?binary_view ?utf8_view
      ?list_view ?large_list_view ~default b i t o =
    match Rt.UType.to_default t with
    | 0L when Option.is_some none -> Option.get none
    | 1L when Option.is_some null -> Option.get null (Rt.Ref.read_table b o i)
    | 2L when Option.is_some int -> Option.get int (Rt.Ref.read_table b o i)
    | 3L when Option.is_some floating_point ->
        Option.get floating_point (Rt.Ref.read_table b o i)
    | 4L when Option.is_some binary ->
        Option.get binary (Rt.Ref.read_table b o i)
    | 5L when Option.is_some utf8 -> Option.get utf8 (Rt.Ref.read_table b o i)
    | 6L when Option.is_some bool -> Option.get bool (Rt.Ref.read_table b o i)
    | 7L when Option.is_some decimal ->
        Option.get decimal (Rt.Ref.read_table b o i)
    | 8L when Option.is_some date -> Option.get date (Rt.Ref.read_table b o i)
    | 9L when Option.is_some time -> Option.get time (Rt.Ref.read_table b o i)
    | 10L when Option.is_some timestamp ->
        Option.get timestamp (Rt.Ref.read_table b o i)
    | 11L when Option.is_some interval ->
        Option.get interval (Rt.Ref.read_table b o i)
    | 12L when Option.is_some list -> Option.get list (Rt.Ref.read_table b o i)
    | 13L when Option.is_some struct_ ->
        Option.get struct_ (Rt.Ref.read_table b o i)
    | 14L when Option.is_some union ->
        Option.get union (Rt.Ref.read_table b o i)
    | 15L when Option.is_some fixed_size_binary ->
        Option.get fixed_size_binary (Rt.Ref.read_table b o i)
    | 16L when Option.is_some fixed_size_list ->
        Option.get fixed_size_list (Rt.Ref.read_table b o i)
    | 17L when Option.is_some map -> Option.get map (Rt.Ref.read_table b o i)
    | 18L when Option.is_some duration ->
        Option.get duration (Rt.Ref.read_table b o i)
    | 19L when Option.is_some large_binary ->
        Option.get large_binary (Rt.Ref.read_table b o i)
    | 20L when Option.is_some large_utf8 ->
        Option.get large_utf8 (Rt.Ref.read_table b o i)
    | 21L when Option.is_some large_list ->
        Option.get large_list (Rt.Ref.read_table b o i)
    | 22L when Option.is_some run_end_encoded ->
        Option.get run_end_encoded (Rt.Ref.read_table b o i)
    | 23L when Option.is_some binary_view ->
        Option.get binary_view (Rt.Ref.read_table b o i)
    | 24L when Option.is_some utf8_view ->
        Option.get utf8_view (Rt.Ref.read_table b o i)
    | 25L when Option.is_some list_view ->
        Option.get list_view (Rt.Ref.read_table b o i)
    | 26L when Option.is_some large_list_view ->
        Option.get large_list_view (Rt.Ref.read_table b o i)
    | _ -> default t
end

module Org = struct
  module Apache = struct
    module Arrow = struct
      module Flatbuf = struct
        module UnionMode = struct
          type t = Rt.Short.t

          let sparse = Rt.Short.of_default 0L
          let dense = Rt.Short.of_default 1L

          let to_string e =
            match Rt.Short.to_default e with
            | 0L -> "sparse"
            | 1L -> "dense"
            | x ->
                "<org.apache.arrow.flatbuf.UnionMode: " ^ Int64.to_string x
                ^ ">"

          module Vector = Rt.Short.Vector
        end

        module Type = struct
          type t = Rt.UType.t

          let none = Rt.UType.of_default 0L
          let null = Rt.UType.of_default 1L
          let int = Rt.UType.of_default 2L
          let floating_point = Rt.UType.of_default 3L
          let binary = Rt.UType.of_default 4L
          let utf8 = Rt.UType.of_default 5L
          let bool = Rt.UType.of_default 6L
          let decimal = Rt.UType.of_default 7L
          let date = Rt.UType.of_default 8L
          let time = Rt.UType.of_default 9L
          let timestamp = Rt.UType.of_default 10L
          let interval = Rt.UType.of_default 11L
          let list = Rt.UType.of_default 12L
          let struct_ = Rt.UType.of_default 13L
          let union = Rt.UType.of_default 14L
          let fixed_size_binary = Rt.UType.of_default 15L
          let fixed_size_list = Rt.UType.of_default 16L
          let map = Rt.UType.of_default 17L
          let duration = Rt.UType.of_default 18L
          let large_binary = Rt.UType.of_default 19L
          let large_utf8 = Rt.UType.of_default 20L
          let large_list = Rt.UType.of_default 21L
          let run_end_encoded = Rt.UType.of_default 22L
          let binary_view = Rt.UType.of_default 23L
          let utf8_view = Rt.UType.of_default 24L
          let list_view = Rt.UType.of_default 25L
          let large_list_view = Rt.UType.of_default 26L

          let to_string e =
            match Rt.UType.to_default e with
            | 0L -> "none"
            | 1L -> "null"
            | 2L -> "int"
            | 3L -> "floating_point"
            | 4L -> "binary"
            | 5L -> "utf8"
            | 6L -> "bool"
            | 7L -> "decimal"
            | 8L -> "date"
            | 9L -> "time"
            | 10L -> "timestamp"
            | 11L -> "interval"
            | 12L -> "list"
            | 13L -> "struct_"
            | 14L -> "union"
            | 15L -> "fixed_size_binary"
            | 16L -> "fixed_size_list"
            | 17L -> "map"
            | 18L -> "duration"
            | 19L -> "large_binary"
            | 20L -> "large_utf8"
            | 21L -> "large_list"
            | 22L -> "run_end_encoded"
            | 23L -> "binary_view"
            | 24L -> "utf8_view"
            | 25L -> "list_view"
            | 26L -> "large_list_view"
            | x -> "<org.apache.arrow.flatbuf.Type: " ^ Int64.to_string x ^ ">"
        end

        module TimeUnit = struct
          type t = Rt.Short.t

          let second = Rt.Short.of_default 0L
          let millisecond = Rt.Short.of_default 1L
          let microsecond = Rt.Short.of_default 2L
          let nanosecond = Rt.Short.of_default 3L

          let to_string e =
            match Rt.Short.to_default e with
            | 0L -> "second"
            | 1L -> "millisecond"
            | 2L -> "microsecond"
            | 3L -> "nanosecond"
            | x ->
                "<org.apache.arrow.flatbuf.TimeUnit: " ^ Int64.to_string x ^ ">"

          module Vector = Rt.Short.Vector
        end

        module Precision = struct
          type t = Rt.Short.t

          let half = Rt.Short.of_default 0L
          let single = Rt.Short.of_default 1L
          let double = Rt.Short.of_default 2L

          let to_string e =
            match Rt.Short.to_default e with
            | 0L -> "half"
            | 1L -> "single"
            | 2L -> "double"
            | x ->
                "<org.apache.arrow.flatbuf.Precision: " ^ Int64.to_string x
                ^ ">"

          module Vector = Rt.Short.Vector
        end

        module MetadataVersion = struct
          type t = Rt.Short.t

          let v1 = Rt.Short.of_default 0L
          let v2 = Rt.Short.of_default 1L
          let v3 = Rt.Short.of_default 2L
          let v4 = Rt.Short.of_default 3L
          let v5 = Rt.Short.of_default 4L

          let to_string e =
            match Rt.Short.to_default e with
            | 0L -> "v1"
            | 1L -> "v2"
            | 2L -> "v3"
            | 3L -> "v4"
            | 4L -> "v5"
            | x ->
                "<org.apache.arrow.flatbuf.MetadataVersion: "
                ^ Int64.to_string x ^ ">"

          module Vector = Rt.Short.Vector
        end

        module IntervalUnit = struct
          type t = Rt.Short.t

          let year_month = Rt.Short.of_default 0L
          let day_time = Rt.Short.of_default 1L
          let month_day_nano = Rt.Short.of_default 2L

          let to_string e =
            match Rt.Short.to_default e with
            | 0L -> "year_month"
            | 1L -> "day_time"
            | 2L -> "month_day_nano"
            | x ->
                "<org.apache.arrow.flatbuf.IntervalUnit: " ^ Int64.to_string x
                ^ ">"

          module Vector = Rt.Short.Vector
        end

        module Feature = struct
          type t = Rt.Long.t

          let unused = Rt.Long.of_default 0L
          let dictionary_replacement = Rt.Long.of_default 1L
          let compressed_body = Rt.Long.of_default 2L

          let to_string e =
            match Rt.Long.to_default e with
            | 0L -> "unused"
            | 1L -> "dictionary_replacement"
            | 2L -> "compressed_body"
            | x ->
                "<org.apache.arrow.flatbuf.Feature: " ^ Int64.to_string x ^ ">"

          module Vector = Rt.Long.Vector
        end

        module Endianness = struct
          type t = Rt.Short.t

          let little = Rt.Short.of_default 0L
          let big = Rt.Short.of_default 1L

          let to_string e =
            match Rt.Short.to_default e with
            | 0L -> "little"
            | 1L -> "big"
            | x ->
                "<org.apache.arrow.flatbuf.Endianness: " ^ Int64.to_string x
                ^ ">"

          module Vector = Rt.Short.Vector
        end

        module DictionaryKind = struct
          type t = Rt.Short.t

          let dense_array = Rt.Short.of_default 0L

          let to_string e =
            match Rt.Short.to_default e with
            | 0L -> "dense_array"
            | x ->
                "<org.apache.arrow.flatbuf.DictionaryKind: " ^ Int64.to_string x
                ^ ">"

          module Vector = Rt.Short.Vector
        end

        module DateUnit = struct
          type t = Rt.Short.t

          let day = Rt.Short.of_default 0L
          let millisecond = Rt.Short.of_default 1L

          let to_string e =
            match Rt.Short.to_default e with
            | 0L -> "day"
            | 1L -> "millisecond"
            | x ->
                "<org.apache.arrow.flatbuf.DateUnit: " ^ Int64.to_string x ^ ">"

          module Vector = Rt.Short.Vector
        end

        module Utf8View = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module Utf8 = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module Union = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] mode b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 0L))

          let[@inline] type_ids b o = Rt.Ref.read_table_opt b o 6

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:2
            let finish b = Rt.Builder.end_table b

            let add_mode =
              Rt.Short.(push_slot_default 0 ~default:(of_default 0L))

            let add_type_ids = Rt.Ref.push_slot 1
          end
        end

        module Timestamp = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] unit b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 0L))

          let[@inline] timezone b o = Rt.Ref.read_table_opt b o 6

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:2
            let finish b = Rt.Builder.end_table b

            let add_unit =
              Rt.Short.(push_slot_default 0 ~default:(of_default 0L))

            let add_timezone = Rt.Ref.push_slot 1
          end
        end

        module Time = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] unit b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 1L))

          let[@inline] bit_width b o =
            Rt.Int.(read_table_default b o 6 ~default:(of_default 32L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:2
            let finish b = Rt.Builder.end_table b

            let add_unit =
              Rt.Short.(push_slot_default 0 ~default:(of_default 1L))

            let add_bit_width =
              Rt.Int.(push_slot_default 1 ~default:(of_default 32L))
          end
        end

        module Struct_ = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module Schema = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] endianness b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 0L))

          let[@inline] fields b o = Rt.Ref.read_table_opt b o 6
          let[@inline] custom_metadata b o = Rt.Ref.read_table_opt b o 8
          let[@inline] features b o = Rt.Ref.read_table_opt b o 10

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:4
            let finish b = Rt.Builder.end_table b

            let add_endianness =
              Rt.Short.(push_slot_default 0 ~default:(of_default 0L))

            let add_fields = Rt.Ref.push_slot 1
            let add_custom_metadata = Rt.Ref.push_slot 2
            let add_features = Rt.Ref.push_slot 3
          end
        end

        module RunEndEncoded = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module Null = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module Map = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] keys_sorted b o =
            Rt.Bool.(read_table_default b o 4 ~default:(of_default false))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:1
            let finish b = Rt.Builder.end_table b

            let add_keys_sorted =
              Rt.Bool.(push_slot_default 0 ~default:(of_default false))
          end
        end

        module ListView = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module List = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module LargeUtf8 = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module LargeListView = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module LargeList = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module LargeBinary = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module KeyValue = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] key b o = Rt.Ref.read_table_opt b o 4
          let[@inline] value b o = Rt.Ref.read_table_opt b o 6

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:2
            let finish b = Rt.Builder.end_table b
            let add_key = Rt.Ref.push_slot 0
            let add_value = Rt.Ref.push_slot 1
          end
        end

        module Interval = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] unit b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 0L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:1
            let finish b = Rt.Builder.end_table b

            let add_unit =
              Rt.Short.(push_slot_default 0 ~default:(of_default 0L))
          end
        end

        module Int = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] bit_width b o =
            Rt.Int.(read_table_default b o 4 ~default:(of_default 0L))

          let[@inline] is_signed b o =
            Rt.Bool.(read_table_default b o 6 ~default:(of_default false))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:2
            let finish b = Rt.Builder.end_table b

            let add_bit_width =
              Rt.Int.(push_slot_default 0 ~default:(of_default 0L))

            let add_is_signed =
              Rt.Bool.(push_slot_default 1 ~default:(of_default false))
          end
        end

        module Footer = struct
          type t

          module Vector = Rt.Ref.Vector

          let extension = None
          let identifier = None

          let[@inline] root ?(size_prefixed = false) ?(off = 0) p b =
            Rt.get_root p b ~size_prefixed ~off

          let finish_buf ?(size_prefixed = false) =
            Rt.Builder.finish ?identifier ~size_prefixed

          let[@inline] version b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 0L))

          let[@inline] schema b o = Rt.Ref.read_table_opt b o 6
          let[@inline] dictionaries b o = Rt.Ref.read_table_opt b o 8
          let[@inline] record_batches b o = Rt.Ref.read_table_opt b o 10
          let[@inline] custom_metadata b o = Rt.Ref.read_table_opt b o 12

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:5
            let finish b = Rt.Builder.end_table b

            let add_version =
              Rt.Short.(push_slot_default 0 ~default:(of_default 0L))

            let add_schema = Rt.Ref.push_slot 1
            let add_dictionaries = Rt.Ref.push_slot 2
            let add_record_batches = Rt.Ref.push_slot 3
            let add_custom_metadata = Rt.Ref.push_slot 4
          end
        end

        module FloatingPoint = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] precision b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 0L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:1
            let finish b = Rt.Builder.end_table b

            let add_precision =
              Rt.Short.(push_slot_default 0 ~default:(of_default 0L))
          end
        end

        module FixedSizeList = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] list_size b o =
            Rt.Int.(read_table_default b o 4 ~default:(of_default 0L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:1
            let finish b = Rt.Builder.end_table b

            let add_list_size =
              Rt.Int.(push_slot_default 0 ~default:(of_default 0L))
          end
        end

        module FixedSizeBinary = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] byte_width b o =
            Rt.Int.(read_table_default b o 4 ~default:(of_default 0L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:1
            let finish b = Rt.Builder.end_table b

            let add_byte_width =
              Rt.Int.(push_slot_default 0 ~default:(of_default 0L))
          end
        end

        module Field = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] name b o = Rt.Ref.read_table_opt b o 4

          let[@inline] nullable b o =
            Rt.Bool.(read_table_default b o 6 ~default:(of_default false))

          let[@inline] type_type b o =
            Rt.UType.(read_table_default b o 8 ~default:(of_default 0L))

          let[@inline] type_ ?none ?null ?int ?floating_point ?binary ?utf8
              ?bool ?decimal ?date ?time ?timestamp ?interval ?list ?struct_
              ?union ?fixed_size_binary ?fixed_size_list ?map ?duration
              ?large_binary ?large_utf8 ?large_list ?run_end_encoded
              ?binary_view ?utf8_view ?list_view ?large_list_view ~default b o =
            Union_.read_table_type___2 b 10 (type_type b o) ?none ?null ?int
              ?floating_point ?binary ?utf8 ?bool ?decimal ?date ?time
              ?timestamp ?interval ?list ?struct_ ?union ?fixed_size_binary
              ?fixed_size_list ?map ?duration ?large_binary ?large_utf8
              ?large_list ?run_end_encoded ?binary_view ?utf8_view ?list_view
              ?large_list_view ~default o

          let[@inline] dictionary b o = Rt.Ref.read_table_opt b o 12
          let[@inline] children b o = Rt.Ref.read_table_opt b o 14
          let[@inline] custom_metadata b o = Rt.Ref.read_table_opt b o 16

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:7
            let finish b = Rt.Builder.end_table b
            let add_name = Rt.Ref.push_slot 0

            let add_nullable =
              Rt.Bool.(push_slot_default 1 ~default:(of_default false))

            let add_type__null = Rt.Ref.push_union 2 3 Type.null
            let add_type__int = Rt.Ref.push_union 2 3 Type.int

            let add_type__floating_point =
              Rt.Ref.push_union 2 3 Type.floating_point

            let add_type__binary = Rt.Ref.push_union 2 3 Type.binary
            let add_type__utf8 = Rt.Ref.push_union 2 3 Type.utf8
            let add_type__bool = Rt.Ref.push_union 2 3 Type.bool
            let add_type__decimal = Rt.Ref.push_union 2 3 Type.decimal
            let add_type__date = Rt.Ref.push_union 2 3 Type.date
            let add_type__time = Rt.Ref.push_union 2 3 Type.time
            let add_type__timestamp = Rt.Ref.push_union 2 3 Type.timestamp
            let add_type__interval = Rt.Ref.push_union 2 3 Type.interval
            let add_type__list = Rt.Ref.push_union 2 3 Type.list
            let add_type__struct_ = Rt.Ref.push_union 2 3 Type.struct_
            let add_type__union = Rt.Ref.push_union 2 3 Type.union

            let add_type__fixed_size_binary =
              Rt.Ref.push_union 2 3 Type.fixed_size_binary

            let add_type__fixed_size_list =
              Rt.Ref.push_union 2 3 Type.fixed_size_list

            let add_type__map = Rt.Ref.push_union 2 3 Type.map
            let add_type__duration = Rt.Ref.push_union 2 3 Type.duration
            let add_type__large_binary = Rt.Ref.push_union 2 3 Type.large_binary
            let add_type__large_utf8 = Rt.Ref.push_union 2 3 Type.large_utf8
            let add_type__large_list = Rt.Ref.push_union 2 3 Type.large_list

            let add_type__run_end_encoded =
              Rt.Ref.push_union 2 3 Type.run_end_encoded

            let add_type__binary_view = Rt.Ref.push_union 2 3 Type.binary_view
            let add_type__utf8_view = Rt.Ref.push_union 2 3 Type.utf8_view
            let add_type__list_view = Rt.Ref.push_union 2 3 Type.list_view

            let add_type__large_list_view =
              Rt.Ref.push_union 2 3 Type.large_list_view

            let add_dictionary = Rt.Ref.push_slot 4
            let add_children = Rt.Ref.push_slot 5
            let add_custom_metadata = Rt.Ref.push_slot 6
          end
        end

        module Duration = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] unit b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 1L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:1
            let finish b = Rt.Builder.end_table b

            let add_unit =
              Rt.Short.(push_slot_default 0 ~default:(of_default 1L))
          end
        end

        module DictionaryEncoding = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] id b o =
            Rt.Long.(read_table_default b o 4 ~default:(of_default 0L))

          let[@inline] index_type b o = Rt.Ref.read_table_opt b o 6

          let[@inline] is_ordered b o =
            Rt.Bool.(read_table_default b o 8 ~default:(of_default false))

          let[@inline] dictionary_kind b o =
            Rt.Short.(read_table_default b o 10 ~default:(of_default 0L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:4
            let finish b = Rt.Builder.end_table b
            let add_id = Rt.Long.(push_slot_default 0 ~default:(of_default 0L))
            let add_index_type = Rt.Ref.push_slot 1

            let add_is_ordered =
              Rt.Bool.(push_slot_default 2 ~default:(of_default false))

            let add_dictionary_kind =
              Rt.Short.(push_slot_default 3 ~default:(of_default 0L))
          end
        end

        module Decimal = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] precision b o =
            Rt.Int.(read_table_default b o 4 ~default:(of_default 0L))

          let[@inline] scale b o =
            Rt.Int.(read_table_default b o 6 ~default:(of_default 0L))

          let[@inline] bit_width b o =
            Rt.Int.(read_table_default b o 8 ~default:(of_default 128L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:3
            let finish b = Rt.Builder.end_table b

            let add_precision =
              Rt.Int.(push_slot_default 0 ~default:(of_default 0L))

            let add_scale =
              Rt.Int.(push_slot_default 1 ~default:(of_default 0L))

            let add_bit_width =
              Rt.Int.(push_slot_default 2 ~default:(of_default 128L))
          end
        end

        module Date = struct
          type t

          module Vector = Rt.Ref.Vector

          let[@inline] unit b o =
            Rt.Short.(read_table_default b o 4 ~default:(of_default 1L))

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:1
            let finish b = Rt.Builder.end_table b

            let add_unit =
              Rt.Short.(push_slot_default 0 ~default:(of_default 1L))
          end
        end

        module Buffer = struct
          type t = Rt.Long.t * Rt.Long.t

          module Vector = Rt.Struct.Vector (struct
            type builder_elt = t

            let size = 16
            let set = Struct.set_buffer__1
          end)

          let[@inline] offset b s = Rt.Long.read_offset b s 0
          let[@inline] length b s = Rt.Long.read_offset b s 8
        end

        module Bool = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module Block = struct
          type t = Rt.Long.t * Rt.Int.t * Rt.Long.t

          module Vector = Rt.Struct.Vector (struct
            type builder_elt = t

            let size = 24
            let set = Struct.set_block__0
          end)

          let[@inline] offset b s = Rt.Long.read_offset b s 0
          let[@inline] meta_data_length b s = Rt.Int.read_offset b s 8
          let[@inline] body_length b s = Rt.Long.read_offset b s 16
        end

        module BinaryView = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end

        module Binary = struct
          type t

          module Vector = Rt.Ref.Vector

          module Builder = struct
            type t = Rt.Builder.t

            let start b = Rt.Builder.start_table b ~n_fields:0
            let finish b = Rt.Builder.end_table b
          end
        end
      end
      (* Flatbuf *)
    end
    (* Arrow *)
  end
  (* Apache *)
end
(* Org *)
