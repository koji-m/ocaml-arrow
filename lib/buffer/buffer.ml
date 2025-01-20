type t = {data : bytes ref; offset : int; len : int}

let create ?(init ='\x00') n =
    {
        data = ref (Bytes.make n init);
        offset = 0;
        len = n;
    }

let of_bytes b =
    {
        data = ref b;
        offset = 0;
        len = Bytes.length b;
    }

let of_string s =
    {
        data = ref (Bytes.of_string s);
        offset = 0;
        len = String.length s;
    }

let get b i =
    Bytes.get !(b.data) (b.offset + i)

let set b i c =
    Bytes.set !(b.data) (b.offset + i) c

let get_int32 b i =
    Bytes.get_int32_le !(b.data) (b.offset + i)

let set_int32 b o i =
    Bytes.set_int32_le !(b.data) (b.offset + o) i

let data b = !(b.data)

let length b = b.len

let sub b ~off ~len =
    {
        data = b.data;
        offset = b.offset + off;
        len = len;
    }

let to_byte_slice (b : t) =
    Byte_slice.(
        {
            data = b.data;
            offset = b.offset;
            len = b.len;
        }
    )

let of_byte_slice (b : Byte_slice.t) =
    {
        data = b.data;
        offset = b.offset;
        len = b.len;
    }

let copy b ~off ~len =
    {
        data = ref (Bytes.sub !(b.data) (b.offset + off) len);
        offset = 0;
        len = len;
    }

let fold_left f b acc =
    let len = length b in
    let rec fold_left_ f b acc i =
        if i < len then
            fold_left_ f b (f acc (get b i)) (i + 1)
        else
            acc
    in
    fold_left_ f b acc 0

let to_bytes b =
    Bytes.sub !(b.data) b.offset b.len
