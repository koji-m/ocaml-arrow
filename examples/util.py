import sys
import pyarrow as pa

def read_batch():
    reader = pa.ipc.open_stream(sys.stdin.buffer)
    batches = [batch for batch in reader]
    table = pa.Table.from_batches(batches)
    print(table.to_pandas())

def write_batch():
    arr = pa.array([1, 2, None, 4], type=pa.int32())
    batch = pa.RecordBatch.from_arrays([arr, arr], names=["col1", "col2"])
    writer = pa.ipc.new_stream(
        pa.output_stream(sys.stdout.buffer),
        batch.schema,
    )
    writer.write_batch(batch)
    writer.close()

if __name__ == "__main__":
    if sys.argv[1] == "write":
        # write RecordBatch with two int32 columns to stdout
        write_batch()
    else:
        # read RecordBatch from stdin
        read_batch()

