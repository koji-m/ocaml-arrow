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

def read_batch_from_file():
    with open("example.arrow", "rb") as f:
        reader = pa.ipc.open_file(f)
        print(reader.read_pandas())

if __name__ == "__main__":
    arg = sys.argv[1]
    if arg == "write":
        # write RecordBatch with two int32 columns to stdout
        write_batch()
    elif arg == "read":
        # read RecordBatch from stdin
        read_batch()
    elif arg == "read_file":
        # read RecordBatch from file
        read_batch_from_file()

