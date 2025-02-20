from datetime import datetime
import sys
import pyarrow as pa

def read_batch():
    reader = pa.ipc.open_stream(sys.stdin.buffer)
    batches = [batch for batch in reader]
    table = pa.Table.from_batches(batches)
    print(table.to_pandas())

def write_batch():
    arr1 = pa.array([1, 2, None, 4], type=pa.int32())
    arr2 = pa.array([10, 20, None, 40], type=pa.int64())
    arr3 = pa.array([3.14, None, 2.718, 1.618], type=pa.float64())
    arr4 = pa.array(
        [datetime(2022, 1, 1), datetime(2023, 4, 10), datetime(2024, 8, 15), datetime(2025, 2, 9)],
        type=pa.date32(),
    )
    arr5 = pa.array(
        [datetime(2022, 1, 1), datetime(2023, 4, 10), datetime(2024, 8, 15), datetime(2025, 2, 9)],
        type=pa.date64(),
    )
    arr6 = pa.array(
        [0, None, 43200, 86399],
        type=pa.time32("s")
    )
    arr7 = pa.array(
        [0, None, 43200000, 86399999],
        type=pa.time32("ms")
    )
    print(arr3.buffers())
    print(arr4.buffers())
    batch = pa.RecordBatch.from_arrays(
        [arr1, arr2, arr3, arr4, arr5, arr6, arr7],
        names=["col1", "col2", "col3", "col4", "col5", "col6", "col7"],
    )
    print(batch)
    writer = pa.ipc.new_stream(
        pa.output_stream(sys.stdout.buffer),
        batch.schema,
    )
    writer.write_batch(batch)
    writer.close()

def read_batch_from_file():
    with open("example.arrow", "rb") as f:
        reader = pa.ipc.open_file(f)
        print(reader.read_pandas().to_string())

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

