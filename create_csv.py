#!/usr/bin/env python3

import os, sys

def extract_key_val(line, prefix):
    data = line[len(prefix):]
    key, value = data.split(":")
    return key, value.strip()

if os.path.isfile(sys.argv[1]) and sys.argv[1] != 'trash.txt':
    print("THIS FILE EXISTS!")
    exit(0)
    
f = open(sys.argv[1], 'w')

metadata = {}
data = [dict()]
last_read_row = 0
last_written_row = 0
first_col = None
col_names = []
for line in sys.stdin:
    if line.startswith("METADATA"):
        key, val = extract_key_val(line, "METADATA ")
        metadata[key] = val

    if line.startswith("LOG"):
        key, val = extract_key_val(line, "LOG ")

        if first_col is None:
            first_col = key
        
        if key not in col_names:
            if last_read_row != 0:
                print("ERROR: unseen value encountered halfway")
                exit(0)
            col_names.append(key)
        # onto another row
        elif key == first_col:
            last_read_row += 1
            if last_read_row == 1:
                # write out header
                for k, v in metadata.items():
                    f.write("# {} : {}\n".format(k, v))
                f.write(",".join(col_names)+"\n")

            data.append(dict())

        data[last_read_row][key] = val
            

    if last_read_row % 10 == 0:
        # NOTE: doesn't write incomplete rows
        for i in range(last_written_row, last_read_row):
            row = ""
            for col_name in col_names:
                row += data[i][col_name] + ","
            f.write(row[:-1]+"\n")
        last_written_row = last_read_row

# dump remaining rows
for i in range(last_written_row, last_read_row+1):
    row = ""
    for col_name in col_names:
        # Note: inserts None to handle incomplete rows
        row += data[i].get(col_name, "") + ","
    f.write(row[:-1]+"\n")

f.close()
        
        

