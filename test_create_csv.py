num_rows = 30
data = {"a": list(range(num_rows)),
        "b": list(range(num_rows)),
        "c": list(range(num_rows))}

print("METADATA test_val: ", 0)

for i in range(num_rows):
    print("LOG a: ", data['a'][i])
    if (i == num_rows-1):
        break # simulate incomplete row
    print("LOG b: ", data['b'][i])
    print("LOG c: ", data['c'][i])
