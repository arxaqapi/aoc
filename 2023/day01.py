patterns = (
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
)

input_file = "input_01.txt"

with open(input_file, 'r') as f:
    lines = f.readlines()


def word_to_int(w):
    for i, p in enumerate(patterns):
        if w == p:
            return i + 1


total = 0
for line in lines:
    buffer = []
    for i, c in enumerate(line):
        if c in map(str, list(range(1, 10))):
            buffer.append(int(c))
        else:
            for pattern in patterns:
                # check that pattern fits
                if len(pattern) + i < len(line):
                    # if pattern fits check if it exists
                    if line[i: i + len(pattern)] == pattern:
                    # if exists, convert and add to buffer
                        buffer.append(word_to_int(pattern))
    print(buffer)
    total += 10 * buffer[0] + buffer[-1]

# 54824
print(total)
