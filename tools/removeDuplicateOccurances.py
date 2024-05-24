import sys
import re
import json

file = sys.argv[2]
patterns = sys.argv[1].split(",")

with open(file) as f:
    lines = f.readlines()


for pattern in patterns:
    moreThanOneOccurance = lambda lns : len([True for line in lns if re.match(pattern, line)]) > 1

    while moreThanOneOccurance(lines):
        state = "tryMatch"

        nlines = []
        for line in lines:
            isLineEmpty = line.strip() == ''
            if state == "tryMatch":
                if re.match(pattern, line):
                    state = "delete"
                    continue
            elif state == "delete":
                if isLineEmpty:
                    state = 'delete-seen-empty'
                continue
            elif state == "delete-seen-empty":
                if isLineEmpty:
                    state = "done"
                    # sys.exit()
                else:
                    state = "delete"
                continue

            nlines.append(line)
        
        lines = nlines

with open(file, "w") as f:
    for l in lines:
        print(l, end='', file=f)