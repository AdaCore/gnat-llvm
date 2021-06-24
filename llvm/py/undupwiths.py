#! /usr/bin/env python3

from sys import argv

# Take each of the files passed to us and remove any duplicate "with"
# lines from each of them

for fn in argv[1:]:
    with open(fn, 'r') as file:
        lines = file.readlines()

    withs = {}
    with open(fn, 'w') as file:
        for line in lines:
            if line[0:4] == 'with':
                if line not in withs:
                    file.write(line)
                    withs[line] = True
            else:
                file.write(line)
