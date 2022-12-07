# Day 7: No Space Left On Device
# Rank: 100 / 119

from utils import get_input
from typing import Dict, Set


def main(text: str):
    files: Dict[str, int] = {}  # All files -> sizes
    dirs: Set[str] = set()  # All directories

    path = []

    def named(f: str) -> str:
        return '/'.join(path) + '/' + f

    for line in text.split('\n'):
        if line == '$ cd /':
            path = ['']
        elif line.startswith('$ cd'):
            _, _, cd = line.split(' ')
            if cd == '..':
                path.pop(-1)
            else:
                path.append(cd)
        elif line == '$ ls':
            pass
        elif line.startswith('dir'):  # Inside 'ls' output
            _, cd = line.split()
            dirs.add(named(cd))
        else:  # A size, file pair
            size, name = line.split()
            files[named(name)] = int(size)

    dir_sizes: Dict[str, int] = {
        d: sum(v for k, v in files.items() if k.startswith(d))
        for d in dirs
    }  # Cumulative size of each directory

    total_size = sum(v for v in files.values())
    upper_limit = 70000000 - 30000000

    print('Part 1:', sum(d for d in dir_sizes.values() if d <= 100000))
    print('Part 2:', next(d for d in sorted(dir_sizes.values()) if total_size - d < upper_limit))


if __name__ == '__main__':
    main(get_input(7))
