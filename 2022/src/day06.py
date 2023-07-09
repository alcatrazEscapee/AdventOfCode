# Day 06: Tuning Trouble
# Rank: 214 / 131

from utils import get_input


def main(text: str):
    print('Part 1:', start_of_text_marker(text, 4))
    print('Part 2:', start_of_text_marker(text, 14))

def start_of_text_marker(text: str, size: int) -> int:
    for i in range(len(text)):
        if len(set(text[i:i+size])) == size:
            return i + size


if __name__ == '__main__':
    main(get_input(6))
