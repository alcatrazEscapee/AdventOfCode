import re

from typing import Tuple

def get_input(day: int, path: str = '../inputs/day%02d.txt') -> str:
    with open(path % day, 'r', encoding='utf-8') as f:
        return f.read()

def ints(text: str, sign_prefixes: bool = True) -> Tuple[int, ...]:
    regex = r'([\-+]?\d+)' if sign_prefixes else r'(\d+)'
    return tuple(map(int, re.findall(regex, text)))