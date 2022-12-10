import re

from typing import Generic, TypeVar, Optional, Sequence, Tuple, NamedTuple, List, Callable, Iterator

T = TypeVar('T')
V = TypeVar('V')

def get_input(day: int, path: str = '../inputs/day%02d.txt') -> str:
    with open(path % day, 'r', encoding='utf-8') as f:
        return f.read()

def ints(text: str, sign_prefixes: bool = True) -> Tuple[int, ...]:
    regex = r'([\-+]?\d+)' if sign_prefixes else r'(\d+)'
    return tuple(map(int, re.findall(regex, text)))

def sign(a: float) -> int:
    """ Returns the sign of a """
    return 0 if a == 0 else (-1 if a < 0 else 1)

class Point2(NamedTuple):
    x: int
    y: int


class FiniteGrid(Generic[T]):

    @staticmethod
    def of_empty(width: int, height: int, default: Optional[T] = None, wrap: bool = False) -> 'FiniteGrid[T]':
        return FiniteGrid(width, height, [default] * width * height, default, wrap)

    @staticmethod
    def of_str(s: str, default: Optional[T] = None, wrap: bool = False) -> 'FiniteGrid[T]':
        return FiniteGrid.of_iter(s.strip().split('\n'), default, wrap)

    @staticmethod
    def of_iter(seq: Sequence[Sequence[T]], default: Optional[T] = None, wrap: bool = False) -> 'FiniteGrid[T]':
        height, width = len(seq), len(seq[0])
        assert all(len(sub) == width for sub in seq), 'Different widths of sub sequences'
        return FiniteGrid(width, height, [e for r in seq for e in r], default, wrap)

    def __init__(self, width: int, height: int, array: List[T], default: Optional[T], wrap: bool):
        self.width: int = width
        self.height: int = height
        self.default: T = default
        self.wrap: bool = wrap
        self.array: List[T] = array

    def __getitem__(self, key: Tuple[int, int]) -> T:
        if self.wrap:
            x, y = key[0] % self.width, key[1] % self.height
        else:
            x, y = key
        if 0 <= x < self.width and 0 <= y < self.height:
            return self.array[x + self.width * y]
        elif self.default is not None:
            return self.default
        raise ValueError('Tried to get the value at invalid location (%d, %d)' % (x, y))

    def __setitem__(self, key: Tuple[int, int], value: T):
        if self.wrap:
            x, y = key[0] % self.width, key[1] % self.height
        else:
            x, y = key
        if 0 <= x < self.width and 0 <= y < self.height:
            self.array[x + self.width * y] = value
        elif self.default is None:
            raise ValueError('Tried to set the value at invalid location (%d, %d)' % (x, y))

    def __contains__(self, key: Tuple[int, int]) -> bool:
        return 0 <= key[0] < self.width and 0 <= key[1] < self.height

    def __len__(self) -> int:
        return len(self.array)

    def __eq__(self, other):
        return other is not None and isinstance(other, FiniteGrid) and self.array == other.array

    def __repr__(self):
        return '[%d x %d Grid]\n%s' % (self.width, self.height, str(self))

    def __str__(self) -> str:
        return '\n'.join(''.join(str(self.array[x + self.width * y]) for x in range(self.width)) for y in range(self.height))

    def map(self, f: Callable[[Point2], T]) -> 'FiniteGrid[T]':
        return FiniteGrid(self.width, self.height, [f(pos) for pos in self.locations()], self.default, self.wrap)

    def map_values(self, f: Callable[[T], V]) -> 'FiniteGrid[V]':
        return FiniteGrid(self.width, self.height, [f(v) for v in self.array], f(self.default) if self.default is not None else None, self.wrap)

    def locations(self) -> Iterator[Point2]:
        """ An iterator over all coordinate positions within the grid """
        for y in range(self.height):
            for x in range(self.width):
                yield Point2(x, y)
