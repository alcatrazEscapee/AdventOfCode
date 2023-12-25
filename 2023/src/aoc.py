from typing import Sequence
from math import gcd


class Fraction:
    """
    Integer fraction (n / d) in reduced normalized form: gcd(n, d) == 1, d >= 0,
    supporting arbitrary precision integer values (standard python `int`)
    """

    n: int
    d: int

    @staticmethod
    def of(*n) -> 'Fraction':
        """ Converts n to a representable fraction """
        if len(n) == 1:
            n, *_ = n
        if isinstance(n, Fraction):
            return n
        if isinstance(n, int):
            return Fraction(n, 1)
        if isinstance(n, Sequence) and len(n) == 2:
            a, b = n
            if isinstance(a, int) and isinstance(b, int):
                return Fraction(a, b)
        raise TypeError('Not convertible to a fraction: %s' % repr(n))

    def __init__(self, n: int, d: int):
        if d < 0:
            n, d = -n, -d
        if d == 0:  # For our use cases, it is easier to allow d=0 and check than to raise an error
            n = 1
        g = gcd(n, d)
        self.n = n // g
        self.d = d // g

    def __add__(self, other) -> 'Fraction':
        a, b = self, Fraction.of(other)
        n = a.n * b.d + b.n * a.d
        d = a.d * b.d
        return Fraction(n, d)

    def __mul__(self, other) -> 'Fraction':
        a, b = self, Fraction.of(other)
        n = a.n * b.n
        d = a.d * b.d
        return Fraction(n, d)

    def __eq__(self, other) -> bool:
        a, b = self, Fraction.of(other)
        return a.n == b.n and a.d == b.d

    def __neg__(self) -> 'Fraction': return Fraction(-self.n, self.d)
    def __abs__(self) -> 'Fraction': return Fraction(abs(self.n), self.d)
    def __invert__(self) -> 'Fraction': return self.reciprocal()
    def __pow__(self, power, modulo=None) -> 'Fraction': return Fraction(pow(self.n, power, modulo), pow(self.d, power, modulo))

    def __sub__(self, other) -> 'Fraction': return self.__add__(-Fraction.of(other))
    def __truediv__(self, other) -> 'Fraction': return self.__floordiv__(other)
    def __floordiv__(self, other) -> 'Fraction': return self.__mul__(Fraction.of(other).reciprocal())

    def __radd__(self, other) -> 'Fraction': return Fraction.of(other).__add__(self)
    def __rmul__(self, other) -> 'Fraction': return Fraction.of(other).__mul__(self)
    def __rsub__(self, other) -> 'Fraction': return Fraction.of(other).__sub__(self)
    def __rtruediv__(self, other) -> 'Fraction': return Fraction.of(other).__truediv__(self)
    def __rfloordiv__(self, other) -> 'Fraction': return Fraction.of(other).__floordiv__(self)

    def __ne__(self, other) -> bool: return not self.__eq__(other)
    def __lt__(self, other) -> bool: return self.ratio() < Fraction.of(other).ratio()
    def __gt__(self, other) -> bool: return self.ratio() > Fraction.of(other).ratio()
    def __le__(self, other) -> bool: return self.ratio() <= Fraction.of(other).ratio()
    def __ge__(self, other) -> bool: return self.ratio() >= Fraction.of(other).ratio()

    def __str__(self) -> str: return '%d / %d' % (self.n, self.d)
    def __repr__(self) -> str: return self.__str__()
    
    def __int__(self) -> int:
        if self.d != 1:
            raise TypeError('Fraction %s is not convertible to an int' % self)
        return self.n

    def reciprocal(self) -> 'Fraction': return Fraction(self.d, self.n)
    def ratio(self) -> float: return self.n / self.d


def solve(a: list[list], b: list) -> list[Fraction]:
    """
    Solves the linear system equation a*x = b, _in the rationals_
    This has no extra handling for error cases, where a solution doesn't exist, or isn't all linearly independent
    """
    assert len(a) == len(b), 'size of a == size of b'
    
    a: list[list[Fraction]] = [[Fraction.of(aij) for aij in ai] for ai in a]  # Indexed by [row][col]
    b: list[Fraction] = [Fraction.of(bi) for bi in b]  # Indexed by [row]
    n: int = len(b)

    def swap(i: int, j: int):
        """ Swaps row i and row j """
        if i != j:
            a[i], a[j] = a[j], a[i]
            b[i], b[j] = b[j], b[i]

    def mul(i: int, m: Fraction):
        """ Multiplies row i by the scalar m """
        b[i] *= m
        a[i] = [aij * m for aij in a[i]]
    
    def sub(i: int, j: int, m: Fraction):
        """ Subtracts row j by m * row i for scalar m """
        if i != j:
            a[j] = [ajk - aik * m for aik, ajk in zip(a[i], a[j])]
            b[j] -= b[i] * m

    for i in range(n):
        ic = next(j for j in range(i, n) if a[i][j] != 0)  # Pick the first row with a != 0 value in this column
        swap(i, ic)  # Swap this row to the target row, if needed
        mul(i, a[i][i].reciprocal())  # Multiply this row by the reciprocal of the selected value, to produce a 1
        for j in range(n):
            sub(i, j, a[j][i])  # Subtract this row from all others, to produce RRE form
    return b
