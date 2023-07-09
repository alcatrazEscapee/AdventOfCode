from utils import get_input
from typing import NamedTuple, Union, Dict

OPS = {'+': lambda a, b: a + b, '-': lambda a, b: a - b, '*': lambda a, b: a * b, '/': lambda a, b: a // b}
INV_OPS = {'+': '-', '-': '+', '*': '/', '/': '*'}


class Term(NamedTuple):
    lhs: Union['Term', 'Value']
    op: str
    rhs: Union['Term', 'Value']

    def const(self) -> bool: return self.lhs.const() and self.rhs.const()
    def value(self) -> int: return OPS[self.op](self.lhs.value(), self.rhs.value())
    def terminal(self) -> bool: return isinstance(self.lhs, Value) and isinstance(self.rhs, Value)
    def commutes(self) -> bool: return self.op in '+*='

class Value(NamedTuple):
    raw: int | str

    def const(self) -> bool: return isinstance(self.raw, int)
    def value(self) -> int: return self.raw


def main(text: str):
    # Parse the input
    jobs: Dict[str, Value | list] = {}
    for line in text.split('\n'):
        k, v = line.split(': ')
        if v.isnumeric():
            jobs[k] = Value(int(v))
        else:
            jobs[k] = v.split(' ')

    # Part 1: the entire term is const, so we just recursively evaluate it
    print('Part 1:', build('root', jobs).value())

    # Modify the input for part 2
    jobs['humn'] = Value('x')
    jobs['root'][1] = '='

    # Use our special solver which, assuming there's exactly one instance of 'humn', un-does each operation until it arrives at a terminal state
    print('Part 2:', reduce(build('root', jobs)))

def build(key: str, jobs: Dict[str, Term | Value]) -> Term | Value:
    job = jobs[key]
    if isinstance(job, Value):
        return job
    lhs, op, rhs = job
    return Term(build(lhs, jobs), op, build(rhs, jobs))

def reduce(term: Term) -> int:
    if term.terminal():
        return term.lhs.value() if term.lhs.const() else term.rhs.value()
    if not term.lhs.const() and term.commutes():
        return reduce(Term(term.rhs, term.op, term.lhs))
    if term.rhs.commutes():
        lhs, rhs = (term.rhs.lhs, term.rhs.rhs) if term.rhs.rhs.const() else (term.rhs.rhs, term.rhs.lhs)
        return reduce(Term(lhs, term.op, construct(term.lhs, INV_OPS[term.rhs.op], rhs)))
    if term.rhs.lhs.const():
        return reduce(Term(term.rhs.rhs, term.op, construct(term.rhs.lhs, term.rhs.op, term.lhs)))
    else:
        return reduce(Term(term.rhs.lhs, term.op, construct(term.lhs, INV_OPS[term.rhs.op], term.rhs.rhs)))

def construct(lhs: Value, op: str, rhs: Value) -> Value:
    return Value(OPS[op](lhs.value(), rhs.value()))


if __name__ == '__main__':
    main(get_input(21))
