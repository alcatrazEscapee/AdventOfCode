# Day 25: Full of Hot Air
# Rank 81 / 73

from utils import get_input


def main(text: str):
    # This is an overcomplicated base system with a twist that it uses negative values

    total = 0
    keys = {'-': -1, '=': -2, '0': 0, '1': 1, '2': 2}
    values = {v: k for k, v in keys.items()}

    # Find the total of all SNAFU numbers
    for line in text.split('\n'):
        value, base = 0, 1
        for c in line[::-1]:
            value += keys[c] * base
            base *= 5
        total += value

    snafu = ''
    carry = False  # Negative bases (or bases with negative values) carry over by one
    while total > 0:
        value = total % 5
        if carry:
            value += 1
            carry = False
        if value > 2:
            carry = True
            value -= 5
        snafu += values[value]
        total //= 5

    print('Part 1:', snafu[::-1])


if __name__ == '__main__':
    main(get_input(25))
