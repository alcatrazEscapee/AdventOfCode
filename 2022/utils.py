

def get_input(day: int) -> str:
    with open('./inputs/day%02d.txt' % day, 'r', encoding='utf-8') as f:
        return f.read()