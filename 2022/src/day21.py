from utils import get_input


def main(text: str):
    lines = text.split('\n')

    ops = {}
    values = {}
    for line in lines:
        key, vals = line.split(': ')
        if vals.isnumeric():
            values[key] = int(vals)
        else:
            print(key, vals)
            ops[key] = vals.split(' ')
    print(ops)

    del values['humn']
    ops['root'] = ops['root'][0], '-', ops['root'][2]
    initial_values = values
    start = 3699945358000
    for v in range(start, start + 1000):
        values = dict(initial_values)
        values['humn'] = v
        while True:
            modified = False
            for ret, (lhs, op, rhs) in ops.items():
                if ret not in values and lhs in values and rhs in values:
                    values[ret] = eval('%d %s %d' % (values[lhs], op, values[rhs]))
                    modified = True
            if not modified:
                #print(len(lines) - len(values), values)

                #print(ops['root'])
                #print(expand('root', ops, values).replace('humn', 'x').replace(' ', '').replace('x', 'B'))

                break
            if 'root' in values:
                print(values['root'], v)
                if values['root'] == 0:
                    return
                break

def expand(term, ops, vals):
    if term in vals:
        return vals[term]
    if term in ops:
        lhs, op, rhs = ops[term]
        if lhs in vals and rhs in vals:
            return eval('%d %s %d' % (vals[lhs], op, rhs[vals]))
        lhs = expand(lhs, ops, vals)
        rhs = expand(rhs, ops, vals)
        if isinstance(lhs, int) and isinstance(rhs, int):
            return eval('%d %s %d' % (lhs, op, rhs))
        lhs = str(lhs)
        rhs = str(rhs)
        return '(%s %s %s)' % (lhs, op, rhs)
    if term == 'humn':
        return 'x'
    print('wut', term)



if __name__ == '__main__':
    main(get_input(21))
