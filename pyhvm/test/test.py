from pyhvm import hvm

@hvm
def test():
    a = [(x, y) for x in range(5) for y in range(6)]
    b = 1 + 2j;
    return a & b

@hvm
def syntax(x):
    while x + 3 > 5:
        if x:
            def helper():
                return 5
            return helper(x + 6)
        else:
            x += 1
