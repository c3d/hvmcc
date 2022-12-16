from .pyhvm import compile_rules, init_runtime

def init_vars(func):
    func.runtime = init_runtime()
    func.compiled_funcs = set()
    return func

@init_vars
def hvm(f):
    from inspect import getsource
    if f.__name__ not in hvm.compiled_funcs:
            #hvm.runtime = pyhvm.compile_rules(hvm.runtime, getsource(f))
        compile_rules(hvm.runtime, getsource(f))
        hvm.compiled_funcs.add(f.__name__)
    # def wrapper(*args, **kwargs):
    #     # return pyhvm.call_hvm(hvm.runtime, f.__name__, args)
    #     pass
    # return wrapper
