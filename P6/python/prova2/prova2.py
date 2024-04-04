import math
import numpy
import scipy

# Questão 1 ------------------------------------------------
def f(l):
    return [
        x * x if (x % 2 == 0) # coisa feia o if vir depois
        else x * x * x
        for x in l
    ]

a = [1, 2,  3,  4,   5,  6,   7,  8,   9,  10]
b = [1, 4, 27, 16, 125, 36, 343, 64, 729, 100]
assert f(a) == b

# Questão 2. -----------------------------------------------
def py_apply(x, func, margin = 0):
    return numpy.apply_along_axis(func, margin, x)

a = numpy.array([
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
])
b = numpy.array([12, 15, 18])
c = numpy.array([ 6, 15, 24])
assert numpy.array_equal(py_apply(a, sum, 0), b)
assert numpy.array_equal(py_apply(a, sum, 1), c)

def py_sapply(x, func):
    if type(x) == dict:
        return {k: func(v) for k, v in x.items()}
    return [func(z) for z in x]

a = [1, 2, 3, 4, 5, 6, 7,  8,  9, 10]
b = [3, 2, 3, 6, 7, 6, 7, 10, 11, 10]
assert py_sapply(a, lambda x: x | 2) == b
a = {"a": 1, "b": 2, "c": 3}
b = {"a": 3, "b": 2, "c": 3}
assert py_sapply(a, lambda x: x | 2) == b

# Questão 3. -----------------------------------------------
def integration(a, b):
    def decorator(f):
        def wrap():
            return scipy.integrate.quad(f, a, b)[0]
        return wrap
    return decorator

@integration(0, 1)
def f3(x):
    return math.log1p(x) / (1 + x * x)

b = math.pi * math.log(2) / 8
assert math.isclose(f3(), b)

# Questão 4. -----------------------------------------------
shape = 2.71
scale = 3.29
sample = scipy.stats.weibull_min.rvs(
    shape,
    scale = scale,
    size = 1000,
)

def log_ver(xs, shape, scale):
    logpdf = scipy.stats.weibull_min.logpdf(xs, shape, scale = scale)
    return -numpy.sum(logpdf)

result = scipy.optimize.minimize(
    lambda par: log_ver(sample, par[0], par[1]),
    (1, 1),
)

assert math.isclose(result.x[0], shape, rel_tol = 0.1)
assert math.isclose(result.x[1], scale, rel_tol = 0.1)

# Questão 5. -----------------------------------------------
def is_prime(prime):
    check = 2
    while check <= math.sqrt(prime):
        if prime % check == 0:
            return False
        check += 1
    return True

def prime_generator():
    prime = 2
    while True:
        if is_prime(prime):
            yield prime
        prime += 1

p = prime_generator()
assert next(p) == 2
assert next(p) == 3
assert next(p) == 5
assert next(p) == 7
assert next(p) == 11
