def z(f):
    return (lambda x: f(lambda y: x(x)(y))) (lambda x: f(lambda y: x(x)(y)))

def c(g):
    return (lambda n: 1 if n == 0 else n * g(n - 1))

print(z(c)(5))
