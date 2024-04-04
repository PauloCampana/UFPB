import pandas
import numpy
import scipy
import typing

# Y = XB + E
# R = Y - XB
# P = XB
class Rekekssion:
    def __init__(
        self,
        n, p,
        Y, X, C, B, R, P,
    ):
        self.n = n # nº rows
        self.p = p # nº columns in X
        self.Y = Y # response
        self.X = X # variables
        self.C = C # cross product XTX
        self.B = B # coefficients
        self.R = R # residual
        self.P = P # prediction

    def __repr__(self):
        return f"""
{self.n}
{self.p}
{self.Y}
{self.X}
{self.C}
{self.B}
{self.R}
{self.P}
"""

    def _sse(self):
        return numpy.sum(numpy.square(self.R))

    def _mse(self):
        return self._sse() / (self.n - self.p)

    def _ssr(self):
        mean = numpy.mean(self.Y)
        return numpy.sum(numpy.square(self.P - mean))

    def _msr(self):
        return self._ssr() / (self.p - 1)

    def _sst(self):
        mean = numpy.mean(self.Y)
        return numpy.sum(numpy.square(self.Y - mean))

    def _mst(self):
        return self._sst() / (self.n - 1)

    def _mae(self):
        return numpy.mean(numpy.abs(self.R))

    def _rmse(self):
        return numpy.sqrt(self._mse())

    def _rsq(self):
        return 1 - self._sse() / self._sst()

    def _rsq_adj(self):
        return 1 - self._mse() / self._mst()

    def _beta_var(self):
        return self._mse() * numpy.diag(self.C)

    def _tstat(self):
        return self.B / numpy.sqrt(self._beta_var())

    def _tpval(self):
        return scipy.stats.t.sf(numpy.abs(self._tstat()), self.n - self.p)

    def _fstat(self):
        return self._msr() / self._mse()

    def _fpval(self):
        return scipy.stats.f.sf(self._fstat(), self.p, self.n - self.p - 1)




def fit(
    data: pandas.DataFrame,
    y: str,
    x: typing.List[str],
    intercept: bool
) -> Rekekssion:
    Y = data[y].to_numpy()
    X = data[x].to_numpy()
    if intercept:
        X = numpy.insert(X, 0, 1, axis = 1)

    C = numpy.linalg.inv(X.T @ X)
    B = C @ X.T @ Y
    P = X @ B
    R = Y - P

    n = X.shape[0]
    p = X.shape[1]
    return Rekekssion(n, p, Y, X, C, B, R, P)

def summary(model: Rekekssion):
    pass

def plot(model: Rekekssion):
    pass




data = pandas.read_csv("concrete.csv")
model = fit(
    data = data,
    y = "compressive_strength",
    x = ["cement", "water", "age"],
    intercept = False,
)
print(model)
print(model._mae())
print(model._rmse())
print(model._rsq())
print(model._rsq_adj())

# IC para os betas
# residuos:
#   padronizado, studentizado
#   detectar: outlier, influência, alavanca,
# predict com dados novos
# gráficos:
#   qqplot, densidade, y vs yhat, r vs yhat, r vs xs, r vs indice
# dados inclusos
