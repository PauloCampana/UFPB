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
        y, x,
        n, p,
        Y, X, C, B, R, P,
    ):
        self.y = y # y name
        self.x = x # x names
        self.n = n # nº rows
        self.p = p # nº columns in X
        self.Y = Y # response
        self.X = X # variables
        self.C = C # cross product XTX
        self.B = B # coefficients
        self.R = R # residual
        self.P = P # prediction

    def __repr__(self):
        eq = f"{self.y} = {self.B[0]:.3} {self.x[0]} "
        for i in range(len(self.B)):
            if i == 0:
                continue
            if self.B[i] >= 0:
                eq += f"+ {abs(self.B[i]):.3} {self.x[i]} "
            else:
                eq += f"- {abs(self.B[i]):.3} {self.x[i]} "
        return eq

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

    def _log_lik(self):
        sigma2 = self._mse()
        l = self.n * numpy.log(2 * numpy.pi * sigma2) + self.n - self.p
        return -0.5 * l

    def _aic(self):
        return 2 * (self.p + 1 - self._log_lik())

    def _bic(self):
        return numpy.log(self.n) * (self.p + 1) - 2 * self._log_lik()

    def _beta_var(self):
        return self._mse() * numpy.diag(self.C)

    def _beta_ic(self, alpha = 0.05):
        t = scipy.stats.t.ppf(1 - alpha / 2, self.n - self.p)
        margin = t * numpy.sqrt(self._beta_var())
        return (self.B - margin, self.B + margin)

    def _tstat(self):
        return self.B / numpy.sqrt(self._beta_var())

    def _tpval(self):
        return scipy.stats.t.sf(numpy.abs(self._tstat()), self.n - self.p)

    def _fstat(self):
        return self._msr() / self._mse()

    def _fpval(self):
        return scipy.stats.f.sf(self._fstat(), self.p, self.n - self.p - 1)

    def summary(self, alpha = 0.05):
        print("formula:")
        print(f"\t{self.__repr__()}")
        print("coefficients:")
        print(
            f"\t{'':<10}",
            f"{f'IC {50 * alpha}%':>10}",
            f"{'estimate':>10}",
            f"{f'IC {100 - 50 * alpha}%':>10}",
            f"{'statistic':>10}",
            f"{'p-value':>10}",
            sep = "",
        )
        ic = self._beta_ic(alpha)
        tstat = self._tstat()
        tpval = self._tpval()
        for i in range(len(self.B)):
            line = f"\t{self.x[i]:<10}"
            line += f"{ic[0][i]:>10.3f}"
            line += f"{self.B[i]:>10.3f}"
            line += f"{ic[1][i]:>10.3f}"
            line += f"{tstat[i]:>10.3f}"
            line += f"{tpval[i]:>10.3g}"
            print(line)
        print("metrics:")
        print(f"\tR squared              {self._rsq():>10.3f}")
        print(f"\tadjusted R squared     {self._rsq_adj():>10.3f}")
        print(f"\troot mean squared error{self._rmse():>10.3f}")
        print(f"\tmean absolute error    {self._mae():>10.3f}")
        print(f"\tF test p-value         {self._fpval():>10.3g}")
        print(f"\tAkaike criteria        {self._aic():>10.3f}")
        print(f"\tBayesian criteria      {self._bic():>10.3f}")
        pass

    def plot(self):
        pass





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
        x.insert(0, "")

    C = numpy.linalg.inv(X.T @ X)
    B = C @ X.T @ Y
    P = X @ B
    R = Y - P

    n = X.shape[0]
    p = X.shape[1]
    return Rekekssion(y, x, n, p, Y, X, C, B, R, P)










# IC para os betas
# residuos:
#   padronizado, studentizado
#   detectar: outlier, influência, alavanca,
# predict com dados novos
# gráficos:
#   qqplot, densidade, y vs yhat, r vs yhat, r vs xs, r vs indice
# dados inclusos
