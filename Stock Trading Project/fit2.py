from nlib import *
import random
from math import *

stock = YStock('AAPL')
h = stock.historical()[-200:]
points = [[float(t),
           day['adjusted_close'],
           day['adjusted_high']-day['adjusted_low']] 
          for t,day in enumerate(h)]

fs = [lambda t: 1,
      lambda t: t,
      lambda t: sin(t/200)]

x, chi2, f = fit_least_squares(points,fs)
print x
print chi2
e_points = [[ti,f(ti)] for (ti,oi,doi) in points]
e_points.append((400,f(400)))
Canvas().errorbar(points).plot(e_points,color='red').save('experiment2.png')
