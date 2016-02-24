from myTraderClass.py import *

symbol = ['MSFT', 'AAPL', 'F', 'TAP', 'CSCO', 'TWX', 'JPM', 'GRMN','SHLD','YHOO']
year = [2006, 2010, 2014]
#symbol = ['MSFT', 'AAPL', 'F', 'TAP', 'KODK', 'GM', 'TSLA', 'GRMN','SHLD','YHOO']

amount = 10000.0

def f(x):
    trader = myTrader(symbol, x, amount)
    net_worth = trader.EMAStrategy(L=5)
    r = (net_worth/amount-1)*100
    #r = net_worth
    print x,r, '\t', net_worth
    return -r

def g(x):
    trader = myTrader(symbol, x, amount)
    net_worth = trader.simulate()
    r = (net_worth/amount-1)*100
    #r = net_worth
    print x,r, '\t', net_worth
    return -r

def getOptimizeEMA(symbol,year, a = -20.0, b = 10.0):
    def ema(x):
        trader = myTrader(symbol, x,year=year)
        net_worth = trader.EMAStrategy()
        r = (net_worth/amount-1)*100
        #r = net_worth
        #print x,r, '\t', net_worth
        return -r

    return optimize_golden_search(ema, float(a), float(b))

def getOptimizeSMA(symbol,year, a = -1.0, b = 1.0):
    def sma(x):
        trader = myTrader(symbol, x,year=year)
        net_worth = trader.SMASimulate()
        r = (net_worth/amount-1)*100
        #r = net_worth
        #print x,r, '\t', net_worth
        return -r

    return optimize_golden_search(sma, float(a), float(b))

def getOptimizeDualEMA(symbol,year, a = -1.0, b = 1.0):
    def dual(x):
        trader = myTrader(symbol, x,year=year)
        net_worth = trader.dualEMAStrategy()
        r = (net_worth/amount-1)*100
        #r = net_worth
        #print x,r, '\t', net_worth
        return -r

    return optimize_golden_search(dual, float(a), float(b))


def dualEMA(sym,year):
    x = getOptimizeDualEMA(sym,year)
    trader1 = myTrader(sym, x, amount,year=year)
    dual = trader1.dualEMAStrategy(10, 50)
    print sym,'dualEMA', dual, '\t', x, trader1.pop_count()
    return x

def singEMA(sym,year):
    x = getOptimizeEMA(sym,year)
    trader2 = myTrader(sym, x, amount,year=year)
    single = trader2.EMAStrategy()
    print sym,'singEMA', single, '\t', x, trader2.pop_count()
    return x

        
def sma(sym,year):
    x = getOptimizeSMA(sym,year)
    trader3 = myTrader(sym, x, amount,year=year)
    sma = trader3.SMASimulate()
    print sym,'my SMA ', sma, '\t', x, trader3.pop_count()
    return x

        
def bb(sym,year):
    #x = getOptimizeSMA(sym,year)
    trader4 = myTrader(sym, amount,year=year)
    bb = trader4.bb_simulate()
    print sym,'BollBnd', bb, '\t', 'N/A           ', trader4.pop_count()
    #return x


def macd(sym,year):
    #x = getOptimizeSMA(sym,year)
    trader5 = myTrader(sym,year=year)
    macd = trader5.macd_simulate()
    print sym,'MovCnDv', macd, '\t', 'N/A           ', trader5.pop_count(), '\n'
    #return x

def run_sim_old():
    for y in year:
        print'*'*12, y, '*'*12
        o = [0.0]*3
        for sym in symbol:
            #print myTrader(sym,year=y).get_yearly_data()
            #o[0]+= dualEMA(sym,y)
            o[1]+= singEMA(sym,y)
            #o[2]+= sma(sym,y)
            #bb(sym,y)
            #macd(sym,y)
        print '\n'

def run_sim():
    for y in year:
        print'*'*12, y, '*'*12
        o = [0.0]*3
        for sym in symbol:
            #print myTrader(sym,year=y).get_yearly_data()
            sma(sym,y)
            singEMA(sym,y)
            dualEMA(sym,y)
            bb(sym,y)
            macd(sym,y)          
        print '\n'

# function that finds the best delta over all 10 stocks
def OptimizeDualOverall():
    def h(x):
        net_worth = 0.0
        for sym in symbol:
            net_worth += myTrader(sym,x,amount).dualEMAStrategy(10,50)

        return net_worth / len(symbol)

    return optimize_golden_search(h, -1.0, 1.0)

##x = OptimizeDualOverall()
##for sym in symbol:
##    trader1 = myTrader(sym, x, amount)
##    dual = trader1.dualEMAStrategy(10, 50)
##    print sym,'dual', dual, x, trader1.pop_count(), trader1.bank_balance
    
##symbol = symbol[-1]
##trader = myTrader(symbol)



#trader.EMACanvas()


'''
points = [(x,y['adjusted_close']) for (x,y) in enumerate(trader.h)]
points2 = [(x,y) for (x,y) in enumerate(trader.EMA())]
#trader.toCanvas(symbol + ' EMA', (points,points2), (symbol,'EMA'))
'''
