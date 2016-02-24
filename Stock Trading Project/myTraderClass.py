from nlib import *
import random
from matplotlib import *
from datetime import date

class myTrader(object):

    # initialize
    '''
    initializes trader class for given stock
    Input:  str(symbol)     - stock symbol
            float(delta)    - changeable variable used in trading algorithms
                              to affect the margins of buy/sell triggers
            float(amount)   - starting money amount
            int(year)       - chosen year to examine

    Output: None
    '''
    def __init__(self, symbol = 'MSFT', delta = 0.0, amount = 10000.0, year = 2006):
        self.symbol = symbol
        self.bank_balance = amount
        self.number_of_shares = 0
        self.delta = float(delta)
        self.get_data(self.symbol, year)
        self.buy = 0
        self.sell = 0
        self.year = year
        #self.EMACanvas()

    '''
    returns and resets the counters that track number of buys and sells for current myTrader class
    Input:  None
    Output: int(b), int(s) - integer counts of buy, sell numbers 
    '''
    def pop_count(self):
        b = self.buy
        s = self.sell
        self.buy = 0
        self.sell = 0
        return b,s

    '''
    resets the counters that track number of buys and sells for current myTrader class
    Input:  None
    Output: None
    '''
    def clear_count(self):
        self.buy = 0
        self.sell = 0

    '''
    Loads stock data for given symbol and year
    Input:  str(symbol) - stock symbol
            int(year)   - chosen year to examine
                     loads last data from last 250 trading days if year = 0 
    Output: None
    '''
    def get_data(self, symbol = 'MSFT', year = 0):
        storage = PersistentDictionary(str(year) + 'storage.db')
        if year == 0:
            if symbol in storage:
                h = storage[symbol]
            else:
                h = YStock(symbol).historical()
                storage[symbol] = h
            self.h = h[-250:]
        else:
            if symbol in storage:
                h = storage[symbol]
            else:
                h = YStock(symbol).historical(start=date(year,1,1),stop=date(year,12,31))
                storage[symbol] = h
            self.h = h
        #self.hh= h[-2*h:]

    '''
    returns list of data from time t, for length l (in days)
    Input:  integer(t)      - start day
            integer(L)      - how many days to examine
    Output: list(h[t:t+L])  - historical data from time t to time t+L
    '''
    def get_time_window(self,t=0,L=7):
        return self.h[t:t+L]

    # unused
    def print_time_window(self,t=0,L=7):
        time_window = self.get_time_window(t,L)
        for day in time_window:
            print day['date'].isoformat(), day['adjusted_close']

    '''
    returns price of stock at beginning/end of time period as well as average price over the year
    created to give a little information on how stock performed over year
    Input:  None
    Output: float(start_year)   - price of stock at start of time period
            float(end_year)     - price of stock at end of time period
            float(avg_year)     - average price of stock over time period
    '''
    def get_yearly_data(self):
        start_year = self.h[0]['adjusted_close']
        end_year = self.h[-1]['adjusted_close']
        avg_year = 0.0

        for x,day in enumerate(self.h):
            avg_year += day['adjusted_close']

        return start_year, end_year, avg_year/len(self.h)

    # creates a plot titled str(title), with tuple(points) plotted,
    # takes another tuple for legend
    '''
    uses nlib Canvas class to plot multiple seperate line plots of data
    Input:  str(title)      - title to put on graph
            list( list( tuple(int(t), float(data))) )
                            - list of line plots
                            - line plots consist of a tuple of data points
    Output: Saves plot to file 'title.png'
    '''
    def toCanvas(self, title, points, legend):
        c = Canvas(title, xlab = 'trading day',ylab = 'adjusted close')
        colors = ['blue','red','green','yellow','cyan','magenta']
        for n,p in enumerate(points):
            c.plot(points[n], legend = legend[n], color = colors[n%6])

        c.save(title + '.png')

    '''
    gives the L-Day Simple Moving Average for day t
    Input:  int(t)  - day t
            int(L)  - Number of days to include in the Simple Moving Average calculation
    Output: float   - returns simple moving average price of the stock on day t
    '''
    # simple moving average
    def SMA(self, t, L):
        window = self.get_time_window(t-L,L)
        avg = 0.0;
        for t,day in enumerate(window):
            avg += day['adjusted_close']

        return avg/L

    '''
    This function determines whether the trader should buy/sell at day t based on whether the current
    day's stock price is greater or smaller than the Simple Moving Avg +- self.delta
    Input:  int(t)  - day t
            int(L)  - Number of days to include in the Simple Moving Average calculation
    Output: returns str 'buy' if today_price > sma + self.delta or
                    str 'sell' if today_price < sma - self.delta or
    '''
    # SMA Strategy
    def SMAStrategy(self, t, L):
        today_price = self.h[t]['adjusted_close']
        sma = self.SMA(t,L)
        if today_price > sma + self.delta and today_price <= self.bank_balance:
            return 'buy'
        elif today_price < sma - self.delta and self.number_of_shares > 0:
            return 'sell'

    '''
    runs a L-Day Simple Moving Average simulation over current myTrader's historical data
    Input:  int(L)              - Number of days to include in the Simple Moving Average calculation
    Output: float(net_worth)    - returns the net_worth of bank account + stocks after simulation
    '''
    def SMASimulate(self, L = 7):
        for t in range(L, len(self.h)):
            indicator = self.SMAStrategy(t,L);
            today_price = self.h[t]['adjusted_close']
            if indicator == 'buy':
                new_shares = int(self.bank_balance/today_price)
                self.number_of_shares += new_shares
                self.bank_balance -= new_shares*today_price
                self.buy += 1
            elif indicator == 'sell':
                self.bank_balance += self.number_of_shares*today_price
                self.number_of_shares = 0
                self.sell += 1
            net_worth = self.bank_balance + self.number_of_shares*today_price
            
##            print "%s\t$%.2f\t%i\t$%.2f" % (
##                self.h[t]['date'].isoformat(),
##                net_worth,
##                self.number_of_shares,
##                self.bank_balance)
            
        return net_worth

    #Exponential Moving Average
    '''
    runs a L-Day Exponential Moving Average simulation over current myTrader's historical data
        'buys'  shares if the ratio between the current day's ema divided by the previous day's ema
                is greater than 1 + self.delta
        'sells' shares if the ratio between the current day's ema divided by the previous day's ema
                is less than 1 - self.delta
        
    Input:  int(L)              - Number of days to include in the Exponential Moving Average calculation
    Output: float(net_worth)    - returns the net_worth of bank account + stocks after simulation
    '''
    def EMAStrategy(self, L = 10):
        previous_ema = self.SMA(L, L)
        multiplier = 2.0 / (L - 1.0)

        for t in range(L,len(self.h)):
            today_price = self.h[t]['adjusted_close']
            current_ema = (today_price - previous_ema) * multiplier + previous_ema
            ratio = abs(current_ema / previous_ema)

            #if current_ema > previous_ema + self.delta:
            if ratio > 1.0 + self.delta and today_price <= self.bank_balance:
                new_shares = int(self.bank_balance/today_price)
                self.number_of_shares += new_shares
                self.bank_balance -= new_shares*today_price
                self.buy += 1
                
            #elif current_ema < previous_ema - self.delta:
            elif ratio < 1.0 - self.delta and self.number_of_shares > 0:
                self.bank_balance += self.number_of_shares*today_price
                self.number_of_shares = 0
                self.sell += 1

            net_worth = self.bank_balance + self.number_of_shares*today_price
            '''
            print "%s\t$%.2f\t%i\t$%.2f" % (
                self.h[t]['date'].isoformat(),
                net_worth,
                self.number_of_shares,
                self.bank_balance)
            '''
        return net_worth
    
    '''
    returns an Array[t], Array[t] = the L-Period EMA for day t, first L entries are the L-Period SMA
    '''
    def EMA(self, L=10):
        l = len(self.h)
        ema = [self.SMA(L,L)] * l
        multiplier = 2.0 / (L - 1.0)
        
        for t in range(L, l):
            today_price = self.h[t]['adjusted_close']
            ema[t] = (today_price - ema[t-1]) * multiplier + ema[t-1]

        return ema

    def EMACanvas1(self, s = 10):
        points = [(x,y['adjusted_close']) for (x,y) in enumerate(self.h)]
        points2 = [(x,y) for (x,y) in enumerate(self.EMA(L=s))]
        return self.toCanvas(str(self.year) + self.symbol + '  10-EMA',
                             (points,points2),
                             (self.symbol,str(s) + '-EMA'))
    
    '''
    simple helper function make plotting EMA data easier
    Input: int(s)   - Short EMA length, default = 10 day EMA
           int(l)   - Long EMA length, default = 50 day EMA
    Output: Saves plot to file 'title.png'
    '''
    def EMACanvas(self, s = 10, l = 50):
        points = [(x,y['adjusted_close']) for (x,y) in enumerate(self.h)]
        points2 = [(x,y) for (x,y) in enumerate(self.EMA(L=s))]
        points3 = [(x,y) for (x,y) in enumerate(self.EMA(L=l))]
        return self.toCanvas(str(self.year) + self.symbol + ' Compare EMA',
                             (points,points2,points3),
                             (self.symbol,str(s) + '-EMA',
                              str(l) + '-EMA'))

    '''
    simulates over current myTrader class' historical data, using my dualEMA Strategy
    Input: int(s)   - Short EMA length, default = 10 day EMA
           int(l)   - Long EMA length, default = 50 day EMA
    Output: float(net_worth)
                    - returns the net_worth of bank account + stocks after simulation
    '''
    def dualEMAStrategy(self, s=10, l=50):
        long_ema = self.EMA(L=l)
        short_ema = self.EMA(L=s)
        prev_diff = abs(short_ema[s] - long_ema[s])
        
        for t in range(s, len(self.h)):
            today_price = self.h[t]['adjusted_close']
            current_short = short_ema[t]
            current_long = long_ema[t]
            current_diff = abs(current_short - current_long)
            
            if current_short > current_long:
                #if current_diff > prev_diff and today_price <= self.bank_balance:
                if (current_diff / prev_diff > 1.0 + self.delta) and today_price <= self.bank_balance:
                    '''buy'''
                    new_shares = int(self.bank_balance/today_price)
                    self.number_of_shares += new_shares
                    self.bank_balance -= new_shares*today_price
                    self.buy += 1

                #elif current_diff < prev_diff and self.number_of_shares > 0:
                elif (current_diff / prev_diff < 1.0 - self.delta) and self.number_of_shares > 0:
                    '''sell'''
                    self.bank_balance += self.number_of_shares*today_price
                    self.number_of_shares = 0
                    self.sell += 1
                    
            elif current_short < current_long:
                #if current_diff < prev_diff and today_price <= self.bank_balance:
                if (current_diff / prev_diff > 1.0 + self.delta) and today_price <= self.bank_balance:
                    '''buy'''
                    new_shares = int(self.bank_balance/today_price)
                    self.number_of_shares += new_shares
                    self.bank_balance -= new_shares*today_price
                    self.buy += 1
                    
                #elif current_diff > prev_diff and self.number_of_shares > 0:
                elif (current_diff / prev_diff < 1.0 - self.delta) and self.number_of_shares > 0:
                    '''sell'''
                    self.bank_balance += self.number_of_shares*today_price
                    self.number_of_shares = 0
                    self.sell += 1

##            else:
##                self.bank_balance += self.number_of_shares*today_price
##                self.number_of_shares = 0
##                self.sell += 1
##                print 's', self.sell

            net_worth = self.bank_balance + self.number_of_shares*today_price
            prev_diff = current_diff
        return net_worth

                        
##            print "%s\t$%.2f\t%i\t$%.2f" % (
##                self.h[t]['date'].isoformat(),
##                net_worth,
##                self.number_of_shares,
##                self.bank_balance)

    def bb_model(self, t, L = 20, delta = 2.0):
        '''
        calculates the bollinger band pts @ day t +- delta*(calculated Standard Deviation)
        returns a tuple (lower band, middle band, upper band)
        where the middle band is the L-Day SMA(t), L = 20 default
        Note: as of now variance uses (t0 - SMA(tn))^2 + (t1 - SMA(tn))^2 + ... + (tn - SMA(tn))^2
                          rather than (t0 - SMA(t0))^2 + (t1 - SMA(t1))^2 + ... + (tn - SMA(tn))^2
        '''
        sma = self.SMA(t, L=20)
        today_price = self.h[t]['adjusted_close']
        std_dev = 0.0

        for ti in range(t-L, L):
            std_dev += (self.h[ti]['adjusted_close'] - sma)**2

        std_dev = sqrt(std_dev/L)

        return sma - delta*std_dev, sma, sma + delta*std_dev

    '''
    This function determines whether the trader should buy/sell at day t based on whether the current
    day's stock price is greater/less than 20 Day SMA +- the current day's standard deviation*self.delta
    Input:  int(L)  - Number of days to include in the Simple Moving Average calculation
    Output: returns str 'buy' if today_price < sma + standard deviation*self.delta or
                    str 'sell' if today_price < sma - standard deviation*self.delta
    '''
    def bb_strategy(self, t, L = 20):
        today_price = self.h[t]['adjusted_close']
        low_band, mid_band, upp_band = self.bb_model(t)
        if today_price  < low_band and today_price <= self.bank_balance:
            return 'buy'
        elif today_price  > upp_band and self.number_of_shares > 0:
            return 'sell'

    '''
    runs a Bollinger Band simulation over current myTrader's historical data, using bb_strategy
    Input:  int(L)              - Number of days to include in the Simple Moving Average calculation
    Output: float(net_worth)    - returns the net_worth of bank account + stocks after simulation
    '''
    def bb_simulate(self, L = 20):
        for t in range(L, len(self.h)):
            indicator = self.bb_strategy(t,L);
            today_price = self.h[t]['adjusted_close']
            if indicator == 'buy':
                new_shares = int(self.bank_balance/today_price)
                self.number_of_shares += new_shares
                self.bank_balance -= new_shares*today_price
                self.buy += 1
            elif indicator == 'sell':
                self.bank_balance += self.number_of_shares*today_price
                self.number_of_shares = 0
                self.sell += 1
            net_worth = self.bank_balance + self.number_of_shares*today_price
        return net_worth

    '''
    simple recursive function that determines the L-Day Estimated Moving Average for day t
    Input:  int(t)  - day t
            int(L)  - Number of days to include in the Estimated Moving Average calculation
    Output: float   - L-Day EMA at day t
    '''
    def recursive_EMA(self, t, L=10):
        if t < L:
            return self.SMA(L,L)
        prev_ema = self.recursive_EMA(t-1, L)
        multiplier = 2.0 / (L - 1.0)
        today_price = self.h[t]['adjusted_close']
        
        return (today_price - prev_ema) * multiplier + prev_ema

    '''
    function that finds the difference between the l-day SMA - s-Day SMA at day t
    Input: int(t)   - day t
           int(s)   - Short SMA length, default = 12 day SMA
           int(l)   - Long SMA length, default = 26 day SMA
    Output: Float   - difference between the l-day SMA - s-Day SMA at day t
    '''
    def macd_pt(self, t, s = 12, l = 26):
        long_ema = self.recursive_EMA(t, l)
        short_ema = self.recursive_EMA(t, s)
        return short_ema - long_ema

    def macd_model(self, t, s = 12, l = 26, L = 9):
        window = self.get_time_window(t-L, L)
        data = []
        for t,day in enumerate(window):
            data.append((t, self.macd_pt(t, s, l),1))
        c, chi2, f = fit_least_squares(data,QUADRATIC)
        return f(t)
    
    def macd_strategy(self, t, s = 12, l = 26, L = 9):
        today_price = self.h[t]['adjusted_close']
        prediction = self.macd_model(t,L)
        prev_macd = self.macd_pt(t-1, s, l)

        #if prev_macd < 0 and prediction >= 0 and today_price <= self.bank_balance:
        if prediction > prev_macd  and today_price <= self.bank_balance:
            return 'buy'
            
        #elif prev_macd > 0 and prediction <= 0 and self.number_of_shares > 0:
        elif prediction < prev_macd  and self.number_of_shares > 0:
            return 'sell'

    def macd_simulate(self, s = 12, l = 26, L = 9):
        net_worth = 0.0
        for t in range(l, len(self.h)):
            indicator = self.macd_strategy(t,L);
            today_price = self.h[t]['adjusted_close']
            if indicator == 'buy':
                new_shares = int(self.bank_balance/today_price)
                self.number_of_shares += new_shares
                self.bank_balance -= new_shares*today_price
                self.buy += 1
            elif indicator == 'sell':
                self.bank_balance += self.number_of_shares*today_price
                self.number_of_shares = 0
                self.sell += 1
            net_worth = self.bank_balance + self.number_of_shares*today_price
            '''   
            print "%s\t$%.2f\t%i\t$%.2f" % (
                self.h[t]['date'].isoformat(),
                net_worth,
                self.number_of_shares,
                self.bank_balance),(self.buy,self.sell), today_price
            '''
        return net_worth

    def sma_macd_model(self, t, s = 12, l = 26, L = 9):
        window = self.get_time_window(t-L, L)
        data = []
        for t,day in enumerate(window):
            data.append( (t, self.sma_macd_pt(t, s, l, L)) )           
        c, chi2, f = fit_least_squares(data,QUADRATIC)
        return f(t)

    def sma_macd_pt(self, t, s = 12, l = 26, L = 9):
        pt = 0.0
        for ti in range(t-L+1, L+1):
            pt += self.macd_pt(ti, s, l)
        return pt/L
    
    def sma_macd_strategy(self, t, s = 12, l = 26, L = 9):
        today_price = self.h[t]['adjusted_close']
        macd_predict = self.macd_model(t, s, l, L)
        sma_macd_predict = self.sma_macd_model(t, s, l, L)

        if sma_macd_predict > macd_predict and today_price <= self.bank_balance:
            return 'buy'
        elif sma_macd_predict < macd_predict  and self.number_of_shares > 0:
            return 'sell'
        
            
            
