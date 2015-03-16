#! /usr/bin/env python

"""
    A simplified backjack simulator that calculates expected wins for a new srategy called "benchmark"
    creator: Andrew Leister
"""
import itertools
import sys

benchmark = 1000
multiplier = 1
init_balance = 1000

def main():
    
    print "starting simulation"
    #sims = 8
    sims = int(sys.argv[1])
    bj_prob = .04745
    win_prob = .4242 - bj_prob
    tie_prob = .0848
    lose_prob = .4910
    
    global benchmark
    global multiplier
    
    
    weight_profit = 0
    #benchmark = profit
    #bet = 10

    for set in itertools.product([0,1,3,2], repeat = sims):
        print "set = {0}" .format(set)
        profit = 0.0
        bet = 10.0
        multiplier = 1
        weight = 1
        for hand in set:
            if hand == 3:
                result = "bj"
                profit += 1.5 * bet
                weight *= bj_prob
            elif hand == 2:
                result = "tie"
                weight *= tie_prob
            elif hand == 1:
                result = "won"
                profit += bet
                weight *= win_prob
            else:
                result = "lost"
                profit -= bet
                weight *= lose_prob
                if init_balance + profit <= 0:
                    break
            print "You {0} {1} dollars" .format(result, bet)
            bet = calcbet(result, profit)
    
        print "profit from this set = {0}" .format(profit)
        weight_profit += profit * weight
        print "contribution to weighted profit: {0}" .format(profit * weight)
        multiplier = 1
        benchmark = 1000
        print "current profit (with weights): {0}" .format(weight_profit)

    print "estimated profit (with weights): {0}" .format(weight_profit)

def calcbet(result, profit):
    #determine benchmark
    global benchmark
    balance = init_balance + profit
    if balance - benchmark > 1000:
        benchmark += 1000
    elif benchmark - balance > 1000:
        benchmark -= 1000

    #determine base
    if abs(balance - benchmark) > 600:
        base = 40
    elif abs(balance - benchmark) > 300:
        base = 30
    elif abs(balance - benchmark) > 100:
        base = 20
    else:
        base = 10

    #determine multiplier
    global multiplier
    if result == "won" or result == "bj":
        multiplier += 0.5
    elif result == "lost":
        multiplier = 1

    #print "     balance: {0}, profit: {1}, benchmark: {2}, base: {3}, result: {4}, multiplier: {5}" .format(balance, profit, benchmark, base, result, multiplier)

    newbet = base * multiplier
    if newbet > balance:
        newbet = balance

    return newbet

if __name__ == "__main__": main()
