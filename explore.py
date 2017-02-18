from collections import Counter
import ast
TEMPS = [ast.literal_eval((x.split())[6]) for x in open('temps.txt').readlines()]
DIFFS = [x - y for (x,y) in zip(TEMPS[1:],TEMPS[0:-1])]
COUNTS = Counter(DIFFS)
COUNTS2= Counter([z for z in zip(DIFFS[1:],DIFFS[0:-1])])
def pdist(counter):
    "Make a probability distribution, given evidence from a Counter."
    N = sum(counter.values())
    return lambda x: counter[x]/N
P=pdist(COUNTS)
P2=pdist(COUNTS2)
def product(nums):
    "Multiply the numbers together.  (Like `sum`, but with multiplication.)"
    result = 1
    for x in nums:
        result *= x
    return result

def Pwords(words):
    "Probability of words, assuming each word is independent of others."
    return product(P(w) for w in words)

def cPword(word, prev):
    "Conditional probability of word, given previous word."
    bigram = (prev,word)
    if P2(bigram) > 0 and P(prev) > 0:
        return P2(bigram) / P(prev)
    else: # Average the back-off value and zero.
        return P(word) / 2

def Pwords2(words):
    "The probability of a sequence of words, using bigram data, given prev word."
    return product(1 if (i==0) else cPword(w, words[i-1] )
                   for (i, w) in enumerate(words))
import os
def tail(n=4):
    rows = subprocess.check_output(["tail","-n "+str(n), "temps.txt"]).splitlines()
    temps=[ast.literal_eval((x.split())[6].decode("utf_8")) for x in rows]
    return [x-y for (x,y) in zip(temps[1:],temps[0:-1])]

def ouch():
    subprocess.check_output(["espeak","ouch"])
import time
def monitor(last):
    return last,Pwords2(last)

def start():
def ouch():
    subprocess.check_output(["espeak","-a10","-vwhisper","nice"])

def start(treshold=0.01):
    last = []
    while True:
        new, p = monitor(tail(7))
        if last==new:
            time.sleep(1)
            continue
        last=new
        if p < treshold:
            print (last, p,datetime.datetime.now().time())
            ouch()
        time.sleep(3)

#COUNTS Counter({0.0: 96773, 0.0625: 10481, -0.0625: 10420, -0.125: 28, -0.1875: 3, -0.25: 1, 0.25: 1})
#COUNTS2 Counter({(0.0, 0.0): 88474, (0.0625, -0.0625): 6407, (-0.0625, 0.0625): 6184, (0.0, 0.0625): 4284, (-0.0625, 0.0): 4213, (0.0625, 0.0): 4061, (0.0, -0.0625): 3988, (-0.0625, -0.0625): 23, (0.0, -0.125): 21, (-0.125, 0.0): 20, (0.0625, -0.125): 7, (0.0625, 0.0625): 6, (-0.125, 0.0625): 6, (-0.1875, 0.0): 3, (0.0, -0.1875): 3, (-0.125, -0.0625): 2, (0.25, 0.0625): 1, (0.0, -0.25): 1, (-0.25, 0.0): 1, (0.0, 0.25): 1})
#[(P(x),x)  for (x,y) in COUNTS.most_common()]
#[(0.822151613752793, 0.0), (0.08904313252397904, 0.0625), (0.08852489656520003, -0.0625), (0.00023787880075101735, -0.125), (2.548701436618043e-05, -0.1875), (8.495671455393477e-06, -0.25), (8.495671455393477e-06, 0.25)]
#[(0.8248781318868537, 0.0), (0.08780549464439791, 0.0625), (0.08662940552542056, -0.0625), (0.0004616424579163438, -0.125), (8.243615319934711e-05, -0.1875), (5.49574354662314e-05, 0.125), (2.74787177331157e-05, -0.25), (2.198297418649256e-05, 0.1875), (1.6487230639869423e-05, 0.25), (1.099148709324628e-05, -0.3125), (5.49574354662314e-06, -0.5), (5.49574354662314e-06, -0.375)]
