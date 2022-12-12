from dataclasses import dataclass, field
from numpy import prod
import copy

@dataclass
class Monkey:
    mItems: list[int] = field(default_factory=list)
    mOp: str          = ""
    mDiv: int         = 0
    mTrue: int        = 0 
    mFalse: int       = 0
    mInspected: int   = 0

datafile = open("day11/input.txt", "r")
# datafile = open("day11/test.txt", "r")
lines = filter(None, datafile.read().splitlines())
monkeys = dict()

for line in lines:
    parsed = line.split()
    token, token2 = parsed[0], parsed[1]
    if token == "Monkey": 
            mNum = int(parsed[-1][:-1])
            monkeys[mNum] = Monkey()
    if token == "Starting": 
            monkeys[mNum].mItems = [int(p.split(",")[0]) for p in parsed[2:]]
    if token == "Operation:": 
            monkeys[mNum].mOp = ' '.join(parsed[3:])
    if token == "Test:":
            monkeys[mNum].mDiv = int(parsed[-1])
    if token == "If":
            if token2 == "true:": 
                    monkeys[mNum].mTrue = int(parsed[-1])
            if token2 == "false:":
                    monkeys[mNum].mFalse = int(parsed[-1])
            
# had to look at reddit to figure out the LCM divisor trick
# to prevent numbers from becoming huuuuuuuuuuuge
LCM = prod([v.mDiv for k,v in monkeys.items()])
monkeysMut = copy.deepcopy(monkeys)

rounds = 20
for _ in range(rounds):
    for i in range(len(monkeysMut)):
        for mI in monkeysMut[i].mItems:
            mWorry = eval(monkeysMut[i].mOp, {'old' : mI} )
            mBored = int(mWorry / 3)
            if (mBored % monkeysMut[i].mDiv) == 0:
                nMonkey = monkeysMut[i].mTrue
            else:
                nMonkey = monkeysMut[i].mFalse
            monkeysMut[nMonkey].mItems.append(mBored)   
            monkeysMut[i].mItems = monkeysMut[i].mItems[1:]
            monkeysMut[i].mInspected += 1

inspections = sorted([v.mInspected for k,v in monkeysMut.items()], reverse=True)[:2]
print("Part 1 answer: ", inspections[0]*inspections[1])

rounds = 10000
for _ in range(rounds):
    for i in range(len(monkeys)):
        for mI in monkeys[i].mItems:
            mBored = eval(monkeys[i].mOp, {'old' : mI} ) % LCM
            if (mBored % monkeys[i].mDiv) == 0:
                nMonkey = monkeys[i].mTrue
            else:
                nMonkey = monkeys[i].mFalse
            monkeys[nMonkey].mItems.append(mBored)   
            monkeys[i].mItems = (monkeys[i].mItems[1:])
            monkeys[i].mInspected += 1

inspections = sorted([v.mInspected for k,v in monkeys.items()], reverse=True)[:2]
print("Part 2 answer: ", inspections[0]*inspections[1])