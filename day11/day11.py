
from dataclasses import dataclass, field
import copy


@dataclass
class Monkey:
    mItems: list[int] = field(default_factory=list)
    mOp: str          = ""
    mTest: int        = 0
    mTrue: int        = 0 
    mFalse: int       = 0
    mInspected: int   = 0

datafile = open("day11/input.txt", "r")
# datafile = open("day11/test.txt", "r")
lines = filter(None, datafile.read().splitlines())
monkeys = dict()
runningSum = 0
for line in lines:
    parsed = line.split()
    token, token2 = parsed[0], parsed[1]
    if token == "Monkey": 
            mNum = int(parsed[-1][:-1])
            monkeys[mNum] = Monkey()
    if token == "Starting": 
            mStarting = [int(p.split(",")[0]) for p in parsed[2:]]
            monkeys[mNum].mItems = mStarting
    if token == "Operation:": 
            mOperation = ' '.join(parsed[3:])
            monkeys[mNum].mOp = mOperation
    if token == "Test:":
            mTest = int(parsed[-1])
            monkeys[mNum].mTest = mTest
    if token == "If":
            if token2 == "true:": 
                    mTrue = int(parsed[-1])
                    monkeys[mNum].mTrue = mTrue
            if token2 == "false:":
                    mFalse = int(parsed[-1])
                    monkeys[mNum].mFalse = mFalse

monkeysMut = copy.deepcopy(monkeys)
rounds = 20
for _ in range(rounds):
    for i in range(len(monkeysMut)):
        for mI in monkeysMut[i].mItems:
            mWorry = eval(monkeysMut[i].mOp, {'old' : mI} )
            mBored = int(mWorry / 3)
            if (mBored % monkeysMut[i].mTest) == 0:
                nMonkey = monkeysMut[i].mTrue
            else:
                nMonkey = monkeysMut[i].mFalse
            monkeysMut[nMonkey].mItems.append(mBored)   
            monkeysMut[i].mItems = monkeysMut[i].mItems
            monkeysMut[i].mInspected += 1

inspections = sorted([v.mInspected for k,v in monkeysMut.items()], reverse=True)[:2]
print("Part 1 answer: ", inspections[0]*inspections[1])


rounds = 1000
for r in range(rounds):
    if r % 100 == 0:
        print(r)
        print([v.mItems for k,v in monkeys.items()])
    for i in range(len(monkeys)):
        for mI in monkeys[i].mItems:
            mBored = eval(monkeys[i].mOp, {'old' : mI} )
            
            if (mBored % monkeys[i].mTest) == 0:
                nMonkey = monkeys[i].mTrue
            else:
                nMonkey = monkeys[i].mFalse
            monkeys[nMonkey].mItems.append(mBored)   
            monkeys[i].mItems = (monkeys[i].mItems[1:])[:]
            monkeys[i].mInspected += 1

inspections = sorted([v.mInspected for k,v in monkeys.items()], reverse=True)[:2]
print("Part 2 answer: ", inspections[0]*inspections[1])