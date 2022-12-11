
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

# datafile = open("day11/input.txt", "r")
datafile = open("day11/test.txt", "r")
lines = filter(None, datafile.read().splitlines())
monkeys = dict()
runningSum = 0
for line in lines:
    parsed = line.split()
    token, token2 = parsed[0], parsed[1]
    match token:
        case "Monkey": 
            mNum = int(parsed[-1][:-1])
            monkeys[mNum] = Monkey()
        case "Starting": 
            mStarting = [int(p.split(",")[0]) for p in parsed[2:]]
            monkeys[mNum].mItems = mStarting
        case "Operation:": 
            mOperation = ' '.join(parsed[3:])
            monkeys[mNum].mOp = mOperation
        case "Test:":
            mTest = int(parsed[-1])
            monkeys[mNum].mTest = mTest
        case "If":
            match token2:
                case "true:": 
                    mTrue = int(parsed[-1])
                    monkeys[mNum].mTrue = mTrue
                case "false:":
                    mFalse = int(parsed[-1])
                    monkeys[mNum].mFalse = mFalse

monkeysMut = copy.deepcopy(monkeys)
rounds = 20
for _ in range(rounds):
    for i in range(len(monkeysMut)):
        for mI in monkeysMut[i].mItems:
            mWorry = eval(monkeysMut[i].mOp, {'old' : mI} )
            mBored = int(mWorry / 3)
            mTest = (mBored % monkeysMut[i].mTest) == 0
            if mTest:
                nMonkey = monkeysMut[i].mTrue
            else:
                nMonkey = monkeysMut[i].mFalse
            monkeysMut[nMonkey].mItems.append(mBored)   
            monkeysMut[i].mItems = monkeysMut[i].mItems[1:]
            monkeysMut[i].mInspected += 1

inspections = sorted([v.mInspected for k,v in monkeysMut.items()], reverse=True)[:2]
print("Part 1 answer: ", inspections[0]*inspections[1])

monkeysMut = copy.deepcopy(monkeys)
rounds = 1000
for r in range(rounds):
    if r % 100 == 0:
        print(r)
    for i in range(len(monkeysMut)):
        for mI in monkeysMut[i].mItems:
            mBored = eval(monkeysMut[i].mOp, {'old' : mI} )
            mTest = (mBored % monkeysMut[i].mTest) == 0
            if mTest:
                nMonkey = monkeysMut[i].mTrue
            else:
                nMonkey = monkeysMut[i].mFalse
            monkeysMut[nMonkey].mItems.append(mBored)   
            monkeysMut[i].mItems = monkeysMut[i].mItems[1:]
            monkeysMut[i].mInspected += 1

inspections = sorted([v.mInspected for k,v in monkeysMut.items()], reverse=True)[:2]
# print(inspections)
print("Part 2 answer: ", inspections[0]*inspections[1])