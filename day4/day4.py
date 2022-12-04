import re

# datafile = open("day4/test.txt", "r")
datafile = open("day4/input.txt", "r")

assigns = [line.strip() for line in datafile]
parse = re.compile('\d*')

def parseAssign(a):
    return [int(x) for x in parse.findall(a) if x != '']


def overlap(a,b):
    return range(max(a[0], b[0]), min(a[-1], b[-1])+1)


def interpretAssign1(a):
    taskA = range(a[0], a[1]+1)
    taskB = range(a[2], a[3]+1)
    overlapAB = overlap(taskA, taskB)
    if (taskA == overlapAB) or (taskB == overlapAB):
        return True
    return False

def interpretAssign2(a):
    taskA = range(a[0], a[1]+1)
    taskB = range(a[2], a[3]+1)
    overlapAB = overlap(taskA, taskB)
    if len(overlapAB) > 0:
        return True
    return False

answer1 = sum([interpretAssign1(parseAssign(a)) for a in assigns])
print("Answer Part 1: ", answer1)

answer2 = sum([interpretAssign2(parseAssign(a)) for a in assigns])
print("Answer Part 2: ", answer2)