from functools import cmp_to_key

datafile = open("day13/input.txt", "r")
# datafile = open("day13/test.txt", "r")
lines = filter(None, datafile.read().splitlines())
packets = list(map(eval,lines))

def runCompare(xs,ys):
    result = 1
    for j, _ in enumerate(xs):
        if j > len(ys)-1:
            result = 0
            break
        if not ys: 
            result = 0
            break
        result = compare(xs[j], ys[j])
        if result == 0 or result == 1:
            break
    if result == -1 and [] in ys: 
        result = 1
    return result

# 1 = right order, 0 = wrong order, -1 = continue
def compare(x,y):
    if isinstance(x, int) and isinstance(y, int):
        if x < y: return 1
        if x > y: return 0
        if x == y: return -1
    if isinstance(x, list) and isinstance(y,list):
        if (not x) and (not y): 
            return -1
        return runCompare(x, y)
    if isinstance(x, list):
        return runCompare(x, [y])
    if isinstance(y, list):
        return runCompare([x], y)

def sortCompare(xs, ys):
    result = runCompare(xs,ys)
    if result == -1: result = 1
    if result == 0: result = -1 #need negative for cmp_to_key
    return result

answer1 = 0
packetsAll = [] 
for i in range(0,len(packets), 2):
    left, right = packets[i], packets[i+1]
    result = sortCompare(left,right)
    pairNum = int((i+2)/2)
    if result == 1:
        answer1 += pairNum
    packetsAll.append(left)
    packetsAll.append(right)

decode1, decode2 = [[2]], [[6]]
packetsAll.append(decode1)
packetsAll.append(decode2)    

answer2 = sorted(packetsAll, key=cmp_to_key(sortCompare), reverse=True)
d1idx = answer2.index(decode1)+1
d2idx = answer2.index(decode2)+1

print("Answer 1:", answer1)
print("Answer 2:", d1idx*d2idx)
