datafile = open("day13/input.txt", "r")
# datafile = open("day13/test.txt", "r")
lines = filter(None, datafile.read().splitlines())
packets = list(map(eval,lines))

def runCompare(xs,ys):
    result = 1
    for j, _ in enumerate(xs):
        print("xs =",xs,"ys =",ys)
        if j > len(ys)-1:
            result = 0
            break
        if not ys: 
            result = 0
            break
        result = compare(xs[j], ys[j])
        print("Compared", xs[j], ys[j],"and result =", result)
        if result == 0 or result == 1:
            break
    # if result == -1: 
    #     print("triggered")
    #     result = 1
    return result

# 1 = right order, 0 = wrong order, -1 = continue
def compare(x,y):
    if isinstance(x, int) and isinstance(y, int):
        if x < y: return 1
        if x > y: return 0
        if x == y: return -1
    if isinstance(x, list) and isinstance(y,list):
        if (not x) and (not y): 
            print("ugh")
            return -1
        print("both lists")
        if not y: 
            print("not y")
            return 0
        return runCompare(x, y)
    if isinstance(x, list):
        return runCompare(x, [y])
    if isinstance(y, list):
        # if not y: return 1
        print("list y")
        return runCompare([x], y)

correct = 0
for i in range(0,len(packets), 2):
    left = packets[i]
    right = packets[i+1]
    result = runCompare(left,right)
    if result == -1: result = 1
    pairNum = int((i+2)/2)
    print("pair",pairNum,result)
    correct += result*pairNum

print(correct)
# but I get 5949
# should be 5393

        # print(left[j], type(left[j]))
        # print(right[j], type(right[j]))

