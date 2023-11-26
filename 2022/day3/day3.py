datafile = open("day3/input.txt", "r")
# datafile = open("day3/test.txt", "r")

def getPriority(x):
    if x.islower():
       return ord(x)-96
    else:
       return ord(x)-38

sacks = [x.strip() for x in datafile]
priority = 0
for sack in sacks:
    splitPoint = int(len(sack) / 2)
    commonItem = list(set(sack[0:splitPoint]).intersection(set(sack[splitPoint:])))[0]    
    priority += getPriority(commonItem)
print("Answer Part 1: ", priority)

priority = 0
sacksThree =  [sacks[i:i + 3] for i in range(0, len(sacks), 3)]
for sack in sacksThree:
    commonItem = list(set(sack[0]).intersection(set(sack[1])).intersection(set(sack[2])))[0]
    priority += getPriority(commonItem)
print("Answer Part 2: ", priority)
