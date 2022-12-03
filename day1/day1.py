datafile = open("input1.txt", "r")

sums = []
runningSum = 0
for line in datafile:
    if line == "\n":
        sums.append(runningSum)
        runningSum = 0
    else:
        runningSum += int(line)
sums.append(runningSum)
sums.sort(reverse=True)

print("Part 1 answer: ", sums[0])
print("Part 2 answer: ", sum(sums[0:3]))