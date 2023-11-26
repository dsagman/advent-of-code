datafile = open("day2/input.txt", "r")

moves = [(x[0],x[2]) for x in datafile]
wins = [('A', 'B'), ('B', 'C'), ('C', 'A')]
bonus = {'A': 1, 'B': 2, 'C': 3}

strategy1 = {'X': 'A', 'Y': 'B', 'Z': 'C'}
myScore1 = 0
for move in moves:
    decoded = (move[0],strategy1[move[1]])
    myScore1 += bonus[decoded[1]]
    if decoded in wins:
        # print("win")
        myScore1 += 6
    elif decoded[0] == decoded[1]:
        # print("draw")
        myScore1 += 3

def strategy2(f, s):
    if s == 'X': return losesDict[f]
    if s == 'Y': return f
    if s == 'Z': return winsDict[f]

winsDict = dict(wins)
losesDict = {v:k for k,v in winsDict.items()} 
myScore2 = 0
for move in moves:
    decoded = (move[0],strategy2(move[0],move[1]))
    myScore2 += bonus[decoded[1]]
    if decoded in wins:
        # print("win")
        myScore2 += 6
    elif decoded[0] == decoded[1]:
        # print("draw")
        myScore2 += 3

print("Part 1 answer: ", myScore1)
print("Part 2 answer: ", myScore2)