datafile = open("day2/input.txt", "r")

moves = [(x[0],x[2]) for x in datafile]
print(moves)

strategy = {'X': 'A', 'Y': 'B', 'Z': 'C'}

wins = [('A', 'B'), ('B', 'C'), ('C', 'A')]
scores = {'A': 1, 'B': 2, 'C': 3}
myScore = 0
for move in moves:
    decoded = (move[0],strategy[move[1]])
    myScore += scores[decoded[1]]
    if decoded in wins:
        print("win")
        myScore += 6
    elif decoded[0] == decoded[1]:
        print("draw")
        myScore += 3
    else:
        print("lose")


print("Part 1 answer: ", myScore)
# print("Part 2 answer: ", sum(sums[0:3]))