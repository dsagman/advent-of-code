def check_integers(left, right):
    if left < right:
        return "correct"
    elif left > right:
        return "wrong"
    else:
        return False

def check_lists(left, right):
    decision = False
    for pair in zip(left, right):
        # Int vs Int
        if type(pair[0]) == type(pair[1]) == type(1):
            decision = check_integers(pair[0], pair[1])
        # List vs List
        elif type(pair[0]) == type(pair[1]) == type([]):
            decision = check_lists(pair[0], pair[1])
        # Int vs List
        elif type(pair[0]) == type(1) and type(pair[1]) == type([]):
            decision = check_lists([pair[0]], pair[1])
        # List vs Int
        elif type(pair[0]) == type([]) and type(pair[1]) == type(1):
            decision = check_lists(pair[0], [pair[1]])
        if decision:
            break
    if not decision:
        if len(left) < len(right):
            decision = "correct"
        elif len(left) > len(right):
            decision = "wrong"
    return decision

def part1_solution(packet_pairs):
    solution = 0
    for i in range(len(packet_pairs)):
        if check_lists(packet_pairs[i][0], packet_pairs[i][1]) == "correct":
            solution += i+1
            print(i+1)
        # print(i+1)
    return solution

def part2_solution(packet_pairs):
    old_order = []
    for pair in packet_pairs:
        old_order.extend(pair)
    old_order.extend([[2], [6]])
    keep_going = True
    while keep_going:
        new_order = []
        keep_going = False
        for i in range(len(old_order)-1):
            left = old_order[i]
            right = old_order[i+1]
            if check_lists(left, right) == "correct":
                new_order.extend([left])
                if i+2 == len(old_order):
                    new_order.extend([right])
            elif check_lists(left, right) == "wrong":
                new_order.extend([right, left])
                new_order.extend(old_order[i+2:])
                keep_going = True
                break
        old_order = new_order[:]
    return (1+old_order.index([2])) * (1+old_order.index([6]))

with open("day13/tempmine.txt", "r", encoding="UTF-8") as f:
    lines = f.read().split("\n")
    packet_pairs = []
    for i in range(0,len(lines),3):
        packet_pairs.append([eval(lines[i]), eval(lines[i+1])])

print(f"Part1: {part1_solution(packet_pairs)}") 

# print(f"Part2: {part2_solution(packet_pairs)}")