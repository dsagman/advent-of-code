# https://www.reddit.com/r/adventofcode/comments/5isvxv/comment/dbarh7d/

from hashlib import md5
from itertools import compress

input = "awrkjxxr"
moves = {
    'U': lambda x, y: (x, y - 1),
    'D': lambda x, y: (x, y + 1),
    'L': lambda x, y: (x - 1, y),
    'R': lambda x, y: (x + 1, y)
}

def doors(path):
    string = (input + ''.join(path)).encode('utf-8')
    return (int(x, 16) > 10 for x in md5(string).hexdigest()[:4])

def bfs(start, goal):
    queue = [(start, [start], [])]
    while queue:
        (x, y), path, dirs = queue.pop(0)
        for dir in compress('UDLR', doors(dirs)):
            next = moves[dir](x, y)
            nx, ny = next
            if not (0 <= nx < 4 and 0 <= ny < 4):
                continue
            elif next == goal:
                yield dirs + [dir]
            else:
                queue.append((next, path + [next], dirs + [dir]))

def day17():
    paths = list(bfs((0, 0), (3, 3)))
    return ''.join(paths[0]), len(paths[-1])

print(day17())