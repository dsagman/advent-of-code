from icecream import ic
import itertools as it
import numpy as np
import matplotlib.pyplot as plt
from dataclasses import dataclass
import copy
import random

@dataclass
class Grid:
    grid : np.ndarray             
    minmax: list[int]          
    overflow: bool 
    frame: int

def parse(xs):
    return [list(map(int,x.split(","))) for x in xs.split(" -> ")]

def getRocks(xs):
    rocks = []
    for a in zip(xs,xs[1:]):
        [x1,y1], [x2,y2] = sorted(a, key=lambda e: (e[0], e[1]))
        if y1 == y2:
            rocks.append([[x,y1] for x in range(x1,x2+1)])
        else:
            rocks.append([[x1,y] for y in range(y1,y2+1)])
    return rocks

def minmax(rocks):
    minX = min([r[0] for r in rocks])
    maxX = max([r[0] for r in rocks])
    minY = min([r[1] for r in rocks])
    maxY = max([r[1] for r in rocks])
    return [minX, maxX, minY, maxY]

def putRocks(rocks, minX, maxX, minY, maxY):
    grid = np.zeros((maxX+5,maxY+10), dtype=int)
    for x in range(minX, maxX+1):
        for y in range(0,maxY+1):
            if [x,y] in rocks:
                grid[x,y] = "1"
    return grid

def printGrid(grid):
    minX, maxX, minY, maxY = grid.minmax
    for y in range(0,maxY+1):
        for x in range(minX, maxX+1):
            print(str(grid.grid[x,y]).translate(str.maketrans("012",".#o")),end='')
        print()

def dropSand(grid,x):
    minX, maxX, minY, maxY = grid.minmax
    atRest = False
    sandX, sandY = x, 0
    while not atRest:
        if grid.grid[sandX, sandY+1] == 0:
            sandY += 1
        elif grid.grid[sandX-1, sandY+1] == 0:
            sandX -= 1
            sandY += 1
        elif grid.grid[sandX+1, sandY+1] == 0:
            sandX += 1
            sandY += 1
        else:
            atRest = True
    
        if sandY > maxY+1 or sandY == 0:
            grid.overflow = True
            atRest = True
    saveImg(grid)
    grid.frame += 1
    grid.grid[sandX, sandY] = random.randrange(2,10)
    return grid
        
def saveImg(grid):
    minX, maxX, minY, maxY = grid.minmax
    plt.imsave(f"day14/frames/grid{grid.frame:05}.png", np.rot90(grid.grid[minX:maxX][:]), origin="lower")

if __name__ == "__main__":
    datafile = open("day14/input.txt", "r")
    # datafile = open("day14/test.txt", "r")
    lines = map(parse,datafile.read().splitlines())
    rocks = list(it.chain(*it.chain(*[getRocks(l) for l in lines])))
    minX, maxX, minY, maxY = minmax(rocks)
    minX = minX - 200
    maxX = maxX + 200
    myGrid = putRocks(rocks, minX, maxX, minY, maxY)
    
    grid = Grid(grid=copy.deepcopy(myGrid), minmax=[minX, maxX, minY, maxY], overflow=False, frame=0)
    grains = 0
    while not grid.overflow:
        grains += 1
        dropSand(grid,500)
    grains -= 1
    print("Answer 1,", grains)  

    plt.imshow(np.rot90(grid.grid[minX:maxX][:]), origin="lower", aspect="auto", interpolation="none")
    plt.show()

    floor = maxY + 2
    grid2 = Grid(grid=copy.deepcopy(myGrid), minmax=[minX, maxX, minY, floor], overflow=False, frame=0)
    for x in range(minX, maxX+1):
        grid2.grid[x,floor] = "1"
    grains2 = 0
    while not grid2.overflow:
        grains2 += 1
        dropSand(grid2,500)
    print("Answer 2,", grains2) 
    
    plt.imshow(np.rot90(grid2.grid[minX:maxX][:]), origin="lower", aspect="auto", interpolation="none")
    plt.show()


