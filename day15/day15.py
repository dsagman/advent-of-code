from icecream import ic
import numpy as np
import matplotlib.pyplot as plt
import re
from shapely import Polygon, LineString
from shapely.ops import unary_union
       
def drawGrid(grid):
    gridX = list(map(lambda x: x[0], grid))
    gridY = list(map(lambda x: x[1], grid))
    for i in range(0,len(grid),4):
        plt.plot(gridX[i:i+4]+[gridX[i]],gridY[i:i+4]+[gridY[i]])
    plt.show()

# 0 = empty, 1 = coverage, 2 = signal, 3, beacon
if __name__ == "__main__":
    datafile = open("day15/input.txt", "r")
    # datafile = open("day15/test.txt", "r")

    signalBeacons=list(map(int,re.findall(r'-?\d+',datafile.read())))
    signalX = signalBeacons[0::4]
    signalY = signalBeacons[1::4]
    beaconX = signalBeacons[2::4]
    beaconY = signalBeacons[3::4]

    grid = []
    for i in range(len(signalX)):
        xRange = max(signalX[i],beaconX[i]) - min(signalX[i],beaconX[i])
        yRange = max(signalY[i],beaconY[i]) - min(signalY[i],beaconY[i])
        dist = xRange+yRange+1
        grid.append((signalX[i]+dist,signalY[i],1))
        grid.append((signalX[i],signalY[i]-dist,1))
        grid.append((signalX[i]-dist,signalY[i],1))
        grid.append((signalX[i],signalY[i]+dist,1))        

    coverage = []
    for i in range(0,len(grid),4):
        coverage.append(Polygon(grid[i:i+4]))
    
    minX = min([min(c.bounds[0],c.bounds[2]) for c in coverage])-10000
    maxX = max([max(c.bounds[0],c.bounds[2]) for c in coverage])+10000
    scanY = 2000000
    scanLine = LineString([(minX,scanY),(maxX,scanY)])
    noBeacons = scanLine.intersection(unary_union(coverage))

    beaconsOnLine = set()
    for i, bY in enumerate(beaconY):
        if bY == scanY:
            beaconsOnLine.add((beaconX[i], bY))
 
    print("Answer 1:",int(noBeacons.length)-len(beaconsOnLine)-1)

    # drawGrid(grid)
# answer is 5394423



    
   
    #     plt.plot(gridX,gridY)

    # plt.show()
     
    # plt.imshow(np.rot90(grid2.grid[minX:maxX][:]), origin="lower", aspect="auto", interpolation="none")
    # plt.show()

        # for x in range(0,dist):
        #     for y in range(0,dist-x):
    # grid.append((signalX[i]+x,signalY[i]+y,1))
    # grid.append((signalX[i]+x,signalY[i]-y,1))
    # grid.append((signalX[i]-x,signalY[i]+y,1))
    # grid.append((signalX[i]-x,signalY[i]-y,1))