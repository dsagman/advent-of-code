import re
from shapely import Polygon, LineString, geometry
from shapely.ops import unary_union
import matplotlib.pyplot as plt
       
if __name__ == "__main__":
    datafile = open("day15/input.txt", "r")
    # datafile = open("day15/test.txt", "r")

    signalBeacons=list(map(int,re.findall(r'-?\d+',datafile.read())))
    signalX = signalBeacons[0::4]
    signalY = signalBeacons[1::4]
    beaconX = signalBeacons[2::4]
    beaconY = signalBeacons[3::4]

    coverage = []
    for i in range(len(signalX)):
        xRange = max(signalX[i],beaconX[i]) - min(signalX[i],beaconX[i])
        yRange = max(signalY[i],beaconY[i]) - min(signalY[i],beaconY[i])
        dist = xRange+yRange
        coverage.append(Polygon([(signalX[i]+dist,signalY[i])
                                ,(signalX[i],signalY[i]-dist)
                                ,(signalX[i]-dist,signalY[i])
                                ,(signalX[i],signalY[i]+dist)]))        
    
    minX = min([min(c.bounds[0],c.bounds[2]) for c in coverage])-10000
    maxX = max([max(c.bounds[0],c.bounds[2]) for c in coverage])+10000
    scanY = 2000000
    noBeacons = LineString([(minX,scanY),(maxX,scanY)]).intersection(unary_union(coverage))
    beaconsOnLine = set()
    [beaconsOnLine.add((beaconX[i], bY)) for i, bY in enumerate(beaconY) if bY == scanY]
    print("Answer 1:",int(noBeacons.length-len(beaconsOnLine)+1))
   
    distressLimit = 4000000
    distressRect = geometry.box(0,0,distressLimit, distressLimit)
    distressPoss = distressRect.difference(unary_union(coverage))
    print("Answer 2:", int(((distressPoss.bounds[0]+1) * distressLimit) + distressPoss.bounds[1]+1))
    
    plt.plot(*distressPoss.exterior.xy, marker="P", color="red")
    plt.plot(*distressRect.exterior.xy)
    for c in coverage:
        plt.plot(*c.exterior.xy)
    plt.show()
