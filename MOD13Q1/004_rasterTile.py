# https://gis.stackexchange.com/a/91090/67264
import os
import sys
import subprocess

width = 1072
height = 648

print(width, 'x', height)

tilesize = 375

for i in range(0, width, tilesize):
    for j in range(0, height, tilesize):
        w = min(i+tilesize, width) - i
        h = min(j+tilesize, height) - j
        output = subprocess.Popen("gdal_translate -of GTIFF -srcwin "+str(i)+", "+str(j)+", "+str(w)+", " \
            +str(h)+" " + sys.argv[1] + " " + sys.argv[2] + "_"+str(i)+"_"+str(j)+".tif", shell=True)