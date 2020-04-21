import shapefile
import sys

shape = shapefile.Reader(sys.argv[1])

print(shape)