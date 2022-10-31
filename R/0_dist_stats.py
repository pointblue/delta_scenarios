from arcpy import env  
from arcpy.sa import *  

def dist_stats(fullpathin, filename, fullpathout):
  # Set environment settings
  env.workspace = fullpathin
  
  # Calculate Euclidean Distance & convert to km
  outEucDistance = EucDistance(in_source_data = filename)/1000
  
  # Save
  outEucDistance.save(fullpathout)
