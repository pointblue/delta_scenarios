from arcpy import env  
from arcpy.sa import *  

def focal_stats(pathin, pathout, buffer, fun = 'SUM', regex = '*'):
  # Set environment settings
  env.workspace = pathin

  # Get a list of the rasters in the workspace
  rast = arcpy.ListRasters(regex)

  # Set out folder
  outFolder = pathout
  
  # Loop through the list of rasters
  for inRaster in rast:
    # Set the outputname for each output to be the same as the input
    outRaster = outFolder + "\\" + inRaster

    # Process focal stats ("DATA" argument means NA values will be dropped)
    outFocalStat = FocalStatistics(inRaster, NbrCircle(buffer, "Map"), fun, "DATA")
    outFocalStat.save(outRaster)

def dist_stats(pathin, regex, pathout):
  # Set environment settings
  env.workspace = pathin
  rast = arcpy.ListRasters(regex)
  
  # Set out folder
  outFolder = pathout

  for inRaster in rast:
    outRaster = outFolder = "\\" + inRaster + '_dist'
    
    outEucDistance = EucDistance(inRaster)
    outEucDistance.save(outRaster)
