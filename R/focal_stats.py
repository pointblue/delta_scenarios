from arcpy import env  
from arcpy.sa import *  
arcpy.CheckOutExtension("Spatial")

def focal_stats(pathin, regex = '*', pathout, buffer, fun = 'SUM'):
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

def distance(pathin, filename):
  env.workspace = pathin
  inRaster = filename
  
  outEucDistance = EucDistance(inRaster)
  return outEucDistance
