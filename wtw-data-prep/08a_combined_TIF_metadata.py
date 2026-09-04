import os
from osgeo import gdal, gdalconst

def combine_TIF_metadata(project_folder, project_name, reference_rast):
  
  source_file = reference_rast
  dest_file = os.path.join(project_folder,'WTW', str(project_name) + '.tif').replace("\\","/")
  
  print('Source file: ')
  print(source_file)
  print('Dest file: ')
  print(dest_file)
  
  tif_with_WKT = gdal.Open(source_file, gdalconst.GA_ReadOnly)
  tif_without_WKT = gdal.Open(dest_file, gdalconst.GA_Update)

  print('Changing TIF projection from:')
  print(tif_with_WKT.GetProjection())
  print('To:')
  print(tif_without_WKT.GetProjection())
  
  tif_without_WKT.SetProjection(tif_with_WKT.GetProjection())
  
  del(tif_with_WKT)
  del(tif_without_WKT)
  
  return
