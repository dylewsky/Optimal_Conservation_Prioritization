# Check vectors for overlaps

# Overlapping features could lead to double counting areas, lengths or counts when data are
# summarized per planning unit.

# Any unexpected overlaps can be investigated in ArcGIS using the Intersect tool

# Overlaps can be fixed by dissolving the layer prior to running the WTW data prep workflow

import arcpy, os

def check_vectors(project_folder, data_folder):
  
  themes_dir = os.path.join(data_folder,"Regional","Themes") # <--- Themes data folder
  includes_dir = os.path.join(data_folder,"Regional","Includes") # <--- Includes data folder
  excludes_dir = os.path.join(data_folder,"Regional","Excludes") # <--- Excludes data folder
  weights_dir = os.path.join(data_folder,"Regional","Weights") # <--- Weights data folder
  
  
  # Functions
  #-----------------------------------------------------------------
  def get_gdb_layers(search_dir):
  
      # returns paths to all feature classes inside all .gdbs residing in search_dir
      
      in_gdb_list = []
      layers = []
  
      for file in os.listdir(search_dir):
          if file.endswith(".gdb"):
              in_gdb_list.append(os.path.join(search_dir, file))
              
      # loop over gdb's in list
      for in_gdb in in_gdb_list:
          #print(in_gdb)
          arcpy.env.workspace = in_gdb
  
          # get any feature classes directly in the gdb
          fcList = arcpy.ListFeatureClasses()
          #print(fcList)
          for fc in fcList:
                  layers.append(os.path.join(in_gdb, fc))
  
          # get any features classes inside feature datasets inside the gdb
          datasets = arcpy.ListDatasets(feature_type = 'feature')
          #print(datasets)
          for dataset in datasets:
              arcpy.env.workspace = os.path.join(in_gdb, dataset)
              fcList = arcpy.ListFeatureClasses()
              for fc in fcList:
                  layers.append(os.path.join(in_gdb, dataset, fc))
  
      # arcpy.env.workspace = os.path.dirname(os.path.realpath(__file__)) # reset arcpy working dir
      arcpy.ResetEnvironments()
      return layers
  
  def check_for_overlaps(fc):
  
      # Checks for overlapping polygons and gives a warning if overlaps found
      arcpy.analysis.Intersect(fc, r'memory/intersects', 'ONLY_FID')
      count = int(arcpy.management.GetCount(r'memory/intersects').getOutput(0))
  
      if count > 0:
          print("WARNING: Layer " + arcpy.Describe(fc).name + " has overlapping features, see them using Intersect(layer) in ArcGIS.")
      
      arcpy.management.Delete(r'memory/intersects')
  
  
  #-----------------------------------------------------------------
          
  # THEMES ---------------------------------------------------------
  # search for geodatabase feature classes in all sub-folders (Theme groups)
  
  # get all sub-folders in Themes
  sub_themes = os.listdir(themes_dir)
  
  # loop over sub-themes
  for sub_theme in sub_themes:
  
      # find all gdb feature classes in folder
      layers = get_gdb_layers(os.path.join(themes_dir, sub_theme))
  
      # proceed if there are layers
      if len(layers) > 0:
  
          print("Processing Theme: " + sub_theme)
  
          # dissolve and intersect
          for layer in layers:
              print("checking: " + arcpy.Describe(layer).name)
              check_for_overlaps(layer)
  
  
  # INCLUDES ------------------------------------------------------
  # find all gdb feature classes in folder
  if os.path.exists(includes_dir):
      layers = get_gdb_layers(includes_dir)
  
  # proceed if there are layers
  if len(layers) > 0:
  
      print("Processing Includes")
      
      # dissolve and intersect
      for layer in layers:
          print("checking: " + arcpy.Describe(layer).name)
          check_for_overlaps(layer)
  
  # WEIGHTS ------------------------------------------------------
  # find all gdb feature classes in folder
  if os.path.exists(weights_dir):
      layers = get_gdb_layers(weights_dir)
  
  # proceed if there are layers
  if len(layers) > 0:
  
      print("Processing Weights")
      
      # dissolve and intersect
      for layer in layers:
          print("checking: " + arcpy.Describe(layer).name)
          check_for_overlaps(layer)
  
  # EXCLUDES ------------------------------------------------------
  # find all gdb feature classes in folder
  if os.path.exists(excludes_dir):
      layers = get_gdb_layers(excludes_dir)
  
  # proceed if there are layers
  if len(layers) > 0:
  
      print("Processing Excludes")
      
      # dissolve and intersect
      for layer in layers:
          print("checking: " + arcpy.Describe(layer).name)
          check_for_overlaps(layer)
