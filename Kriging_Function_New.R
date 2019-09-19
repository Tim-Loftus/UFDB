#Kriging Function to be applied to any input point shapefile with a column of values.
#Code Owner: Tim Loftus

#Install required libraries for the process:
install.packages("rgdal")
install.packages("sp")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gstat")
install.packages("raster")

#Function Inputs for Kriging:
#Set Inputs:
myDir <- "C:/ArcGIS/UFDB.gdb"                                                 #Location of files 
merged_frc <- readOGR(myDir, "KS_UFDB_FRC")                                   #Merge forecast data to station file; station by station name
frcvar <- merged_frc$RAIN                                                     #Column of Interest: Forecast Variable Name
grd_extent <- readOGR(myDir, "KS_Extent")                                     #Extent or Boundary of Observations (Polygon)

#Begin Modular Function***************************************************************************
#Inputs Would be Forecasted Column Name: frc_var, Forecasted Data Layer: merged_frc, Grid Cell Size: cell_size
#Extent: Polygon Boundary
#Declare Kriging Function:
station_kriging <- function(frcvar, merged_frc, cell_size, grd_extent) {
  #Libraries required for function:  
  library(dplyr)
  library(gstat)
  library(sp)
  library(rgdal)
  library(ggplot2)
  library(raster)
  
  #Create Gridded Forecast Layer:
  grd <- makegrid(grd_extent, cellsize = cell_size)
  grd_frame <- SpatialPixelsDataFrame(grd, grd)     #Converts grid to a spatial pixels data frame
  
  #Projected Coordinates:
  crs(grd_frame) <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(merged_frc) <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  #kriging_result = autoKrige(frcvar~1, merged_frc, grd_frame)
  #plot(kriging_result)
  #print("Done")
  
  #Generate Blank Raster from SpatialPixelsDataFrame:
  rasterDF <- raster(grd_frame)
  rasterDF
  
  #IF Statement When Values are All Equal for Vectors:
  if(min(frcvar)==max(frcvar)){
    kriging_result <- setValues(rasterDF, max(frcvar)) #Sets values of raster to maximum constant value
    result <- writeRaster(raster(kriging_result), "pred.var.tif", options="COMPRESS=LZW", overwrite =TRUE) #Converts to an output raster object
    result_crop <- crop(result, extent(grd_extent)) #Crops raster to input extent
    final_result <- mask(result_crop, grd_extent)
    print("Constant Forecasted Values")
    plot(final_result)
  } else {
    kriging_result = autoKrige(frcvar~1, merged_frc, grd_frame) #Autokrige function
    result <- writeRaster(raster(kriging_result$krige_output, "var1.pred"), "pred.var.tif", options="COMPRESS=LZW", overwrite =TRUE) #Converts to an output raster object
    result_crop <- crop(result, extent(grd_extent)) #Crops raster to input extent
    final_result <- mask(result_crop, grd_extent)
    plot(final_result)
  }
  return(final_result)
}
#Function Ends**************************************************************************************

#Run Kriging Function:
forecast <- station_kriging(frcvar, merged_frc, 0.1, grd_extent)  #Function inputs
forecast                                                          #This returns Kriging Model Raster Layer, krige_output

#Utilize forecast$pred.var for additional analysis


