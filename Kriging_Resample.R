#Kriging Function to be applied to any input point shapefile with a column of values.
#Code Owner: Tim Loftus

#Install required libraries for the process:
install.packages("rgdal")
install.packages("sp")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gstat")
install.packages("raster")
install.packages("automap")
install.packages("devtools")

#Function Inputs for Kriging:
#Set Inputs:
library(rgdal)
myDir <- "C:/ArcGIS/UFDB.gdb"                                                 #Location of files 
merged_frc <- readOGR(myDir, "KS_UFDB_FRC")                                 #Point Data to be interpolated
frcvar <- merged_frc$TSTORM_POP                                               #Column of Interest: Forecast Variable Name
grd_extent <- readOGR(myDir, "KS_Extent")                                     #Extent or Boundary of Observations (Polygon)

#Begin Modular Kriging Function***************************************************************************
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
  library(automap)
  
  #Create Gridded Forecast Layer:
  grd <- makegrid(grd_extent, cellsize = cell_size)
  grd_frame <- SpatialPixelsDataFrame(grd, grd)     #Converts grid to a spatial pixels data frame
  
  #Projected Coordinates:
  crs(grd_frame) <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(merged_frc) <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
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
    zerodist(merged_frc)
    kriging_result = autoKrige(frcvar~1, merged_frc[!is.na(frcvar),], grd_frame, remove_duplicates = TRUE) #Autokrige function
    result <- writeRaster(raster(kriging_result$krige_output, "var1.pred"), "pred.var.tif", options="COMPRESS=LZW", overwrite =TRUE) #Converts to an output raster object
    result_crop <- crop(result, extent(grd_extent)) #Crops raster to input extent
    final_result <- mask(result_crop, grd_extent)
    plot(final_result)
  }
  return(final_result)
}
#Function Ends**************************************************************************************

#Run Kriging Function:
forecast <- station_kriging(frcvar, merged_frc, 0.05, grd_extent)  #Function inputs
forecast                                                          #This returns Kriging Model Raster Layer, krige_output

#Utilize forecast$pred.var for additional analysis

#Begin Modular Resample Function***************************************************************************
#Inpter,uts Would be Input Raster; input_raster and resample_factor; reduce cell size by X%
resample_func <- function(input_raster, resample_factor) {
  #Libraries required for function:
  library(devtools)
  library(raster)
  library(usethis)
  
  #Set number of columns and number of rows from input raster:
  inCols <- ncol(input_raster)
  inRows <- nrow(input_raster)
  
  #Calculate number of columns and number of rows for output raster by an input factor:
  resampledRaster <- raster(ncol=(inCols / resample_factor), nrow=(inRows / resample_factor))
  
  #Resample to the same extent as input raster:
  extent(resampledRaster) <- extent(input_raster)
  
  #Resample function:
  resampledRaster <- resample(input_raster,resampledRaster,datatype="INT1U",method='bilinear',filename="testOutResamp.tif",overwrite=TRUE)
  
  #Plot new resampled raster object
  plot(resampledRaster, main = "Resampled Raster")
  return (resampledRaster)
}
#Function Ends**************************************************************************************

#Run Resample Function: 
input_raster <- forecast$pred.var                               #Input raster, use raster() if not raster object
resample_data <- resample_func(input_raster, 0.16)              #Raster Function, declare inputs
