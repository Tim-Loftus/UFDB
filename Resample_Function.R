#Resample function to be applied to an input raster object by a factor using a bilinear derivative. 
#Code Owner: Tim Loftus

#Install required libraries for the process:

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
