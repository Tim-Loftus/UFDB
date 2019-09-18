
#Install required libraries for the process:
install.packages("odbc")
install.packages("DBI")
install.packages("rgdal")
install.packages("sp")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("data.table")
install.packages("gstat")
install.packages("gridExtra")
install.packages("maps")
install.packages("parallel")
install.packages("maptools")
install.packages("sf")
install.packages("automap")

#Connect to SQL Database, remove this once process is built from others:
library(odbc)
library(DBI)
odbc::odbcListDrivers()
conn <- dbConnect(odbc(),
                  Driver = "SQL Server",
                  Server = "widgetsql-03.accu.accuwx.com",
                  Database = "adc_forecast",
                  UID = "AES_DataReader",
                  PWD = "AES_DataR3ad3r!")

library(data.table)
#Query Forecast Table
data <- dbGetQuery(conn, "SELECT vw_DayNight.STATION_CODE, vw_DayNight.DATE, vw_DayNight.DAY_FLAG,
                  vw_DayNight.RAIN, vw_DayNight.SNOW, vw_DayNight.ICE,
                  vw_DayNight.TSTORM_POP FROM vW_DayNight WHERE vw_DayNight.DAY_FLAG='N' AND vw_DayNight.DATE ='2019-09-12'")
frcdata <- setDT(data, keep.rownames = TRUE, check.names = FALSE)

#Function Inputs for Kriging:
#Upstream processes to prep for kriging function, this is used for testing purposes
#Set Inputs:
library(rgdal)
library(sp)
myDir <- "C:/ArcGIS/TempSpace.gdb"
station <- readOGR(myDir, "KS_UFDB")
merged_frc <- merge(station, frcdata, by.x="STATION_CO", by.y="STATION_CODE")  #Merge forecast data to station file; station by station name
frcvar <- merged_frc$TSTORM_POP                                                      #Column of Interest: Forecast Variable Name
grd_extent <- readOGR(myDir, "KS_Extent")                                      #Extent or Boundary of Observations (Polygon)

#Begin Modular Function***************************************************************************
#Inputs Would be Forecasted Column Name: frc_var, Forecasted Data Layer: merged_frc, Grid Cell Size: cell_size
# Extent: Polygon Boundary
#Declare Kriging Function:
station_kriging <- function(frcvar, merged_frc, cell_size, grd_extent) {
  library(dplyr)
  library(gstat)
  library(sp)
  library(rgdal)
  library(automap)
  library(ggplot2)
  library(gridExtra)
  library(OpenStreetMap)
  library(rJava)
  library(OSMscale)
  library(raster)
  
  #Create Gridded Forecast Layer:
  grd <- makegrid(grd_extent, cellsize = cell_size)
  grd_frame <- SpatialPixelsDataFrame(grd, grd)
  
  #Projected Coordinates:
  crs(grd_frame) <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(merged_frc) <- "+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  #kriging_result = autoKrige(frcvar~1, merged_frc, grd_frame)
  #plot(kriging_result)
  #print("Done")
  
  #Generate Blank Raster from SpatialPixelsDataFrame:
  rasterDF <- raster(grd_frame)
  rasterDF
  
  #IF Statement When Values are All 0 for Vectors:
  if(all(frcvar==0)){
    kriging_result  <- rasterDF
    print("No Forecasted Values")
    plot(kriging_result)
  } else {
    kriging_result = autoKrige(frcvar~1, merged_frc, grd_frame)
    plot(kriging_result)
  }
  #return(kriging_result)
}
#Function Ends**************************************************************************************

#Run Kriging Function:
forecast <- station_kriging(frcvar, merged_frc, 0.1, grd_extent)  #Function inputs
forecast                                                          #This returns Kriging Model Raster Layer, krige_output


