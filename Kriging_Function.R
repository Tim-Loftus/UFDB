
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


#Begin Modular Function*******************************************************************************
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
  min_lon <- min(merged_frc$coords.x1)
  max_lon <- max(merged_frc$coords.x1)
  min_lat <- min(merged_frc$coords.x2)
  max_lat <- max(merged_frc$coords.x2)
  
  Longitude.range <- as.numeric(c(min_lon,max_lon))
  Latitude.range <- as.numeric(c(min_lat,max_lat))
  
  grd <- expand.grid(Longitude = seq(from = Longitude.range[1], to = Longitude.range[2], by = 0.1), 
                     Latitude = seq(from = Latitude.range[1],
                                    to = Latitude.range[2], by = 0.1))  # expand points to grid
  
  #Convert Grid to a Spatial Pixels Data Frame:
  grd_frame = SpatialPixelsDataFrame(points = grd[c("Longitude", "Latitude")], data = grd)
  
  #Projected Coordinates:
  proj4string(merged_frc) <- proj4string(grd_frame)
  proj4string(grd_extent) <- proj4string(grd_frame)#make sure the shapefile has the same CRS from the data, and from the prediction grid.
  
  #Clip the prediction grid with the shapefile.
  grd_subset <- grd_frame[grd_extent,]
  proj4string(grd_subset) <- CRS(proj4string(grd_extent))
  
  #IF Statement When Values are All 0 for Vectors:
  if(all(frcvar==0)){
  grd_subset["var1.pred"] <- NA
  grd_subset$var1.pred <- 0 
  kriging_result  <- as(grd_subset, "SpatialPixels")
  } else {
  kriging_result = autoKrige(frcvar~1, merged_frc, grd_subset)
  }
  plot(kriging_result)

}
#Function Ends**************************************************************************************

#Function Inputs for Kriging:
#Upstream processes to prep for kriging function, this is used for testing purposes
#Set Inputs:
library(rgdal)
library(sp)
myDir <- "C:/ArcGIS/TempSpace.gdb"
station <- readOGR(myDir, "KS_UFDB")

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
data <- dbGetQuery(conn, "SELECT vw_DayNight.STATION_CODE, vw_DayNight.DATE, vw_DayNight.DAY_FLAG,
                   vw_DayNight.RAIN, vw_DayNight.SNOW, vw_DayNight.ICE,
                   vw_DayNight.TSTORM_POP FROM vW_DayNight WHERE vw_DayNight.DAY_FLAG='D' AND vw_DayNight.DATE = '2019-09-12'")
frcdata <- setDT(data, keep.rownames = TRUE, check.names = FALSE)

#Merge forecast data to station file; station by station name; stat_col:
merged_frc <- merge(station, frcdata, by.x="STATION_CO", by.y="STATION_CODE")

View(merged_frc)
barplot(table(merged_frc$RAIN))
class(merged_frc)

frcvar <- merged_frc$RAIN
grd_extent <- readOGR(myDir, "KS_Extent")
forecast <- station_kriging(frcvar, merged_frc, 0.1, grd_extent)
forecast
