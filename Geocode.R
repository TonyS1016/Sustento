## packages
library(censusxy)
library(lubridate)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

## setwd
setwd("C:/Users/Nick/Desktop/UCLA/Fall 2020/Sustainability Applied Project/")

# load data
data <- read.csv("C:/Users/Nick/Desktop/UCLA/Fall 2020/Sustainability Applied Project/Existing_Buildings_Energy___Water_Efficiency__EBEWE__Program.csv", na.strings = "")
ces3 <- read_excel("C:/Users/Nick/Desktop/UCLA/Fall 2020/Sustainability Applied Project/ces3results.xlsx")

## clean/wrangle data
data[, c(3,5:7,12:17,21:25)] <- lapply(data[, c(3,5:7,12:17,21:25)], as.numeric)
data[, c(2,11,18:20,26:27)] <- lapply(data[, c(2,11,18:20,26:27)], factor)
data$COMPLIANCE.STATUS <- ifelse(data$COMPLIANCE.STATUS == "COMPLIED", 1, 0)
data$CityOwned <- ifelse(grepl("City Owned", data$LADBS.Building.Category, ignore.case = T) == T, 1, 0)
data$LADBS.Building.Category <- ifelse(grepl("100,000+", data$LADBS.Building.Category, ignore.case = T) == T, "100,000+", 
                                ifelse(grepl("15,000 to 19,999", data$LADBS.Building.Category, ignore.case = T) == T, "15,000 to 19,999",
                                ifelse(grepl("20,000 to 49,999", data$LADBS.Building.Category, ignore.case = T) == T, "20,000 to 49,999",
                                ifelse(grepl("50,000 to 99,999", data$LADBS.Building.Category, ignore.case = T) == T, "50,000 to 99,999",
                                ifelse(grepl("7,500 to 14,999", data$LADBS.Building.Category, ignore.case = T) == T, "7,500 to 14,999", NA)))))
data$ENERGY.STAR.CERTIFICATION...LAST.APPROVAL.DATE <- mdy(data$ENERGY.STAR.CERTIFICATION...LAST.APPROVAL.DATE)
data$ENERGY.STAR.CERTIFICATION...ELIGIBILITY <- ifelse(data$ENERGY.STAR.CERTIFICATION...ELIGIBILITY == "Yes", 1, 0)
data$Building_Wave <- str_sub(as.character(data$BUILDING.ID), -1, -1)
data$Building_Wave <- ifelse(data$Building_Wave == "0" | data$Building_Wave == "1", 1, ifelse(data$Building_Wave == "2" | data$Building_Wave == "3", 2, 
                      ifelse(data$Building_Wave == "4" | data$Building_Wave == "5", 3, ifelse(data$Building_Wave == "6" | data$Building_Wave == "7", 4, 
                      ifelse(data$Building_Wave == "8" | data$Building_Wave == "9", 5, NA)))))
data$Building_Wave <- as.numeric(data$Building_Wave)

## add city/state to data
## needed for census geocoding
data$City <- "Los Angeles" #approximate city
data$State <- "CA"

## Batch Geocode Unique Building Addresses
unique_bld <- data[duplicated(data$BUILDING.ADDRESS) == F,] #Subset for non-duplicated building ID's
unique_bld <- unique_bld[c(2,1,31:32,18)] #include only building id, address, city, state, and postal code
geocodes <- cxy_geocode(unique_bld, street = "BUILDING.ADDRESS", city = "City", 
                        state = "State", zip = "POSTAL.CODE", return = "geographies", 
                        vintage = 419, class = "dataframe")
geocodes$Census_Tract <- ifelse(is.na(geocodes$cxy_tract_id) == F, paste(6037, geocodes$cxy_tract_id, sep = ""), NA)

## subset those that failed to parse (NA's)
na <- geocodes[is.na(geocodes$cxy_lon)==T,]

## for loop to individually geocode
geocodes1 <- matrix(NA, nrow = 1, ncol = 5)
geocodes1 <- as.data.frame(geocodes1)
colnames(geocodes1) <- c("matchedAddress", "coordinates.x", "coordinates.y", "geographies.Census.Tracts.GEOID", "Building.ID")

for (i in 1:nrow(na)) {
  id <- na[i, 1] #store building id's
  temp <- cxy_single(na[i,2], "Los Angeles", "CA", na[i,5], return = "geographies", vintage = 419)
  temp <- temp[, c(1:3, 76)]
  
  if (is.null(temp) == F){
    temp$Building.ID <- id
  }
  else {
    temp <- matrix(NA, nrow = 1, ncol = 5)
    temp <- as.data.frame(temp)
    colnames(temp) <- c("matchedAddress", "coordinates.x", "coordinates.y", "geographies.Census.Tracts.GEOID", "Building.ID")
    temp$Building.ID <- id
  }
  geocodes1 <- rbind(geocodes1, temp)
  print(i)
}

# merge and clean both geocode df's
geocodes1 <- geocodes1[-1,]
geo <- merge(geocodes, geocodes1, by.x = "BUILDING.ID", by.y = "Building.ID", all = T)
geo$cxy_lon <- ifelse(is.na(geo$cxy_lon) == T & is.na(geo$coordinates.x) == F, geo$coordinates.x, geo$cxy_lon)
geo$cxy_lat <- ifelse(is.na(geo$cxy_lat) == T & is.na(geo$coordinates.y) == F, geo$coordinates.y, geo$cxy_lat)
geo$Census_Tract <- ifelse(is.na(geo$Census_Tract) == T & is.na(geo$geographies.Census.Tracts.GEOID) == F, substr(geo$geographies.Census.Tracts.GEOID, 2, nchar(geo$geographies.Census.Tracts.GEOID)), geo$Census_Tract)
geo <- geo[,c(1:2,5:7,12)]
colnames(geo) <- c("BUILDING.ID", "BUILDING.ADDRESS", "POSTAL.CODE", "Longitude", "Latitude", "Census_Tract_GEOID")
write.csv(geo, "Geocodes.csv", na="", row.names = F)

# merge Geocodes (geo) with building dataset (data)
data <- merge(data, geo, by = "BUILDING.ID", all = T)
data <- data[, -c(31:34)]

# merge with CES
ces3 <- ces3[, c(1,4,8:9,11)]
ces3$`Census Tract` <- as.character(ces3$`Census Tract`)
data <- merge(data, ces3, by.x = "Census_Tract_GEOID", by.y = "Census Tract", all.x = T)
data$`SB 535 Disadvantaged Community` <- ifelse(data$`SB 535 Disadvantaged Community` == "Yes", 1, 0)
colnames(data) <- c("Census.Tract.GEOID", "BUILDING.ID", "BUILDING.ADDRESS", "CARBON.DIOXIDE.EMISSIONS", "COMPLIANCE.STATUS",
                    "X.DIFFERENCE.SOURCE.EUI", "X.DIFFERENCE.SITE.EUI", "ENERGY.STAR.SCORE", "ENERGY.STAR.CERTIFICATION.ELIGIBILITY", 
                    "ENERGY.STAR.CERTIFICATION.LAST.APPROVAL.DATE", "ENERGY.STAR.CERTIFICATION.YEARS.CERTIFIED", "ENTITY.RESPONSIBLE.FOR.BENCHMARK", 
                    "GROSS.BUILDING.FLOOR.AREA", "INDOOR.WATER.USE", "INDOOR.WATER.USE.INTENSITY", "NUMBER.OF.BUILDINGS", "OCCUPANCY", 
                    "OUTDOOR.WATER.USE", "POSTAL.CODE", "PROGRAM.YEAR", "PROPERTY.TYPE", "SITE.EUI", "Source.EUI", "TOTAL.WATER.USE", 
                    "WEATHER.NORMALIZED.SITE.EUI", "WEATHER.NORMALIZED.SOURCE.EUI", "YEAR.BUILT", "AIN", "LADBS.Building.Category", 
                    "CityOwned", "Building.Wave", "Longitude", "Latitude", "ZIP", "CES.Score", "CES.Score.Percentile", "Disadvantaged.Community")
data <- data[,-34]
write.csv(data, "Building_Data.csv", na="", row.names = F)

