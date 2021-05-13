## packages
library(lubridate)
library(RColorBrewer)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(ggplot2)
library(readxl)
library(gridExtra)

## setwd
setwd("C:/Users/Tony/Desktop/UCLA/Fall 2020/Sustainability Applied Project/")

# load data
data <- read.csv("C:/Users/Nick/Desktop/UCLA/Fall 2020/Sustainability Applied Project/Existing_Buildings_Energy___Water_Efficiency__EBEWE__Program.csv", na.strings = "")
dwp_equity <- read.csv("C:/Users/Nick/Desktop/UCLA/Fall 2020/Sustainability Applied Project/DWP_Equity_Metrics_Data_Initiative.csv", na.strings = "")
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
data_zip$POSTAL.CODE <- as.character(data_zip$POSTAL.CODE)

## building category counts
count1 <- data %>% count(LADBS.Building.Category)
count2 <- data %>% count(CityOwned)

par(mfrow=c(2,2))
pie(count1$n, labels = paste(count1$LADBS.Building.Category, "\n", round((count1$n/sum(count1$n))*100, 2), "%", sep = ""),
    main="LADBS Building Size Category (SQFT)", col = brewer.pal(n = 5, name = "BuGn"))
pie(count2$n, labels = paste(c("Not City-Owned", "City-Owned"), "\n", round((count2$n/sum(count2$n))*100, 2), "%", sep = ""),
    main="City Owned Buildings", col = brewer.pal(n = 5, name = "BuGn"))

## building year
bld_year <- data %>% count(YEAR.BUILT)
bld_year$YEAR.BUILT <- as.numeric(as.character(bld_year$YEAR.BUILT))
bld_year$code <- ifelse(bld_year$YEAR.BUILT < 1900, 0, ifelse(bld_year$YEAR.BUILT >= 1900 & bld_year$YEAR.BUILT < 1940, 1, 
                  ifelse(bld_year$YEAR.BUILT >= 1940 & bld_year$YEAR.BUILT < 1980, 2, ifelse(bld_year$YEAR.BUILT >= 1980 & bld_year$n <= 2020, 3, NA))))
bld_year[133, 3] <- 4
bld_year1 <- aggregate(bld_year$n ~ bld_year$code, FUN = sum)

barplot(bld_year[1:132, 2], names.arg = bld_year[1:132, 1], col = "#b2e2e2", main = "Year Built")
bp <- barplot(bld_year1$`bld_year$n`, names.arg = c("Before 1900", "1900 to 1940", "1940 to 1980", "1980 to 2020", "NA"),
        col = brewer.pal(n = 5, name = "BuGn"), main = "Year Built (40 Year Groupings)", ylim = c(0, 25000))
text(bp, bld_year1$`bld_year$n`+2, labels = as.numeric(bld_year1$`bld_year$n`), cex = 1, pos = 3)

# Grouped Bar Plot for compliancy status
par(mfrow=c(2, 1))
t <- table(data$COMPLIANCE.STATUS, data$PROGRAM.YEAR)
bp1 <- barplot(t, main="Compliancy by Program Year", xlab="Program Year", col = c("#b2e2e2","#66c2a4"), beside=TRUE, ylim = c(0, 14000))
legend(12000, legend = c("Non-Compliant", "Compliant"), fill = c("#b2e2e2","#66c2a4"), cex = .7)
text(bp1, t[1:10]+2, labels = t, cex = 1, pos = 3)

## group by building type
building_type <- data %>% 
  group_by(PROPERTY.TYPE) %>% 
  summarise(Carbon_Emissions = sum(CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., na.rm = T), Total_Water_Usage = sum(TOTAL.WATER.USE..kgal., na.rm = T),
            Indoor_Water_Use = sum(INDOOR.WATER.USE..kgal., na.rm = T), Outdoor_Water_Use = sum(OUTDOOR.WATER.USE..kgal., na.rm = T), 
            Compliant_Buildings = sum(COMPLIANCE.STATUS, na.rm = T), Total_Occupancy = sum(OCCUPANCY, na.rm = T))
building_type <- cbind(building_type, data %>% count(PROPERTY.TYPE))
building_type <- building_type[, -8]
colnames(building_type)[(colnames(building_type) == "n")] <- "Total_Buildings"
building_type$Compliancy_Rate <- round((building_type$Compliant_Buildings/building_type$Total_Buildings)*100, 1)
building_type <- building_type[order(building_type$Compliancy_Rate, decreasing = T), ]

# plot lowest compliance rates
bp2 <- barplot(building_type[c((nrow(building_type)-9):nrow(building_type)), 9], names.arg = building_type[c((nrow(building_type)-9):nrow(building_type)), 1],
     col = brewer.pal(n = 10, name = "BuGn"), main = "Lowest Compliancy Rates by Building Type", cex.names = 0.7, ylim = c(0, 100), ylab = "Compliancy Rate (%)")
text(bp2, building_type[c((nrow(building_type)-9):nrow(building_type)), 9]+2, 
     labels = paste(building_type[c((nrow(building_type)-9):nrow(building_type)), 9], "%", sep = ""), cex = 0.75, pos = 3)

# group by building category
building_size <- data %>% 
  group_by(LADBS.Building.Category) %>% 
  summarise(Carbon_Emissions = sum(CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., na.rm = T), Total_Water_Usage = sum(TOTAL.WATER.USE..kgal., na.rm = T),
            Indoor_Water_Use = sum(INDOOR.WATER.USE..kgal., na.rm = T), Outdoor_Water_Use = sum(OUTDOOR.WATER.USE..kgal., na.rm = T), 
            Compliant_Buildings = sum(COMPLIANCE.STATUS, na.rm = T), Total_Occupancy = sum(OCCUPANCY, na.rm = T), 
            Avg_Energy_Star_Score= mean(ENERGY.STAR.SCORE, na.rm = T))

## plot for water, carbon emission, and energy usage
par(mfrow=c(2,2))
pie(building_size$Carbon_Emissions, labels = paste(building_size$LADBS.Building.Category, "\n", 
    round((building_size$Carbon_Emissions/sum(building_size$Carbon_Emissions))*100, 1), "%", sep = ""),
    main="Carbon Emissions by Building Size", col = brewer.pal(n = 5, name = "BuGn"))
pie(building_size$Total_Water_Usage, labels = paste(building_size$LADBS.Building.Category, "\n", 
    round((building_size$Total_Water_Usage/sum(building_size$Total_Water_Usage))*100, 1), "%", sep = ""),
    main="Total Water Usage by Building Size", col = brewer.pal(n = 5, name = "BuGn"))
pie(building_size$Total_Occupancy, labels = paste(building_size$LADBS.Building.Category, "\n", 
    round((building_size$Total_Occupancy/sum(building_size$Total_Occupancy))*100, 1), "%", sep = ""),
    main="Total Occupancy by Building Size", col = brewer.pal(n = 5, name = "BuGn"))
barplot(building_size$Avg_Energy_Star_Score, names.arg = building_size$LADBS.Building.Category,
        col = brewer.pal(n = 5, name = "BuGn"), main = "Average Energy Star Score", cex.names = 0.7, 
        ylim = c(0, 100), ylab = "Energy Star Score")
abline(h = 68.44999, lty = 2, col = "red")

## code duplicate observations and changes in efficiency overtime
Program_year <- data %>% 
  group_by(PROGRAM.YEAR, COMPLIANCE.STATUS) %>% 
  summarise(Carbon_Emissions = sum(CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., na.rm = T), Total_Water_Usage = sum(TOTAL.WATER.USE..kgal., na.rm = T),
            Indoor_Water_Use = sum(INDOOR.WATER.USE..kgal., na.rm = T), Outdoor_Water_Use = sum(OUTDOOR.WATER.USE..kgal., na.rm = T), 
            Compliant_Buildings = sum(COMPLIANCE.STATUS, na.rm = T), Total_Occupancy = sum(OCCUPANCY, na.rm = T), 
            Avg_Energy_Star_Score = mean(ENERGY.STAR.SCORE, na.rm = T), Total_Buildings = sum(NUMBER.OF.BUILDINGS, na.rm = T), 
            Site_EUI = mean(`SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ft².`, na.rm = T))
Program_year <- cbind(Program_year, data %>% count(PROGRAM.YEAR, COMPLIANCE.STATUS))
Program_year <- Program_year[, -(12:13)]
colnames(Program_year)[(colnames(Program_year) == "n")] <- "Total_Observations"
Program_year$Carbon_Emissions_per_Building <- round((Program_year$Carbon_Emissions/Program_year$Total_Buildings)*100, 1)
Program_year$Carbon_Emissions_per_Observation <- round((Program_year$Carbon_Emissions/Program_year$Total_Observations)*100, 1)
Program_year$Water_Use_per_building <- round((Program_year$Total_Water_Usage/Program_year$Total_Buildings)*100, 1)
Program_year$Water_Use_per_Observation <- round((Program_year$Total_Water_Usage/Program_year$Total_Observations)*100, 1)
Program_year$Water_Use_per_building <- round((Program_year$Total_Water_Usage/Program_year$Total_Buildings)*100, 1)
Program_year$Water_Use_per_Observation <- round((Program_year$Total_Water_Usage/Program_year$Total_Observations)*100, 1)
Program_year$COMPLIANCE.STATUS...2 <- ifelse(Program_year$COMPLIANCE.STATUS...2 == 0, "Non-Compliant", "Compliant")

## plots
p1 <- ggplot(Program_year, aes(Program_year$PROGRAM.YEAR...1, Program_year$Carbon_Emissions_per_Building)) +   
  geom_bar(aes(fill = Program_year$COMPLIANCE.STATUS...2), position = "dodge", stat="identity") + xlab("Program Year") +
  ylab("Carbon Emissions (Metric Tons)") + labs(fill = "Compliance Status") + 
  theme_minimal() + scale_fill_brewer(palette="BuGn") + ggtitle("Carbon Emissions per Building") + 
  theme(legend.position = c(.9, 0.8), legend.direction = "vertical", legend.background = element_rect(fill = "#F8F8F8"), 
        plot.title = element_text(size = 18))

p2 <- ggplot(Program_year, aes(Program_year$PROGRAM.YEAR...1, Program_year$Water_Use_per_building)) +   
  geom_bar(aes(fill = Program_year$COMPLIANCE.STATUS...2), position = "dodge", stat="identity") + xlab("Program Year") +
  ylab("Water Usage (Kgals)") + labs(fill = "Compliance Status") + 
  theme_minimal() + scale_fill_brewer(palette="BuGn") + ggtitle("Water Use per Building") + 
  theme(legend.position = "none", plot.title = element_text(size = 18))

p3 <- ggplot(Program_year, aes(Program_year$PROGRAM.YEAR...1, Program_year$Avg_Energy_Star_Score)) +   
  geom_bar(aes(fill = Program_year$COMPLIANCE.STATUS...2), position = "dodge", stat="identity") + xlab("Program Year") +
  ylab("Energy Star Score") + labs(fill = "Compliance Status") + 
  theme_minimal() + scale_fill_brewer(palette="BuGn") + ggtitle("Average Energy Star Score") + 
  theme(legend.position = "none", plot.title = element_text(size = 18)) + ylim(c(0, 100)) + geom_hline(yintercept=68.44999, lty = 2, col = "red")

p4 <- ggplot(Program_year, aes(Program_year$PROGRAM.YEAR...1, Program_year$Site_EUI)) +   
  geom_bar(aes(fill = Program_year$COMPLIANCE.STATUS...2), position = "dodge", stat="identity") + xlab("Program Year") +
  ylab("Energy Use (kBtu/sqft)") + labs(fill = "Compliance Status") + 
  theme_minimal() + scale_fill_brewer(palette="BuGn") + ggtitle("Average Energy Use Intensity") + 
  theme(legend.position = "none", plot.title = element_text(size = 18))

## Arrange plots
plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

## save images
ggsave("Efficiency Metrics Plot 1.png", plot = plot, width = 20, height = 14)


## map by zipcode
## add shapefile to map
ca_zips <- read.csv("C:/Users/Nick/Desktop/UCLA/Fall 2020/Sustainability Applied Project/ca_zips.csv")
ca_zips <- as.character(ca_zips$zip_code)
zips <- readOGR("D:/Work Files/Research Projects and Data/Shapefiles/Zip Codes/cb_2018_us_zcta510_500k.shp")
zips <- zips[zips$ZCTA5CE10 %in% ca_zips,]
dwp_equity <- dwp_equity[, c("zipcode", "Population", "Poverty", "pcpov")]
dwp_equity$zipcode <- as.character(dwp_equity$zipcode)

# aggregate data to zip level
data_zip <- data %>% 
  group_by(POSTAL.CODE) %>% 
  summarise(sum_occupancy = sum(OCCUPANCY, na.rm = T), compliant_buildings = sum(COMPLIANCE.STATUS, na.rm = T), 
            total_water_use_kgal = sum(TOTAL.WATER.USE..kgal., na.rm = T), 
            total_CarbonDioxide_use = sum(CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., na.rm = T))
data_zip <- merge(data_zip, dwp_equity, by.x = "POSTAL.CODE", by.y = "zipcode", all = T)
zips <- merge(zips, data_zip, by.x = "ZCTA5CE10", by.y = "POSTAL.CODE", all = T)

# create color pallets
pal1 <- colorBin(c("YlOrRd"), domain = zips$sum_occupancy)
pal2 <- colorBin(c("YlOrRd"), domain = zips$compliant_buildings)
pal3 <- colorBin(c("YlOrRd"), domain = zips$total_water_use_kgal)
pal4 <- colorBin(c("YlOrRd"), domain = zips$total_CarbonDioxide_use)
pal5 <- colorBin(c("YlOrRd"), domain = zips$pcpov)

leaflet(zips) %>%
  addProviderTiles(providers$OpenStreetMap.HOT) %>%
  addPolygons(data = zips, fillColor =  pal1(zips$sum_occupancy), color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5, 
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F), label = paste(zips$ZCTA5CE10, ": ", zips$sum_occupancy, sep = ""), group = "Occupancy") %>% 
  addPolygons(data = zips, fillColor =  pal2(zips$compliant_buildings), color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5, 
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F), label = paste(zips$ZCTA5CE10, ": ", zips$compliant_buildings, sep = ""), group = "Compliant_Buildings") %>% 
  addPolygons(data = zips, fillColor =  pal3(zips$total_water_use_kgal), color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5, 
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F), label = paste(zips$ZCTA5CE10, ": ", zips$total_water_use_kgal, sep = ""), group = "Total_water_use") %>% 
  addPolygons(data = zips, fillColor =  pal4(zips$total_CarbonDioxide_use), color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5, 
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F), label = paste(zips$ZCTA5CE10, ": ", zips$total_CarbonDioxide_use, sep = ""), group = "Carbon_Dioxide") %>% 
  addPolygons(data = zips, fillColor =  pal5(zips$pcpov), color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5, 
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F), label = paste(zips$ZCTA5CE10, ": ", round(zips$pcpov, 1), "%", sep = ""), group = "Poverty") %>% 
  addLayersControl(overlayGroups = c("Compliant_Buildings", "Occupancy","Total_water_use", "Carbon_Dioxide", "Poverty"), position = "topleft", options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("Occupancy","Total_water_use", "Carbon_Dioxide", "Poverty"))








