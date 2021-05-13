## packages
library(ggplot2)
library(car)
library(ggpubr)
library(dplyr)

## setwd
setwd("C:/Users/Nick/Desktop/UCLA/Fall 2020/Sustainability Applied Project/simulation data/")

## load data
postprocessing <- read.csv("LA_HVAC_results.csv")
basic <- read.csv("Free (Basic).csv")
advanced2 <- read.csv("HVAC Elec (advanced 2) package.csv")
advanced1 <- read.csv("HVAC traditional (advanced 1) package.csv")
mid <- read.csv("MidPackage_RetrofitResults.csv")

## merge post-processing data with each simulation package
package_sims <- c("basic", "advanced1", "mid")
for (i in package_sims){
  t <- merge(get(i), postprocessing, by.x = "ubid", by.y = "LA_bid")
  assign(i, t)
  rm(t)
}

## note the post-processing does not merge with advanced 2 due to the ubid #'s rounding off
## manually merge advanced 2 using other building id
advanced2 <- merge(advanced2, postprocessing, by.x = "building.id", by.y = "citybes_id")

## histogram plots for investment costs for basic
bld_types <- c("Medium Office", "Multi Family House", "Large Office", "Medium Retail", "Small Office", "Small Retail")
par(mfrow=c(3,2))
m_cost <- numeric(length(bld_types))

for (i in 1:length(bld_types)){
  cost <- basic[basic$building.type == bld_types[i], "investment.cost...."]
  mc <- median(cost, na.rm = T)
  h <- hist(cost, col = "lightblue", main = paste("Histogram of Investment Cost: ", bld_types[i], sep = ""), ylim = c(0, 60))
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_cost[i] <- mc
}

m_cost <- data.frame(m_cost, bld_types)
plot1 <- ggplot(data=m_cost, aes(x=m_cost, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Investment Cost ($)") + ylab(NULL) + theme_minimal() + xlim(0, 300000) + ggtitle("Basic Package") + 
  geom_text(aes(label=round(m_cost)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)
plot1


## histogram plots for annual savings for basic
par(mfrow=c(3,2))
m_savings <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  savings <- basic[basic$building.type == bld_types[i], "annual.energy.cost.saving...."]
  mc <- median(savings, na.rm = T)
  h <- hist(savings, col = "lightblue", main = paste("Histogram of Energy Cost Savings (Annual): ", bld_types[i], sep = ""), ylim = c(0, 70))
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_savings[i] <- mc
}

m_savings <- data.frame(m_savings, bld_types)
plot2 <- ggplot(data=m_savings, aes(x=m_savings, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Energy Savings ($)") + ylab(NULL) + theme_minimal() + xlim(0, 200000) + ggtitle("Basic Package") + 
  geom_text(aes(label=round(m_savings)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)
plot2


## create plots for hvac
## hvac units for all packages
par(mfrow=c(2,3))
m_savings <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  hvac_units <- advanced1[advanced1$building.type == bld_types[i] & duplicated(advanced1$ubid)==F, "number_of_HVAC_units"]
  mc <- median(hvac_units, na.rm = T)
  h <- hist(hvac_units, col = "lightblue", main = paste(bld_types[i], sep = ""), ylim = c(0, 100), 
            xlab = "HVAC Units", cex = 2)
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_savings[i] <- mc
}

m_savings <- data.frame(m_savings, bld_types)
plot3 <- ggplot(data=m_savings, aes(x=m_savings, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median HVAC Units") + ylab(NULL) + theme_minimal() + xlim(0, 100) + ggtitle("Number of HVAC Units") + 
  geom_text(aes(label=round(m_savings)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)
plot3

## average cooling capacity for all packages
par(mfrow=c(3,2))
m_savings <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  avg_capacity <- advanced1[advanced1$building.type == bld_types[i] & duplicated(advanced1$ubid)==F, "average_cooling_capacity_per_HVAC_unit.kW."]
  mc <- median(avg_capacity/3.51685284, na.rm = T)
  h <- hist(avg_capacity/3.51685284, col = "lightblue", main = paste("Histogram of Average Cooling Capacity (Tons): ", bld_types[i], sep = ""), ylim = c(0, 80),
            xlab = "Average Cooling Capacity (Tons per HVAC Unit)")
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_savings[i] <- mc
}

m_savings <- data.frame(m_savings, bld_types)
plot4 <- ggplot(data=m_savings, aes(x=m_savings, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Average Cooling Capacity (Tons per HVAC Unit)") + ylab(NULL) + theme_minimal() + xlim(0, 500) + ggtitle("Average Cooling Capacity per HVAC Unit") + 
  geom_text(aes(label=round(m_savings)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)
plot4

## Arrange plots
plot5 <- ggarrange(plot3, plot4, ncol = 1, nrow = 2, common.legend = T, legend = "bottom")

## save images
ggsave("HVAC Units and Cooling Capacity by Building Type.pdf", plot = plot5, width = 10, height = 5)

## combine all simulation data and all packages
## remove duplicate observations
package_sims <- c("basic", "advanced1", "mid", "advanced2")
for (i in package_sims){
  t <- get(i)
  t <- t[duplicated(t$building.id)==T,]
  assign(i, t)
  rm(t)
}

## combine all data for the four packages
advanced2 <- advanced2[, c(4, 1:3, 5:109)]

# order basic by building id, pull ubid and citybes to put into advanced 2
basic <- basic[order(basic$building.id),]
ub <- basic$ubid
cb <- basic$citybes_id

advanced2$ubid <- ub
advanced2$LA_bid <- cb
colnames(advanced2) <- names(basic)

## add package name variable to each df
basic$package <- "Basic"
mid$package <- "Mid"
advanced1$package <- "Advanced1"
advanced2$package <- "Advanced2"

simulation_data <- rbind(basic, mid, advanced1, advanced2)
simulation_data <- simulation_data[order(simulation_data$building.id),]
simulation_data[simulation_data == ""] <- NA


## wrangle dataset for model
## change building type to factor
basic$building.type <- as.factor(basic$building.type)

## convert units
basic$annual_water <- (basic$baseline.annual.water.for.water.systems..m3.m2.*basic$total.floor.area..m2.*264.172)
basic$floor_area <- basic$total.floor.area..m2.*10.7639
basic$annual_water <- basic$annual_water/basic$total.floor.area..m2.
basic$floor_area_2 <- (basic$floor_area^2)
basic$year.built..year. <- as.factor(basic$year.built..year.)
basic$hvac_cooling_ton <- basic$average_cooling_capacity_per_HVAC_unit.kW./3.51685284

## convert year to decades
basic$year.built..year. <- as.numeric(as.character(basic$year.built..year.))
basic$year.built..year. <- ifelse(basic$year.built..year. < 1960, "1950s", 
                                  ifelse(basic$year.built..year. < 1970 & basic$year.built..year. >= 1960, "1960s",
                                         ifelse(basic$year.built..year. < 1980 & basic$year.built..year. >= 1970, "1970s",
                                                ifelse(basic$year.built..year. < 1990 & basic$year.built..year. >= 1980, "1980s",
                                                       ifelse(basic$year.built..year. < 2000 & basic$year.built..year. >= 1990, "1990s", basic$year.built..year.)))))

basic$building.type <- ifelse(grepl("Office", basic$building.type), "Office", 
                              ifelse(grepl("Retail", basic$building.type), "Retail", "Multi Family House"))
bld_types <- c("Office", "Multi Family House", "Retail")

## subset to remove rows with outliers in floor area
t <- boxplot(basic$floor_area)
out <- basic[order(basic$floor_area, decreasing = T), 112][1:length(t$out)]
basic_nout <- basic[!(basic$floor_area %in% out),]

## build regression models
model1 <- lm(log(hvac_cooling_ton) ~ year.built..year. + scale(floor_area) + scale(floor_area_2) +
               scale(baseline.annual.site.energy.use.intensity..kWh.m2.) + building.type , data = basic_nout)
summary(model1)

par(mfrow=c(1,1))
plot(fitted(model1), resid(model1), xlab = "All (Fitted)", ylab = "All (Residual)")
abline(col = "red", h=0)
vif(model1)

## prepare building data
data <- read.csv("C:/Users/Nick/Desktop/UCLA/Fall 2020/Sustainability Applied Project/Building_Data.csv", na.strings = "")

## clean/wrangle data
data <- data[duplicated(data$BUILDING.ID) == F,]
data$PROPERTY.TYPE <- ifelse(data$PROPERTY.TYPE == "Multifamily Housing", "Multi Family House", 
                             ifelse(data$PROPERTY.TYPE == "Retail Store", "Retail",
                                    ifelse(data$PROPERTY.TYPE == "Office", "Office", NA)))

bld_data <- data[, c(2, 21, 27, 13, 22)]
#bld_data <- data[, c(2, 27, 13, 22)]
bld_data <- bld_data[complete.cases(bld_data)==T,]
colnames(bld_data) <- c("BUILDING.ID", "building.type", "year.built..year.", "floor_area", "baseline.annual.site.energy.use.intensity..kWh.m2.")
bld_data$floor_area_2 <- bld_data$floor_area^2
bld_data$year.built..year. <- ifelse(bld_data$year.built..year. < 1960, "1950s", 
                                     ifelse(bld_data$year.built..year. < 1970 & bld_data$year.built..year. >= 1960, "1960s",
                                            ifelse(bld_data$year.built..year. < 1980 & bld_data$year.built..year. >= 1970, "1970s",
                                                   ifelse(bld_data$year.built..year. < 1990 & bld_data$year.built..year. >= 1980, "1980s",
                                                          ifelse(bld_data$year.built..year. < 2019 & bld_data$year.built..year. >= 1990, "1990s", bld_data$year.built..year.)))))

## predict
bld_data$predicted_cooling_capacity_tons <- round(exp(predict.lm(model1, bld_data)),6)
bld_data <- bld_data[is.finite(bld_data$predicted_cooling_capacity_tons) & bld_data$predicted_cooling_capacity_tons < 4080676074696.67383,]




## investment cost and annual energy savings
## change building type to factor
simulation_data$building.type <- ifelse(grepl("Office", simulation_data$building.type), "Office", 
                                        ifelse(grepl("Retail", simulation_data$building.type), "Retail", "Multi Family House"))
simulation_data$building.type <- as.factor(simulation_data$building.type)

## convert units
simulation_data$annual_water <- (simulation_data$baseline.annual.water.for.water.systems..m3.m2.*simulation_data$total.floor.area..m2.*264.172)
simulation_data$floor_area <- simulation_data$total.floor.area..m2.*10.7639
simulation_data$annual_water <- simulation_data$annual_water/simulation_data$total.floor.area..m2.
simulation_data$floor_area_2 <- (simulation_data$floor_area^2)
simulation_data$year.built..year. <- as.factor(simulation_data$year.built..year.)
simulation_data$hvac_cooling_ton <- simulation_data$average_cooling_capacity_per_HVAC_unit.kW./3.51685284

## convert year to decades
simulation_data$year.built..year. <- as.numeric(as.character(simulation_data$year.built..year.))
#simulation_data$post1980 <- ifelse(simulation_data$year.built..year. >1980, 1, 0)
simulation_data$year.built..year. <- ifelse(simulation_data$year.built..year. < 1960, "1950s", 
                                            ifelse(simulation_data$year.built..year. < 1970 & simulation_data$year.built..year. >= 1960, "1960s",
                                                   ifelse(simulation_data$year.built..year. < 1980 & simulation_data$year.built..year. >= 1970, "1970s",
                                                          ifelse(simulation_data$year.built..year. < 1990 & simulation_data$year.built..year. >= 1980, "1980s",
                                                                 ifelse(simulation_data$year.built..year. < 2000 & simulation_data$year.built..year. >= 1990, "1990s", simulation_data$year.built..year.)))))
simulation_data$year.built..year. <- as.factor(simulation_data$year.built..year.)


## investment cost model
ic_model <- lm(log(investment.cost....) ~  year.built..year. + scale(log(floor_area)) + scale(floor_area_2) + package + 
                 scale(log(baseline.annual.site.energy.use.intensity..kWh.m2.)) + building.type, data = simulation_data)
summary(ic_model)

## residual plot
par(mfrow=c(1,1))
plot(fitted(ic_model), resid(ic_model), xlab = "All (Fitted)", ylab = "All (Residual)")
abline(col = "red", h=0)

## 
a1 <- bld_data
a2 <- bld_data
a3 <- bld_data
a4 <- bld_data
a1$package <- "Basic"
a2$package <- "Mid"
a3$package <- "Advanced1"
a4$package <- "Advanced2"

a1$Predicted_Investment_Cost_Basic <- round(exp(predict.lm(ic_model, a1)),6)
a2$Predicted_Investment_Cost_Mid <- round(exp(predict.lm(ic_model, a2)),6)
a3$Predicted_Investment_Cost_Advanced1 <- round(exp(predict.lm(ic_model, a3)),6)
a4$Predicted_Investment_Cost_Advanced2 <- round(exp(predict.lm(ic_model, a4)),6)

t <- as.data.frame(cbind(a1[, "Predicted_Investment_Cost_Basic"], a2[, "Predicted_Investment_Cost_Mid"], 
                         a3[, "Predicted_Investment_Cost_Advanced1"], a4[, "Predicted_Investment_Cost_Advanced2"]))
colnames(t) <- c("Predicted_Investment_Cost_Basic", "Predicted_Investment_Cost_Mid", "Predicted_Investment_Cost_Advanced1", "Predicted_Investment_Cost_Advanced2")
bld_data <- cbind(bld_data, t)
View(bld_data)















