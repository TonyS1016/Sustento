## packages
library(ggplot2)
library(gridExtra)
library(gridGraphics)
library(ggpubr)

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

## histogram plots for investment costs for advanced1
bld_types <- c("Medium Office", "Multi Family House", "Large Office", "Medium Retail", "Small Office", "Small Retail")

par(mfrow=c(3,2))
m_cost <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  cost <- advanced1[advanced1$building.type == bld_types[i], "investment.cost...."]
  mc <- median(cost, na.rm = T)
  h <- hist(cost, col = "lightblue", main = paste("Histogram of Investment Cost: ", bld_types[i], sep = ""), ylim = c(0, 52))
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_cost[i] <- mc
}

m_cost <- data.frame(m_cost, bld_types)
plot1 <- ggplot(data=m_cost, aes(x=m_cost, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Investment Cost ($)") + ylab(NULL) + theme_minimal() + xlim(0, 300000) + ggtitle("HVAC traditional (advanced 1) Package") + 
  geom_text(aes(label=round(m_cost)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)

## histogram plots for investment costs for basic
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
plot2 <- ggplot(data=m_cost, aes(x=m_cost, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Investment Cost ($)") + ylab(NULL) + theme_minimal() + xlim(0, 300000) + ggtitle("Basic Package") + 
  geom_text(aes(label=round(m_cost)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)

## histogram plots for investment costs for mid package
par(mfrow=c(3,2))
m_cost <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  cost <- mid[mid$building.type == bld_types[i], "investment.cost...."]
  mc <- median(cost, na.rm = T)
  h <- hist(cost, col = "lightblue", main = paste("Histogram of Investment Cost: ", bld_types[i], sep = ""), ylim = c(0, 52))
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_cost[i] <- mc
}

m_cost <- data.frame(m_cost, bld_types)
plot3 <- ggplot(data=m_cost, aes(x=m_cost, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Investment Cost ($)") + ylab(NULL) + theme_minimal() + xlim(0, 400000) + ggtitle("Mid Package") + 
  geom_text(aes(label=round(m_cost)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)

## histogram plots for investment costs for advanced 2 package
par(mfrow=c(3,2))
m_cost <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  cost <- advanced2[advanced2$building.type == bld_types[i], "investment.cost...."]
  mc <- median(cost, na.rm = T)
  h <- hist(cost, col = "lightblue", main = paste("Histogram of Investment Cost: ", bld_types[i], sep = ""), ylim = c(0, 45))
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_cost[i] <- mc
}

m_cost <- data.frame(m_cost, bld_types)
plot4 <- ggplot(data=m_cost, aes(x=m_cost, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Investment Cost ($)") + ylab(NULL) + theme_minimal() + xlim(0, 400000) + ggtitle("HVAC Elec (advanced 2) Package") + 
  geom_text(aes(label=round(m_cost)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)


## Arrange plots
plot <- ggarrange(plot2, plot3, plot1, plot4, ncol = 2, nrow = 2, common.legend = T, legend = "bottom")

## save images
ggsave("Median Investment Cost by Package and Bld_type.png", plot = plot, width = 10, height = 7)









## histogram plots for annual savings for advanced1
par(mfrow=c(3,2))
m_savings <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  savings <- advanced1[advanced1$building.type == bld_types[i], "annual.energy.cost.saving...."]
  mc <- median(savings, na.rm = T)
  h <- hist(savings, col = "lightblue", main = paste("Histogram of Energy Cost Savings (Annual): ", bld_types[i], sep = ""), ylim = c(0, 60))
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_savings[i] <- mc
}

m_savings <- data.frame(m_savings, bld_types)
plot1 <- ggplot(data=m_savings, aes(x=m_savings, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Energy Savings ($)") + ylab(NULL) + theme_minimal() + xlim(0, 300000) + ggtitle("HVAC traditional (advanced 1) Package") + 
  geom_text(aes(label=round(m_savings)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)

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

## histogram plots for annual savings for mid package
par(mfrow=c(3,2))
m_savings <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  savings <- mid[mid$building.type == bld_types[i], "annual.energy.cost.saving...."]
  mc <- median(savings, na.rm = T)
  h <- hist(savings, col = "lightblue", main = paste("Histogram of Energy Cost Savings (Annual): ", bld_types[i], sep = ""), ylim = c(0, 62))
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_savings[i] <- mc
}

m_savings <- data.frame(m_savings, bld_types)
plot3 <- ggplot(data=m_savings, aes(x=m_savings, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Energy Savings ($)") + ylab(NULL) + theme_minimal() + xlim(-20000, 200000) + ggtitle("Mid Package") + 
  geom_text(aes(label=round(m_savings)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)


## histogram plots for annual savings for advanced2 package
par(mfrow=c(3,2))
m_savings <- numeric(length(bld_types))
for (i in 1:length(bld_types)){
  savings <- advanced2[advanced2$building.type == bld_types[i], "annual.energy.cost.saving...."]
  mc <- median(savings, na.rm = T)
  h <- hist(savings, col = "lightblue", main = paste("Histogram of Energy Cost Savings (Annual): ", bld_types[i], sep = ""), ylim = c(0, 62))
  abline(v = mc, col = "red", lty = 2)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
  m_savings[i] <- mc
}

m_savings <- data.frame(m_savings, bld_types)
plot4 <- ggplot(data=m_savings, aes(x=m_savings, y=bld_types)) + geom_bar(stat="identity", fill="lightblue") + 
  xlab("Median Energy Savings ($)") + ylab(NULL) + theme_minimal() + xlim(-20000, 200000) + ggtitle("HVAC Elec (advanced 2) Package") + 
  geom_text(aes(label=round(m_savings)), color = "black", hjust=-.5, size = 3) + 
  theme(plot.title = element_text(size=15), axis.title.x = element_text(size=8),)




## Arrange plots
plot <- ggarrange(plot2, plot3, plot1, plot4, ncol = 2, nrow = 2, common.legend = T, legend = "bottom")

## save images
ggsave("Median Annual Energy Savings by Package and Bld_type.png", plot = plot, width = 10, height = 7)















