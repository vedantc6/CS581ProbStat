install.packages("sqldf")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("psych")

library(sqldf)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(pastecs)
library(psych)

# Load csv data
category_4 = read.csv("Category4Hurricanes - Sheet1.csv", header = TRUE)
category_5 = read.csv("Category5Hurricanes - Sheet1.csv", header = TRUE)

# Changing column names for cateogry_4 dataset
colnames(category_4) <- c("Name", "Season", "Month", "Max_wind_knots", "Max_wind_kmh", "Max_wind_mph", "Min_pressure_mbar")

# Changing column names for cateogry_5 dataset
colnames(category_5) <- c("Name", "Dates", "Duration_hours", "WindSpeedsMPH", "PressurehPA", "Affected_Areas", "Deaths", "DamageUSDMillions")

# Clean month column for Category 4
category_4$Month <- gsub(" .*$", "",category_4$Month)
category_4$Month <- gsub(",", "", category_4$Month)

# Clean min_pressure column for Category 4
from <- c("≤ ","-", "–")
to <- c("")

gsub_func <- function(pattern, replacement, x) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x)
  x
}

category_4$Min_pressure_mbar <- as.numeric(gsub_func(from, to, category_4$Min_pressure_mbar))

#  Clean Category 5 data
category_5$Dates <- gsub("†", "", category_5$Dates)
category_5$Year <- as.numeric(gsub(".+, ", "", category_5$Dates))
category_5$Month <- gsub("[0-9, ].+", "", category_5$Dates)
category_5$DamageUSDMillions <- gsub(">", "", category_5$DamageUSDMillions)
category_5$WindSpeedsMPH <- as.numeric(gsub("([0-9]+).*", "\\1", category_5$WindSpeedsMPH))
category_5$PressurehPA <- as.numeric(gsub("([0-9]+).*", "\\1", category_5$PressurehPA))

category_5$DamageUSDMillions <- gsub("([0-9]+).*", "\\1", category_5$DamageUSDMillions)
category_5$DamageUSDMillions <- gsub("Extensive", "", category_5$DamageUSDMillions)
category_5$DamageUSDMillions <- as.numeric(gsub("\\$", "", category_5$DamageUSDMillions))

# Making decade bins for category 4 and 5
decades <- seq(1850, 2020, 10)

convert_to_decades <- function(x){
  for (index in 2:length(decades)){
    if (x <= decades[index] && x >= decades[index-1]){
      return(decades[index])
    }
  }
}

category_4$decade <- unlist(lapply(category_4$Season, convert_to_decades))
category_5$decade <- unlist(lapply(category_5$Year , convert_to_decades))

#---------------------------------------------------------------#
#-----------------EDA FOR CATEGORY 4 HURRICANE------------------#
#---------------------------------------------------------------#
# Histogram/Barplot - if count available
# hist(category_4$decade,breaks = 20)
ggplot(data=cat_4_decade, aes(x=decade, y=count_dec)) + geom_bar(stat="identity")


cormat <- cor(category_4[, c("Max_wind_knots", "Max_wind_kmh", "Max_wind_mph", "Min_pressure_mbar")], use = "complete.obs", method = "pearson")
melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#---------------------------------------------------------------#
#-----------------EDA FOR CATEGORY 5 HURRICANE------------------#
#---------------------------------------------------------------#
cormat5 <- cor(category_5[, c("Duration_hours", "WindSpeedsMPH", "PressurehPA", "Deaths", "DamageUSDMillions")], use = "complete.obs", method = "pearson")
melted_cormat5 <- melt(cormat5)

ggplot(data = melted_cormat5, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

# We can assume occurence of hurricanes is random in nature and independent of each other
# Thus, we can try to fit poisson distribution to the dataset

#---------------------------------------------------------------#
#--------POISSON DISTRIBUTION FOR CATEGORY 4 HURRICANES---------#
#---------------------------------------------------------------#
#============================YEARLY=============================#
# lambda in this case is defined as the average count of hurricanes in a year
# Finding lambda for poisson distribution (yearly)
count_yearly <- sqldf('SELECT Season, count(Season) as count_yearly FROM category_4 GROUP BY Season')
category_4 <- merge(category_4, count_yearly, by = "Season", all.x = TRUE)
lambda_yearly <- mean(category_4$count_yearly)

# Plotting cdf for the poisson distribution - yearly
cdf_yearly <- c()
for (i in 1:20){
  cdf_yearly[i] <- dpois(i, lambda_yearly)
}

plot(cdf_yearly, main = "Poisson(2.083) with its approximating normal curve")

# Adding an approximating normal distribution line for comparison
normden <- function(x){
  dnorm(x, mean = 2, sd = sqrt(2))
}
curve(normden, from = 0, to = 20, add=TRUE, col="red")

#============================DECADE============================#
# Aggregating category_4 data on decade level
cat_4_decade <- sqldf(
  'SELECT Name, decade, avg(Max_wind_knots) as avg_wind_knots,
  avg(Max_wind_kmh) as avg_wind_kmh,
  avg(Max_wind_mph) as avg_wind_mph,
  avg(Min_pressure_mbar) as avg_pressure_mbar,
  count(decade) as count_dec FROM category_4 GROUP BY decade')

# lambda in this case is defined as the average count of hurricane in a decade
# Finding lambda for poisson distribution (decade wise)
lambda_decade <- mean(cat_4_decade$count_dec)

# Plotting cdf for the poisson distribution- decades
cdf_decade <- c()
for (i in 1:20){
  cdf_decade[i] <- dpois(i, lambda_decade)
}

plot(cdf_decade, main = "Poisson(7.06) with its approximating normal curve")

# Adding an approximating normal distribution line for comparison
normden <- function(x){
  dnorm(x, mean = 6.7, sd = sqrt(6.8))
}
curve(normden, from = 0, to = 50, add=TRUE, col="red")

#---------------------------------------------------------------#
#--------POISSON DISTRIBUTION FOR CATEGORY 5 HURRICANES---------#
#---------------------------------------------------------------#
#============================YEARLY=============================#
# Aggregating category_5 data on decade level
cat_5_decade <- sqldf(
  'SELECT Name, decade, avg(Duration_hours) as avg_hours,
  avg(WindSpeeds) as avg_wind_speed,
  avg(Pressure) as avg_pressure,
  Affected_Areas,
  avg(Deaths) as avg_deaths,
  count(decade) as count_dec FROM category_5 GROUP BY decade')

# Finding lambda for poisson distribution (decade wise)
lambda_5_decade <- mean(cat_5_decade$count_dec)

# Plotting cdf for the poisson distribution- decades
cdf_decade <- c()
for (i in 1:20){
  cdf_decade[i] <- dpois(i, lambda_5_decade)
}

plot(cdf_decade, main = "Poisson(3.66) with its approximating normal curve")

# Adding an approximating normal distribution line for comparison
normden <- function(x){
  dnorm(x, mean = 3.3, sd = sqrt(3.7))
}
curve(normden, from = 0, to = 50, add=TRUE, col="red")
