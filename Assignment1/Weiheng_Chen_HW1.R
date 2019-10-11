# Name: Weiheng Chen (001802389)
# Section: 01
# Course ID: IE 6600 18004


# Question 1

#Import Dataset
#Transfor blank space to NA
NYPD_Motor_Vehicle_Collisions_._Crashes <- read.csv("C:/Users/weihe/OneDrive/Desktop/IE6600/Class4-Sep17/NYPD_Motor_Vehicle_Collisions_-_Crashes.csv", na = "")
View(NYPD_Motor_Vehicle_Collisions_._Crashes)
nydata <- NYPD_Motor_Vehicle_Collisions_._Crashes

# 1.Compute the number of persons who got injured in each borough

# Pipe operator for clean code

library(magrittr)
library(dplyr)

nydata %>%
    filter(!is.na(BOROUGH))%>% #Remove NA
    group_by(BOROUGH)%>%
    summarise(TOTAL.INJURED_EACH.BOROUGH = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE))%>%
    arrange(desc(TOTAL.INJURED_EACH.BOROUGH))

# 2.Compute the number of persons who got killed in each borough

nydata %>%
    filter(!is.na(BOROUGH))%>% #Remove NA
    group_by(BOROUGH)%>%
    summarise(TOTAL.KILLED_EACH.BOROUGH = sum(NUMBER.OF.PERSONS.KILLED, na.rm=TRUE))%>%
    arrange(desc(TOTAL.KILLED_EACH.BOROUGH))

# 3.List top 10 on street location from Bronx from where most of the injuries were reported

# If Group by the on street 'LOCATION' column, we could directly rank by 'NUMBER.OF.PERSONS.INJURED'.
df1 <- nydata%>%
    filter(BOROUGH == "BRONX")%>% 
    arrange(desc(NUMBER.OF.PERSONS.INJURED))%>%
    head(10)%>%
    select(LOCATION, ON.STREET.NAME, NUMBER.OF.PERSONS.INJURED)
df1
# Conclusion: From the result, Location of the top1 isn't clear, which we only know its on.street.name.
# If we want to remove NA, we could change the codes as following:
df2 <- nydata%>%
    filter(BOROUGH == "BRONX" & !is.na(LOCATION) & !is.na(ON.STREET.NAME))%>% #Remove NA
    arrange(desc(NUMBER.OF.PERSONS.INJURED))%>%
    head(10)%>%
    select(LOCATION, ON.STREET.NAME, NUMBER.OF.PERSONS.INJURED)
df2

# If Group by "ON.STREET.NAME", we need to use summarize to calucate 'sum_injured' first, 
# and then rank by sum_injured of each "ON.STREET.NAME".
df3 <- nydata%>%
    filter(BOROUGH == "BRONX" & !is.na(ON.STREET.NAME))%>% #Remove NA 
    group_by(ON.STREET.NAME)%>%
    summarise(sum_injured = sum(NUMBER.OF.PERSONS.INJURED, na.rm=TRUE))%>%
    arrange(desc(sum_injured))
## List top 10
df3[1:10,]

# 4. Create a new column "Address" 
#    and populate them with combined values of BOROUGH, ZIP CODE, ON STREET NAME 
#    with each string seperated by a coma
# Keep "" space
nydata1 <- read.csv("C:/Users/weihe/OneDrive/Desktop/IE6600/Class4-Sep17/NYPD_Motor_Vehicle_Collisions_-_Crashes.csv", stringsAsFactors=FALSE)
library(stringr)
nydata1$Address <- str_c(nydata1$BOROUGH, nydata1$ZIP.CODE, nydata1$ON.STREET.NAME, sep = ",")
View(nydata1)

# 5. Calculate the average of number of people injured according to time in a day
#    and print the top 10 hours of the day where the average was maximum in the Manhattan borough

library(lubridate)
df4 <- nydata%>%
    filter(BOROUGH == "MANHATTAN"  & !is.na(DATE) & !is.na(TIME))%>% #Remove NA
    select(BOROUGH, DATE, TIME, NUMBER.OF.PERSONS.INJURED)
df4$TIME1 <- format(strptime(df4$TIME, "%H:%M")) #Transfor TIME to Standard Format for the next step, please Ingore the DATE of TIME1
df5 <- df4%>%
    mutate(HOUR = hour(TIME1))%>% #Using hour() function to get "HOUR"
    group_by(HOUR)%>%
    summarise(avg_injured = mean(NUMBER.OF.PERSONS.INJURED), na.rm = TRUE)%>%
    arrange(desc(avg_injured))%>%
    head(10)
df5

# 6. How does the number of person injured, and the number of persons killed vary according to each year for all the boroughs?

# (1) For the Number of Person Injured
df6 <- nydata %>%
    mutate(YEAR = substr(DATE,7,10))%>% #Obtain YEAR from DATE according to the numbers' location
    group_by(YEAR)%>%
    summarise(sum_injured = sum(NUMBER.OF.PERSONS.INJURED, na.rm=TRUE))
# Trend
library(ggplot2) #USe graph to present the trend
ggplot(df6)+
    geom_line(aes(x = YEAR, y = sum_injured), group=1)
# Conclusion: 
# <1> From 2012 to 2013, the number of persons injuered increased sharply from 2012 to 2013 (the maximum of this period was 55000),then slowly went down between 2013 and 2015. 
# <2> Laterly,between 2016 and 2018, it smoothly went up from 60000, and the peak of this period appeared in 2018 which was around 62500.
# <3> Then from 2018 to 2019, it decreases sharply. And now in 2019, it is below 45000

# (2) For the Number of Person Killed
df7 <- nydata %>%
    mutate(YEAR=substr(DATE,7,10))%>% #Obtain YEAR from DATE according to the numbers' location
    group_by(YEAR)%>%
    summarise(sum_killed=sum(NUMBER.OF.PERSONS.KILLED, na.rm=TRUE))
# Trend
ggplot(df7)+
    geom_line(aes(x = YEAR, y =sum_killed), group=1) #USe graph to present the trend
# Conclusion: 
# <1> From 2012 to 2013, the number of persons yearly killed increased sharply, which even came close to 300.
# <2> Between 2013 and 2015, that went down slowly. 
# <3> Laterly, when time came to 2016 and 2017, it plateaued around 250. 
# <4> Then from 2017 to 2019, it dropped relatively largely to 160.

# Question 2
# Import Dataset
# Analyze Boston - Crime Incident Reports (August 2015 - To Date) (Source: New System)
# URL:  https://data.boston.gov/dataset/6220d948-eae2-4e4b-8

# Transfor blank space to NA
BostonCrime <- read.csv("C:/Users/weihe/OneDrive/Desktop/IE6600/BostonCrimeReport_0922.csv", na = "")
View(BostonCrime)

# Select OFFENSE_CODE_GROUP, DISTRICT and Do the Population
library(dplyr)
df8 <- BostonCrime %>%
    select(OFFENSE_CODE_GROUP, DISTRICT) %>%
    filter(!is.na(OFFENSE_CODE_GROUP) & !is.na(DISTRICT))%>% #Remove NA
    group_by(OFFENSE_CODE_GROUP, DISTRICT)%>%
    summarize(COUNT = n())
View(df8)

# Convert to Matrix
library(reshape2)
m1 <- dcast(df8, OFFENSE_CODE_GROUP ~ DISTRICT, value.var = "COUNT")
View(m1)
