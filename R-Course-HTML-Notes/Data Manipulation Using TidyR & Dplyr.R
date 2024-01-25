install.packages('dplyr')
install.packages('nycflights13')
library(dplyr)
library(nycflights13)
summary(flights)

dim(flights)
names(flights)

head(filter(flights,month ==11,day ==3,carrier =="AA"))

head(flights[flights$month == 11 & flights$day == 3 & flights$carrier == 'AA', ])

slice(flights,1:10)

head(arrange(flights,year,month,day,air_time))
head(arrange(flights,desc(dep_delay)))

head(select(flights,carrier))

head(rename(flights,airline_car=carrier))

distinct(select(flights,carrier))

comp <- c(1,1,1,2,2,2,3,3,3)
yr <- c(1998,1999,2000,1998,1999,2000,1998,1999,2000)
q1 <- runif(9, min=0, max=100)
q2 <- runif(9, min=0, max=100)
q3 <- runif(9, min=0, max=100)
q4 <- runif(9, min=0, max=100)

df <- data.frame(comp=comp,year=yr,Qtr1 = q1,Qtr2 = q2,Qtr3 = q3,Qtr4 = q4)
df

head(df %>% gather(Quarter,Revenue,Qtr1:Qtr4))

head(gather(df,Quarter,Revenue,Qtr1:Qtr4))
