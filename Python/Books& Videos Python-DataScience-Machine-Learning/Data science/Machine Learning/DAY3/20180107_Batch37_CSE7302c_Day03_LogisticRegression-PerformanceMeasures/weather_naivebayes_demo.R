setwd("D:\\Batch37\\CSE7302c\\Day03")
weather <- read.csv("weather.txt", header = T, sep = ",")
names(weather)
summary(weather)
str(weather)

library(e1071)
golforno <- naiveBayes(Play.golf~., data = weather)
golforno

testpoint <- data.frame("Outlook" = "Rainy",
                        "Temperature" = "Mild",
                        "Humidity" = "High",
                        "Windy" = FALSE,
                        "Play.golf" = "Yes")
predict(golforno, testpoint, type = "raw")
