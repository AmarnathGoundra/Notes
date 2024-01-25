setwd("C:/Users/Classroom1/Desktop/Batch 37/Day04")

pbinom(14,20,0.5, lower.tail = FALSE, log.p = FALSE)
pnorm(14.5,10,sqrt(5), lower.tail = FALSE)
(14.5-10)/sqrt(5)
pnorm((14.5-10)/sqrt(5),0,1, lower.tail = FALSE)

# pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
# US Commuters
pbinom(104, 150, 0.75, lower.tail = TRUE, log.p = FALSE)
dbinom(104, 150, 0.75, log = FALSE)
pnorm(104.5, 150*0.75, sqrt(150*0.75*0.25))
pbinom(120, 150, 0.75, lower.tail = TRUE, log.p = FALSE)-
  pbinom(109, 150, 0.75, lower.tail = TRUE, log.p = FALSE)
pnorm(120.5, 150*0.75, sqrt(150*0.75*0.25))-
  pnorm(109.5, 150*0.75, sqrt(150*0.75*0.25))
pbinom(96, 150, 0.75, lower.tail = FALSE, log.p = FALSE)
1-pnorm(95.5, 150*0.75, sqrt(150*0.75*0.25))

# Serum Cholesterol
1-pnorm(230, 211, 46)
1-pnorm(230, 211, 46/sqrt(25))
pnorm(2.33, 0, 1)

# Percentile or Percent
qnorm(0.8, 0, 1)
qnorm(0.975, 0, 1)
qnorm(0.025, 0, 1)
qnorm(0.05, 0, 1)
qnorm(0.95, 0, 1)

# Confidence Intervals in t-distribution

# compoff <- c(6, 21, 17, 20, 7, 0, 8, 16, 29, 3, 8, 12, 11, 9, 21, 25, 15, 16, 6, 21, 17, 20, 7, 0, 8, 16, 29, 3, 8, 12, 11, 9, 21, 25, 15, 16,6, 21, 17, 20, 7, 0, 8, 16, 29, 3, 8, 12, 11, 9, 21, 25, 15, 16, 6, 21, 17, 20, 7, 0, 8, 16, 29, 3, 8, 12, 11, 9, 21, 25, 15, 16,6, 21, 17, 20, 7, 0, 8, 16, 29, 3, 8, 12, 11, 9, 21, 25, 15, 16, 6, 21, 17, 20, 7, 0, 8, 16, 29, 3, 8, 12, 11, 9, 21, 25, 15, 16)
compoff <- c(6, 21, 17, 20, 7, 0, 8, 16, 29, 3, 8, 12, 11, 9, 21, 25, 15, 16)
compoff
t.test(compoff, conf.level = 0.90)
ttest <- t.test(compoff)
ttest$statistic
sd(compoff)
ttest$parameter

# Critical t value at a specified confidence level and degrees of freedom
conf.level = 0.90
df = 17
qt((1-conf.level)/2, df)
qt(0.025,24)
qt(0.025, 19)
qt(0.975, 19)

z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
xbar = 80
sdx = 4
c(xbar - z * sdx, xbar + z * sdx)
