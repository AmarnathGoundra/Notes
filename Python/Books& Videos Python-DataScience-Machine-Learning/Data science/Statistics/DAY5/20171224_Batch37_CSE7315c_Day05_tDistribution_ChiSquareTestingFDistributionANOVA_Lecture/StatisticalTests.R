setwd("D:/Batch37/CSE7315c-Stats/Day05")

# Confidence Intervals in t-distribution

dosage <- c(98.6, 102.1, 100.7, 102, 97, 103.4, 98.9, 101.6, 102.9, 105.2)
dosage
sd(dosage)
ttest <- t.test(dosage, conf.level = 0.95, mu = 100)
ttest
ttest$statistic
ttest$parameter
ttest$conf.int
ttest$p.value
ttest$estimate
ttest$null.value
ttest$alternative
ttest$method
ttest$data.name
pt(1.5824,9)

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

# 2-sample UNPAIRED t-test

rifampicinT <- c(1.15, 1.15, 0.92, 1.28, 0.72, 0.67, 1, 0.79,
                 0.76, 0.95, 0.67, 0.82, 1.06, 1.21, 0.82)
rifampicinC <- c(0.81, 0.56, 0.46, 1.06, 0.45, 0.43, 0.43,
                 0.88, 0.37, 0.54, 0.73, 0.73, 0.68, 0.43, 0.93)
ttest2U <- t.test(rifampicinT,rifampicinC, conf.level = 0.95,
                 var.equal = TRUE)
ttest2U

# 2-sample PAIRED t-test

treatmentA <- c(63, 54, 79, 68, 87, 84, 92, 57, 66, 53, 76, 63)
treatmentB <- c(55, 62, 108, 77, 83, 78, 79, 94, 69, 66, 72, 77)
ttest2P <- t.test(treatmentA, treatmentB, conf.level = 0.95, paired = TRUE)
ttest2P

# Critical chi-square value at a specified confidence level and degrees of freedom
qchisq(0.95,4)
qchisq(0.99,4)

# Critical F value at a specified confidence level and degrees of freedom
qf(0.025,9,11,lower.tail = FALSE)
qf(0.025,9,11)
qf(0.975,9,11,lower.tail = FALSE)
qf(0.975,9,11)
qf(0.9,2,24)

# Generate F tables (OPTIONAL)
# Code from http://wiki.socr.umich.edu/index.php/SMHS_ProbabilityDistributions#Generating_Probability_Tables
# Define the right-tail probability of interest, alpha
right_tail_p <- 0.5

# Define the vectors storing the indices corresponding to numerator (n1)
# and denominator (n2, row) degrees of freedom for F(alpha, n1, n2).
# Note the Inf corresponds to Infinity.

n1 <- c(1,2,3,4,5,6,7,8,9,10,11,12,15,20,24,30,40,60,120,Inf)
n2 <- c(1:30,40,60,120,Inf)

# Define precision (4-decimal point accuracy)
options(digits=4)

# Generate an empty matrix of critical F-values
f_table <- matrix(ncol = length(n1), nrow = length(n2))

# Use the F-Distribution quantile function to fill in the matrix values in a nested 2-loop
# Recall that the density (df), distribution function (pf), quantile function (qf) and
# random generation (rf) for the F-distribution

for (i in 1:length(n2)){
    for (j in 1:length(n1)){
        f_table[i,j] <- qf(right_tail_p, n1[j], n2[i], lower.tail = FALSE)
    }
}

# Print results
f_table

# Label rows and columns
rownames(f_table) <- n2; colnames(f_table) <- n1
f_table

# Save results to a file
# write.table(f_table, file="C:\\User\\f_table.txt")

