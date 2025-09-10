# Exercise One

# Part One
# U
library(readr)
UNRATE <- read_csv("C:/Users/joshu/Downloads/UNRATE.csv")
U = UNRATE$UNRATE
# 1. Calculations for U:
mean(UNRATE$UNRATE)
# Mean: 5.689783
var(UNRATE$UNRATE)
# Variance: 2.923748
print(sqrt(sum((UNRATE$UNRATE - mean(UNRATE$UNRATE)) ^ 2/(length(UNRATE$UNRATE) - 1)))
      /sqrt(length(UNRATE$UNRATE)))
# Standard Error: 0.05637363
library(e1071)
skewness(U)
# skewness: 0.8544026 
kurtosis(U)
# kurtosis: 1.047225

# delta y
RGDP <- read_csv("C:/Users/joshu/Downloads/A191RL1Q225SBEA.csv")
head(RGDP)
y = RGDP$A191RL1Q225SBEA
mean(y)
# Mean: 3.209709
var(y)
# Variance: 20.78854
print(sqrt(sum((y - mean(y)) ^ 2/(length(y) - 1)))
      /sqrt(length(y)))
# Standard Error:  0.2593778
skewness(y)
# Skewness: 0.1210887
kurtosis(y)

# Part Two
# U
median(U)
#5.5

# delta y
median(y)
#3.1

# Part 3:
H = hist(U,
     main = "Unemployment Rate US",
     xlab = "Unemployment Rate",
     breaks = seq(min(U),max(U), length.out = 42),
     axes=FALSE, ylim=c(0,100))
axis(side=1, at=seq(min(U),max(U), length.out=42))
axis(side=2, pretty(0:100))

min(U)
max(U)

H2 = hist(y,
         main = "% Change Real GDP US",
         xlab = "% Change Real GDP",
         breaks = seq(min(y),max(y), length.out = 64),
         axes=FALSE, ylim=c(0,60))
axis(side=1, at=seq(min(y),max(y), length.out=64))
axis(side=2, pretty(0:60))


min(y)
max(y)

# Part 5
library(readr)
INDPRO <- read_csv("C:/Users/joshu/Downloads/INDPRO.csv")
I = INDPRO$INDPRO
I1 <- INDPRO[1:634,]
I2 <- INDPRO[635:1268,]

mean(I1$INDPRO)
# Mean of first half: 15.6409
mean(I2$INDPRO)
# Mean of second half: 76.41148

library(dplyr)
PI <- INDPRO %>%
  arrange(DATE) %>%
  mutate(pct_change = (I - lag(I))/lag(I) * 100)

mean(PI$pct_change, na.rm = T)

PI1 <- PI[1:634,]
PI2 <- PI[635:1268,]

mean(PI1$pct_change, na.rm = T)
# First half % change mean: 0.3593831
mean(PI2$pct_change, na.rm = T)
# Second half % change mean: 0.1602088




