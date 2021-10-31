
#1.C########
install.packages("quantmod")
library(zoo)

library(quantmod)

from.dat <- as.Date("12/31/12", format="%m/%d/%y") 
to.dat <- as.Date("12/31/17", format="%m/%d/%y") 
getSymbols("DL", src="yahoo", from = from.dat, to = to.dat)

dataDL <- dailyReturn(DL, subset=NULL, type='log', leading=TRUE)*100

length (dataDL)

options(max.print = 20000)

zoo::plot.zoo(dataDL, main = "China Distance Education index log-return")

#1.d#####

install.packages("moments")
library("moments")
install.packages("e1071")
library("e1071")

m <-mean(dataDL)
std <-sqrt(var(dataDL))
var <- var(dataDL)
sk <- skewness(dataDL, na.rm = FALSE)
kurt <- kurtosis(dataDL, na.rm = FALSE, type = 3)

hist(dataDL, density=20, breaks = 20, prob=TRUE,
     ylim=c(0, 0.18), 
     main="DL log-returns (%) density")
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#2.b - ESTIMATION VOLATILITY #####

install.packages("MSGARCH")
library("MSGARCH")
library("scales")
library(zoo)

CreateSpec(variance.spec = list(model = c("sGARCH", "sGARCH")),
           distribution.spec = list(distribution = c("std", "std")),
           switch.spec = list(do.mix = FALSE, K = NULL), 
           constraint.spec = list(fixed = list(), regime.const = NULL), 
           prior = list(mean = list(m), sd = list(std)))

est_GARCH <- MSGARCH::CreateSpec(variance.spec = list(models = "sGARCH"), 
                                  distribution.spec = list(distributions = "std"))

DL_fit_GARCH <- MSGARCH::FitML(spec = est_GARCH, data = dataDL)



summary(DL_fit_GARCH)

num <- length(dataDL)

MSGARCH::UncVol(DL_fit_GARCH)

##Conditional Volatility ####

ConVOL <- predict(DL_fit_GARCH, nahead = 1, do.return.draws = FALSE)

ConVOL$vol

vol <- Volatility(DL_fit_GARCH)

head(vol, n=10L)
plot(vol)
# 2.c Modeling GJRGARCH ####

MSGARCH::CreateSpec(variance.spec = list(model = c("gjrGARCH", "gjrGARCH")),
           distribution.spec = list(distribution = c("std", "std")),
           switch.spec = list(do.mix = FALSE, K = NULL), 
           constraint.spec = list(fixed = list(), regime.const = NULL), 
           prior = list(mean = list(m), sd = list(std)))


est_GJRGARCH <- MSGARCH::CreateSpec(variance.spec = list(models = "gjrGARCH"), 
                                 distribution.spec = list(distributions = "std"))

DL_fit_GJRGARCH <- MSGARCH::FitML(spec = est_GJRGARCH, data = dataDL)

summary(DL_fit_GJRGARCH)


## 3a & 3b VAR RISK MEASURE##################################


VaR_DLGARCH <- MSGARCH::Risk(DL_fit_GARCH, alpha = 0.05, do.its = TRUE)

 par(mfrow = c(2, 1))

plot(zoo::zoo(dataDL, order.by = index(dataDL)), type = "p",
     col = ifelse(as.vector(dataDL) < as.vector(VaR_DLGARCH$VaR), "red", "black"),
     xlab = "Date",ylab = "%", main = "GARCH 5% VaR")

lines(zoo::zoo(VaR_DLGARCH$VaR, order.by = index(dataDL)), col = scales::alpha(rgb(1, 0, 0), 0.3))

#GARCH has 58 red dots, from 1260 obs it gives 4,60%.

VaR_DLGJRGARCH <- MSGARCH::Risk(DL_fit_GJRGARCH, alpha = 0.05, do.its = TRUE)

par(mfrow = c(2, 1))

plot(zoo::zoo(dataDL, order.by = index(dataDL)), type = "p",
     col = ifelse(as.vector(dataDL) < as.vector(VaR_DLGJRGARCH$VaR), "red", "black"),
     xlab = "Date",ylab = "%", main = "GJRGARCH 5% VaR")

lines(zoo::zoo(VaR_DLGJRGARCH$VaR, order.by = index(dataDL)), col = scales::alpha(rgb(1, 0, 0), 0.3))

#GJRGARCH has 57 red dots, from 1260 obs it gives 4,44%.

# 3c VaR and ES ####

VaR_DLGARCH_fake <- MSGARCH::Risk(DL_fit_GARCH, alpha = 0.05, do.its = FALSE)

VaR_DLGARCH_fake <- MSGARCH::Risk(DL_fit_GARCH, alpha = 0.01, do.its = FALSE)


### 4.a - COMPLEX MSGJR MODEL ######

spec_COMPLX <- MSGARCH::CreateSpec(variance.spec = list(model = c("gjrGARCH")),
                                  distribution.spec = list(distribution = "sstd"), 
                                  switch.spec = list(K = 2))

fit_COMPLX <- MSGARCH::FitML(spec = spec_COMPLX, data = dataDL)
VaR_COMPLX <- MSGARCH::Risk(fit_COMPLX, alpha = 0.01, do.its = TRUE)

par(mfrow = c(1, 1))  
plot(zoo::zoo(dataDL, order.by = index(SMI)), type = "p",
     col = ifelse(as.vector(dataDL) < as.vector(VaR_COMPLX$VaR), "red", "black"),
     xlab = "Date", ylab = "%", main = "MSGJR 1% VaR")

CVaR_dataDL <- ES(R = dataDL, p = 0.95, method = "historical")


lines(zoo::zoo(VaR_COMPLX$VaR,order.by = index(dataDL)), col = alpha(rgb(1, 0, 0), 0.3))

GAS::BacktestVaR(dataDL, VaR = VaR_COMPLX$VaR, alpha = 0.01)$LRcc


### 4.b - forecasting 10 days #####

forecastDL <- predict(fit_COMPLX, nahead = 10, do.return.draws = FALSE)

forecastDL$vol


# Create a scatter plot
chart.Scatter(returns_bonds, returns_equities)

# Find the correlation
cor(returns_equities, returns_bonds)

# Merge returns_equities and returns_bonds 
returns <- merge(returns_bonds, returns_equities)
returns

# Find and visualize the correlation using chart.Correlation
chart.Correlation(returns)

# Visualize the rolling estimates using chart.RollingCorrelation
chart.RollingCorrelation(returns_bonds, returns_equities, width = 24)



