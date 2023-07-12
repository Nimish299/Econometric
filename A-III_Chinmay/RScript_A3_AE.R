# Loading excel file into the R-Studio
#library(readxl)
#AE_ASSIGNMENT_3 <- read_excel("../AE_ASSIGNMENT_3.xlsx", sheet = "Data used for Assignment")

#Loading all required libraries
library(ggplot2)
library (dynlm)
library (knitr)
library (broom)
library (tibble)
library (purrr)
library(lmtest) 
library(urca)
library(forecast)

# Forming dataframe of variables of interest
#rfr <- AE_ASSIGNMENT_3$`Weighted Avg Yield (per cent)
#(Risk-Free Rate)`
#coh <- AE_ASSIGNMENT_3$`Cash on Hand with Banks`
#assgn3 <- cbind(rfr,coh)

# Time-series Setting 
#assgn3.ts <- ts(assgn3, frequency = 24, start = c(0,0), end = c(13,13))
#assgn3.ts
#assgn3_rfr <- assgn3.ts[,"rfr"]
#assgn3_coh <- assgn3.ts[,"coh"]

# Plotting time-series graphs of variables of interest
rfrg <- ggplot(AE_ASSIGNMENT_3, aes(Date, rfr)) +
  geom_point(na.rm=TRUE, color="purple", size=1) + 
  ggtitle("Weighted Avg Yield (per cent)
(Risk-Free Rate)") +
  xlab("Date") + ylab("Risk-Free Rate")+
  stat_smooth(method = "loess")

cohg <- ggplot(AE_ASSIGNMENT_3, aes(Date, coh)) +
  geom_point(na.rm=TRUE, color="purple", size=1) + 
  ggtitle("Cash on Hand with Banks") +
  xlab("Date") + ylab("Cash on Hand")+
  stat_smooth(method = "loess")

plot(rfrg)
plot(cohg)

# Checking ARDL (0, 0) model
assgn3.model01 <- dynlm(assgn3_rfr ~ assgn3_coh, data=assgn3.ts)
sum.gen01 <- summary(assgn3.model01)
kable(tidy(sum.gen01), digits=4, caption="ARDL00 model")
ehat01 <- resid(assgn3.model01)
corrgm01 <- acf(ehat01)
plot(corrgm01)
# Autocorrelation is present for 10+ lags, hence including rfr variable is important

# Checking ARDL (1, 0) model
assgn3.model11 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1) + assgn3_coh, data=assgn3.ts)
sum.gen11 <- summary(assgn3.model11)
sum.gen11
kable(tidy(sum.gen11), digits=4, caption="ARDL10 model")
ehat11 <- resid(assgn3.model11)
corrgm11 <- acf(ehat11)
plot(corrgm11)

# Checking ARDL (1, 1) model
assgn3.model12 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1) + assgn3_coh + L(assgn3_coh,1), data=assgn3.ts)
sum.gen12 <- summary(assgn3.model12)
sum.gen12
kable(tidy(sum.gen12), digits=4, caption="ARDL11 model")
ehat12 <- resid(assgn3.model12)
corrgm12 <- acf(ehat12)
plot(corrgm12)

# Coefficients of variable coh and its lags are insignificant, hence we drop the variable.

# Checking ARDL (1, 0) model
assgn3.model10 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1), data=assgn3.ts)
sum.gen10 <- summary(assgn3.model10)
sum.gen10
kable(tidy(sum.gen10), digits=4, caption="ARDL10 model")
ehat10 <- resid(assgn3.model10)
corrgm10 <- acf(ehat10)
plot(corrgm10)

# Checking ARDL (2, 0) model
assgn3.model20 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1:2), data=assgn3.ts)
sum.gen20 <- summary(assgn3.model20)
sum.gen20
kable(tidy(sum.gen20), digits=4, caption="ARDL20 model")
ehat20 <- resid(assgn3.model20)
corrgm20 <- acf(ehat20)
plot(corrgm20)

# Checking ARDL (3, 0) model
assgn3.model30 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1:3), data=assgn3.ts)
sum.gen30 <- summary(assgn3.model30)
sum.gen30
kable(tidy(sum.gen30), digits=4, caption="ARDL30 model")
ehat30 <- resid(assgn3.model30)
corrgm30 <- acf(ehat30)
plot(corrgm30)

# Checking ARDL (4, 0) model
assgn3.model40 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1:4), data=assgn3.ts)
sum.gen40 <- summary(assgn3.model40)
sum.gen40
kable(tidy(sum.gen40), digits=4, caption="ARDL40 model")
ehat40 <- resid(assgn3.model40)
corrgm40 <- acf(ehat40)
plot(corrgm40)

# Checking ARDL (5, 0) model
assgn3.model50 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1:5), data=assgn3.ts)
sum.gen50 <- summary(assgn3.model50)
sum.gen50
kable(tidy(sum.gen50), digits=4, caption="ARDL50 model")
ehat50 <- resid(assgn3.model50)
corrgm50 <- acf(ehat50)
plot(corrgm50)

# Checking ARDL (6, 0) model
assgn3.model60 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1:6), data=assgn3.ts)
sum.gen60 <- summary(assgn3.model60)
sum.gen60
kable(tidy(sum.gen60), digits=4, caption="ARDL60 model")
ehat60 <- resid(assgn3.model60)
corrgm60 <- acf(ehat60)
plot(corrgm60)

# Autocorrelation is almost insignificant

# Checking ARDL (7, 0) model
assgn3.model70 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1:7), data=assgn3.ts)
sum.gen70 <- summary(assgn3.model70)
sum.gen70
kable(tidy(sum.gen70), digits=4, caption="ARDL70 model")
ehat70 <- resid(assgn3.model70)
corrgm70 <- acf(ehat70)
plot(corrgm70)

# Autocorrelation seems to increase again

# Checking ARDL (8, 0) model
assgn3.model80 <- dynlm(assgn3_rfr ~ L(assgn3_rfr,1:8), data=assgn3.ts)
sum.gen80 <- summary(assgn3.model80)
sum.gen80
kable(tidy(sum.gen80), digits=4, caption="ARDL80 model")
ehat80 <- resid(assgn3.model80)
corrgm80 <- acf(ehat80)
plot(corrgm80)

# Autocorrelation still is significant
# To fit parsimonious model, we conclude to fit ARDL (6, 0) model

# Confirming above result by checking AIC/BIC values
aics <- rep(0,10)
bics <- rep(0,10)
y <- assgn3_rfr
for (i in 1:10){
  ari <- dynlm(y~L(y,1:i), start=i)
  aics[i] <- AIC(ari)
  bics[i] <- BIC(ari)
}
tbl <- data.frame(rbind(aics, bics))
names(tbl) <- c("1","2","3","4","5","6","7","8","9","10")
row.names(tbl) <- c("AIC","BIC")
kable(tbl, digits=1, align='c', caption="Lag order selection for an AR model")
# Now, we assert that ARDL (6, 0) is the best parsimonious model.

# Fitting ARLD (6, 0) model
fitted.model <- assgn3.model60
fitted.sum.gen <- summary(fitted.model)
fitted.sum.gen
kable(tidy(fitted.sum.gen), digits=4, caption="Fitted model")

# Checking for autocorrelation using graphical method (plotting acf)
fitted.ehat <- resid(fitted.sum.gen)
fitted.corrgm <- acf(fitted.ehat)
plot(fitted.corrgm)

# Checking for autocorrelation using analytical method (LM test)

a <- bgtest(fitted.model, order=1, type="F", fill=0)
b <- bgtest(fitted.model, order=1, type="F", fill=NA)
c <- bgtest(fitted.model, order=6, type="Chisq", fill=0)
d <- bgtest(fitted.model, order=6, type="Chisq", fill=NA)

dfr <- data.frame(rbind(a[c(1,2,4)], b[c(1,2,4)], c[c(1,2,4)], d[c(1,2,4)]))
dfr <- cbind(c("1, F, 0", "1, F, NA", "6, Chisq, 0", "6, Chisq, NA"), dfr)

names(dfr)<-c("Method", "Statistic", "Parameters", "p-Value")
kable(dfr, caption="Breusch-Godfrey test for the fitted ARDL (6, 0) model")

dwtest(fitted.model)

# From the result, we understand we aren't able to completely remove autocorrelation due to very high frequency dataset employed.
# Below are a few plots which, along with AIC/BIC test, further justify why ARDL (6, 0) is best possible fit.
plot(ehat01)
plot(ehat10)
plot(ehat11)
plot(ehat12)
plot(ehat20)
plot(ehat30)
plot(ehat40)
plot(ehat50)
plot(ehat60)
plot(ehat70)
plot(ehat80)
# From ehat plots, we infer that ARDL (6,0), ARDL (7,0) and ARDL (8,0) have very similar distribution of errors.
plot(corrgm01)
plot(corrgm10)
plot(corrgm11)
plot(corrgm12)
plot(corrgm20)
plot(corrgm30)
plot(corrgm40)
plot(corrgm50)
plot(corrgm60)
plot(corrgm70)
plot(corrgm80)
# From correlogram plots, keeping law of parsimony under consideration, we can infer that ARDL (6,0) has least possible autocorrelation, which is in line with the AIC/BIC outcome.

# Checking for mean, variance, and normality of errors
plot(fitted.ehat)
# Mean comes out to be zero as seen from the plot, variance is fairly constant.
hist(fitted.ehat)
# From histogram, apart from a few outliers, we can clearly see that the distribution of errors is normal.

# Forecasting
fitted.forecasts <- forecast(fitted.model, h=321)
fitted.forecasts
plot(fitted.forecasts)

fitted.forecasts1 <- forecast(fitted.model, h=321, level=c(99.5))
fitted.forecasts1
plot(fitted.forecasts1)