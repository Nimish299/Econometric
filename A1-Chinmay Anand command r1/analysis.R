knitr::opts_chunk$set(echo = F, warning=F, message=F, cache = T)
library(dplyr)
library(olsrr)
library(ggplot2); theme_set(theme_classic())
library(GGally)
library(papeR)
library(stargazer)
library(lmtest)
library(MASS)
library(texreg)
library(broom)

df <- read.csv("./res/diabetes.csv")
df %>% 
  mutate(Outcome = factor(Outcome)) -> dff

df_nz <- df %>% 
  filter(Glucose > 0 & BloodPressure > 0 & SkinThickness > 0 & Insulin > 0 & BMI > 0) 

df_nz_factor <- df_nz %>% mutate(Outcome = factor(Outcome))


stargazer(df_nz_factor, 
          title="Summary statistics of the dataset",
          header=F, 
          table.placement = 'H',
          float = T)

ggpairs(df_nz_factor, 
        mapping=ggplot2::aes(color=Outcome),
        lower=list(continuous = wrap("smooth", alpha=.3, size=.1)),
        upper=list(
          combo = wrap("box_no_facet", alpha=.5, outlier.size=.1),
          continuous = wrap('cor', size=2)
        ),
        axisLabels = 'none'
) + theme_grey()

ggplot(df_nz, aes(x = factor(Outcome),fill=factor(Outcome))) +
  labs(x="Outcome",y="Number of Observations")+
  scale_x_discrete(labels = c("Non-Diabetic", "Diabetic"))+
  theme(legend.position="none") +
  geom_bar() 

df_nz$PregnancyRange <- cut(df_nz$Pregnancies, breaks = c(0, 4, 10, 17), include.lowest = T)

table(df_nz$PregnancyRange) %>% 
  knitr::kable()

ggplot(df_nz, aes(x = BloodPressure, fill = factor(Outcome))) +
  labs(fill = "Outcome")+
  scale_fill_discrete(labels = c("Non-Diabetic", "Diabetic"))+
  geom_histogram(position = "identity", alpha = 0.6, bins=20)

df_nz$BloodDummy <- cut(df_nz$BloodPressure, breaks = c(0, 60, 80, 89, 110), labels=c("Hypo","Normal", "Pre", "Hyper"))

df_nz$BloodDummy <- relevel(df_nz$BloodDummy, ref="Normal")

table(df_nz$BloodDummy) %>% 
  knitr::kable()

ggplot(df_nz, aes(factor(Outcome), SkinThickness, color = factor(Outcome)))+
  geom_violin(data=subset(df_nz,SkinThickness>0)) +
  geom_boxplot(data=subset(df_nz,SkinThickness>0),width = .2)+
  labs(x="Outcome",y="Skin Thickness")+
  scale_x_discrete(labels = c("Non-Diabetic", "Diabetic"))+
  theme(legend.position="none")

ggplot(df_nz, aes(x = BMI, fill = factor(Outcome))) +
  labs(fill = "Outcome") +
  scale_fill_discrete(labels = c("Non-Diabetic", "Diabetic" )) +
  geom_histogram(data=subset(df_nz, BMI>0),
                 position = "identity", alpha = 0.6) 

ggplot(df_nz, aes(x = factor(Outcome),
                     y = DiabetesPedigreeFunction,fill=factor(Outcome))) +
  labs(x="Outcome",y="Diabetes Pedigree Function")+
  scale_x_discrete(labels = c("Non-Diabetic", "Diabetic"))+
  geom_boxplot(alpha=0.7)+
  geom_jitter(alpha=0.23)+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

ggplot(df_nz, aes(x = Age, fill = factor(Outcome))) +
  labs(fill = "Outcome")+
  scale_fill_discrete(labels = c("Non-Diabetic", "Diabetic" ))+
  geom_histogram(position = "identity", alpha = 0.6) + 
  theme(legend.position = c(.77, .87), 
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16))

ggplot(df_nz, aes(x = factor(Outcome), y = Age,fill=factor(Outcome))) +
  labs(x="Outcome",y="Age")+
  scale_x_discrete(labels = c("Non-Diabetic", "Diabetic"))+
  geom_boxplot(alpha=0.6) +
  theme(legend.position="none",
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=16)) +
  scale_fill_brewer(palette="Dark2")

# Regression
fit <- lm(Outcome ~ PregnancyRange + 
            SkinThickness +
            Glucose +
            BloodDummy +
            SkinThickness +
            Insulin +
            log(DiabetesPedigreeFunction) +
            Age +
            BMI, 
           df_nz)

# Robust Regression
robust <- rlm(Outcome ~ PregnancyRange + 
            SkinThickness +
            Glucose +
            BloodDummy +
            SkinThickness +
            Insulin +
            log(DiabetesPedigreeFunction) +
            Age +
            BMI, 
           df_nz)
stargazer(robust, title="Robust Regression Results", header=F, table.placement="H")

# Calculate R-squared and adjusted R-squared
SST <- sum((df_nz$Outcome - mean(df_nz$Outcome))^2)
SSE <- sum(robust$residuals^2)
R2 <- 1 - SSE/SST
n <- nrow(df_nz)
p <- length(coefficients(robust)) - 1
adjR2 <- 1 - SSE/(n - p - 1)/(SST/(n - 1))

# Calculate the F-statistic
F_stat <- (t(robust$coefficients) %*% solve(vcov(robust)) %*% robust$coefficients)/(p * summary(robust)$sigma^2)

pval_f <- pf(F_stat, p, n - p - 1, lower.tail = F)

matrix(c(R2, adjR2, F_stat, pval_f), nrow=4, ncol=1) %>% 
  as.data.frame() -> results

rownames(results) <- c("R-square", "Adj. R-square", "F-statistic", "p-value for F-test")
colnames(results) <- "Value"

knitr::kable(round(results, 4), caption = "Regression Statistics")



transpose <- function(df) {
  df %>% 
    as.matrix() %>% 
    t() -> df1
  
  rownames(df1) <- colnames(df)
  df1
}

robust %>% 
  bptest() %>% 
  tidy() %>% 
  transpose() %>% 
  knitr::kable(caption="BP Test Results")

robust %>% 
  resettest(power=2, type="fitted") %>% 
  tidy() %>% 
  transpose() %>% 
  knitr::kable(caption="RESET Test Results")

ols_vif_tol(fit) %>% 
  mutate(Tolerance = round(Tolerance, 3),
         VIF = round(VIF, 3))