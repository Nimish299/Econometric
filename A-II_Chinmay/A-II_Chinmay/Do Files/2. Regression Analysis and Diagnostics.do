** Use the file "B. Final Balanced Panel Dataset.dta"

xtset iau_id_num year, yearly delta (5 year)

** Hausman Test: RE Estimation v/s FE Estimation (Ho: Unique errors (ui) are not correlated with the regressors. Hence, Fail to Reject --> RE > FE)

xtreg students5_estimated i.regionnum i.incomegroupnum foundedyr private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized i.year, re
estimates store random

xtreg students5_estimated i.regionnum i.incomegroupnum private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized i.year, fe
estimates store fixed

* Diagnostics

hausman fixed random

** Checking for significance of Year Fixed Effects (Ho: Coefficients for all years are jointly equal to zero. Hence, no time fixed effects are present)

xtreg students5_estimated i.regionnum i.incomegroupnum private01 foundedyr phd_granting m_granting b_granting divisions total_fields unique_fields specialized i.year, fe
testparm i.year

** BPLM Test: RE Estimation v/s Pooled OLS Estimation (Ho: Variances across entities are zero. Hence, there's no significant difference across units (i.e. no panel effect). Fail to Reject --> Simple Pooled OLS > RE)

xtreg students5_estimated i.regionnum i.incomegroupnum private01 foundedyr phd_granting m_granting b_granting divisions total_fields unique_fields specialized i.year, re
xttest0

** Normality Test: (Shapiro-Wilk Test) (Ho: Errors are normally distributed)

xtreg students5_estimated i.regionnum i.incomegroupnum private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized i.year, fe
predict e, resid
swilk e

** Heteroskedasticity Test: (Modified Wald test) (Ho: Homoskedastic Model)

xtreg students5_estimated i.regionnum i.incomegroupnum foundedyr private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized i.year, fe
xttest3

** Multicollinearity Test: (If vif < 10, no multicollinearity is present)

reg students5_estimated i.regionnum i.incomegroupnum private01 foundedyr phd_granting m_granting b_granting divisions total_fields unique_fields specialized i.year
vif

** Altenative way to test Multicollinearity: (If vif < 10, no multicollinearity is present) (No need to input dep var and dummy (factor) variables while using collin)

collin private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized

** Omitted Variable Bias Test: (RAMSAY RESET) (Ho: No significant variable has been omitted)

reg students5_estimated i.regionnum i.incomegroupnum private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized i.year
estat ovtest

** Cross-Sectional Dependence (Contemporaneous Correlation) test:

** According to Baltagi, cross-sectional dependence is a problem in macro panels with long time series (over 20-30 years). This is not much of a problem in micro panels (few years and large
** number of cases). Hence, we can safely assume that in our model, there's no contemporaneous correlation.

** Serial Correlation (Autocorrelation)

** Again, serial correlation tests apply to macro panels with long time series (over 20-30 years). Not a problem in micro panels (with very few years). Hence, we can safely assume that in our model, there's no serial correlation.
