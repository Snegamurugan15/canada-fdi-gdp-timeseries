# install.packages("moments")
# install.packages("Kendall")
library(readxl)
library(moments)
library(tseries)
library(Kendall)
library(urca)

data= read_excel(file.choose())

summary = as.data.frame(summary(data))
summary

sapply(data, sd, na.rm = TRUE)

skewness(data)
kurtosis(data)

#goodness of fit test
jarque.test(data$FDI_TOTAL)
jarque.test(data$FDI_AGRI)
jarque.test(data$FDI_MINING)
jarque.test(data$FDI_MANUFACTURING)
jarque.test(data$FDI_WHOLESALE_TRADE)
jarque.test(data$FDI_RETAIL_TRADE)  
jarque.test(data$FDI_FINANCE)
jarque.test(data$FDI_REAL_ESTATE)
jarque.test(data$FDI_TECHNICAL_SERVICES)
jarque.test(data$FDI_MANAGEMENT_COMPANY)
jarque.test(data$GDP_TOTAL)
jarque.test(data$GDP_AGRI)
jarque.test(data$GDP_MINING)
jarque.test(data$GDP_MANUFACTURING)
jarque.test(data$GDP_WHOLESALE_TRADE)
jarque.test(data$GDP_RETAIL_TRADE)  
jarque.test(data$GDP_FINANCE)
jarque.test(data$GDP_REAL_ESTATE)
jarque.test(data$GDP_TECHNICAL_SERVICES)
jarque.test(data$GDP_MANAGEMENT_COMPANY)

###since all the p-value > 0.05, we would accept null hypo(i.e) data is normally distributed
###Here we have FDI_AGRI, GDP_TOTAL, GDP_WHOLESALE_TRADE, GDP_FINANCE p-value>0.05, which means data is normally distributed. 


# johansen_cointgration_test
jotest_1 = ca.jo(data.frame(diff(data$FDI_TOTAL), diff(data$GDP_TOTAL)), K=6, spec = "transitory", season=5, dumvar = NULL)
summary(jotest_1)
# MAKING STATIONARY
Total = 0.2817768*diff(data$FDI_TOTAL) -0.1384584*diff(data$GDP_TOTAL)
plot(Total, type = "l", main = "Overall Cointegration in all industries")
adf.test(Total)
#1 COINTEGRATING FACTOR

jotest_2 = ca.jo(data.frame(data$FDI_AGRI, data$GDP_AGRI), K=4, spec = "transitory", season=5, dumvar = NULL)
summary(jotest_2)
# MAKING STATIONARY
Agri =   0.11819174*(data$FDI_AGRI) -0.07851161*data$GDP_AGRI
plot(Agri, type = "l", main = "Cointegration in Agriculture industry")
adf.test(Agri)
#1COINTEGRATING FACTOR

jotest_3 = ca.jo(data.frame(diff(data$FDI_MANUFACTURING), diff(data$GDP_MANUFACTURING)), K=4, spec = "transitory", season=5, dumvar = NULL)
summary(jotest_3)
# MAKING STATIONARY
Manufacturing =   0.30841722*(data$FDI_MANUFACTURING) -0.08962771*data$GDP_MANUFACTURING
plot(Manufacturing, type = "l", main = "Cointegration in Manufacturing industry")
adf.test(Manufacturing)
# 0 COINTEGRATING FACTOR

jotest_4 = ca.jo(data.frame(diff(data$FDI_MINING), diff(data$GDP_MINING)), K=4, spec = "transitory", season=5, dumvar = NULL)
summary(jotest_4)
# MAKING STATIONARY
Mining =   0.3816846*(data$FDI_MINING) -0.1199206*data$GDP_MINING
plot(Mining, type = "l", main = "Cointegration in Mining industry")
adf.test(Mining)
#1-cointegrating factor

jotest_5= ca.jo(data.frame(diff(data$FDI_WHOLESALE_TRADE),diff(data$GDP_WHOLESALE_TRADE)), type = "trace", K=4, spec ="longrun")
summary(jotest_5)
Wholesale_Trade = 0.38056445*diff(data$FDI_WHOLESALE_TRADE) - 0.08888729*diff(data$GDP_WHOLESALE_TRADE)
plot(Wholesale_Trade, type = "l", main = "Cointegration in Wholesale Trading industry")
adf.test(Wholesale_Trade)
#1-cointegrating factor

jotest_6= ca.jo(data.frame(diff(data$FDI_MANAGEMENT_COMPANY),diff(data$GDP_MANAGEMENT_COMPANY)), type = "trace", K=4, spec ="longrun")
summary(jotest_6)
Management_Company = 0.17194148*diff(data$FDI_MANAGEMENT_COMPANY) - 0.07465558*diff(data$GDP_MANAGEMENT_COMPANY)
plot(Management_Company, type = "l", main = "Cointegration in Managing Companies")
adf.test(Management_Company)
#1-cointegrating factor

jotest_7= ca.jo(data.frame(diff(data$FDI_RETAIL_TRADE),diff(data$GDP_RETAIL_TRADE)), type = "trace", K=4, spec ="longrun")
summary(jotest_7)
Retail_Trade = 0.3672932*diff(data$FDI_RETAIL_TRADE) - 0.1581030*diff(data$GDP_RETAIL_TRADE)
plot(Retail_Trade, type = "l", main = "Cointegration in Retail Trading industry")
adf.test(Retail_Trade)
#2-cointegrating factor

jotest_8= ca.jo(data.frame(diff(data$FDI_FINANCE),diff(data$GDP_FINANCE)), type = "trace", K=4, spec ="longrun")
summary(jotest_8)
Finance = 0.2716062*diff(data$FDI_FINANCE) - 0.2040492*diff(data$GDP_FINANCE)
plot(Finance, type = "l", main = "Cointegration in Finance industry")
adf.test(Finance)
#2-cointegrating factor

  jotest_9= ca.jo(data.frame(diff(data$FDI_REAL_ESTATE),diff(data$GDP_REAL_ESTATE)), type = "trace", K=4, spec ="longrun")
summary(jotest_9)
Realestate = 0.4081529445*diff(data$FDI_REAL_ESTATE) - 0.1261542*diff(data$GDP_REAL_ESTATE)
plot(Realestate, type = "l", main = "Cointegration in Real_estate industry")
adf.test(Realestate)
#1-cointegrating factor

jotest_10= ca.jo(data.frame(diff(data$FDI_TECHNICAL_SERVICES),diff(data$GDP_TECHNICAL_SERVICES)), type = "trace", K=4, spec ="longrun")
summary(jotest_10)
Technical_service = 0.27156822*diff(data$FDI_TECHNICAL_SERVICES) - 0.07286953*diff(data$GDP_TECHNICAL_SERVICES)
plot(Technical_service, type = "l", main = "Cointegration in Technical Service industry")
adf.test(Technical_service)
#0-cointegrating factor



#VAR Lag Order Selection Criteria under AIC
# install.packages("tsDyn")
# install.packages("bvartools")
library(tsDyn)
library(bvartools)
lags.select(data.frame(data$FDI_TOTAL,data$GDP_TOTAL),lag.max = 8, include = "trend", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_AGRI,data$GDP_AGRI),lag.max = 8, include = "trend", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_MINING,data$GDP_MINING),lag.max = 5, include = "const", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_MANUFACTURING,data$GDP_MANUFACTURING),lag.max = 8, include = "const", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_WHOLESALE_TRADE,data$GDP_WHOLESALE_TRADE),lag.max = 8, include = "trend", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_RETAIL_TRADE,data$GDP_RETAIL_TRADE),lag.max = 8, include = "trend", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_FINANCE,data$GDP_FINANCE),lag.max = 8, include = "trend", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_REAL_ESTATE,data$GDP_REAL_ESTATE),lag.max = 8, include = "trend", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_TECHNICAL_SERVICES,data$GDP_TECHNICAL_SERVICES),lag.max = 8, include = "trend", fitMeasure = "SSR", sameSample = TRUE)
lags.select(data.frame(data$FDI_MANAGEMENT_COMPANY,data$GDP_MANAGEMENT_COMPANY),lag.max = 8, include = "trend", fitMeasure = "SSR", sameSample = TRUE)


library(vars)
# VARselect(data.frame(data$FDI_TOTAL,data$GDP_TOTAL), lag.max = 5, type = "const")
# VARselect(data.frame(data$GVA_MANU,data$FDI_MANU), lag.max = 5, type = "const")
# VARselect(data.frame(data$GVA_ELEC,data$FDI_ELEC), lag.max = 5, type = "const")
# VARselect(data.frame(data$GVA_CON,data$FDI_CONS), lag.max = 5, type = "const")
# VARselect(data.frame(data$GVA_MIN,data$FDI_MIN), lag.max = 5, type = "const")

#VECM
vecm_1 = VECM(data.frame(data$FDI_TOTAL,data$GDP_TOTAL), lag = 5, r = 1, estim = "ML" , LRinclude = "none", exogen = NULL)
summary(vecm_1)
predict(vecm_1, n.ahead = 5)
VECM_2 = VECM(data.frame(data$FDI_AGRI,data$GDP_AGRI), lag = 3, r = 1, estim = "ML" , LRinclude = "none", exogen = NULL)
summary(VECM_2)
predict(VECM_2, n.ahead = 5)
VECM_3 = VECM(data.frame(data$FDI_MINING,data$GDP_MINING), lag = 3, r = 1, estim = "ML" , LRinclude = "none", exogen = NULL)
summary(VECM_3)
predict(VECM_3, n.ahead = 5)
VECM_4 = VECM(data.frame(data$FDI_WHOLESALE_TRADE,data$GDP_WHOLESALE_TRADE), lag = 3, r = 1, estim = "ML" , LRinclude = "none", exogen = NULL)
summary(VECM_4)
predict(VECM_4, n.ahead = 5)
VECM_5 = VECM(data.frame(data$FDI_REAL_ESTATE,data$GDP_REAL_ESTATE), lag = 3, r = 1, estim = "ML" , LRinclude = "none", exogen = NULL)
summary(VECM_5)
predict(VECM_5, n.ahead = 5)
VECM_6 = VECM(data.frame(data$FDI_MANAGEMENT_COMPANY,data$GDP_MANAGEMENT_COMPANY), lag = 3, r = 1, estim = "ML" , LRinclude = "none", exogen = NULL)
summary(VECM_6)
predict(VECM_6, n.ahead = 5)

library(vars)
var.model_Manufacturing = VAR(data.frame(data$FDI_MANUFACTURING, data$GDP_MANUFACTURING), p = 4, type = "const", season = 4)
var.model_Manufacturing
pred_var_Manufacturing = predict(var.model_Manufacturing, n.ahead = 5)
pred_var_Manufacturing

var_Technical = VAR(data.frame(data$FDI_TECHNICAL_SERVICES, data$GDP_TECHNICAL_SERVICES), p = 4, type = "const", season = 4)
var_Technical
predict(var_Technical, n.ahead = 5)

var_Finance = VAR(data.frame(data$FDI_FINANCE, data$GDP_FINANCE), p = 4, type = "const", season = 4)
var_Finance
predict(var_Finance, n.ahead = 5)

var_Retail_Trade = VAR(data.frame(data$FDI_RETAIL_TRADE, data$GDP_RETAIL_TRADE), p = 4, type = "const", season = 4)
var_Retail_Trade
predict(var_Retail_Trade, n.ahead = 5)


#DIAGNOSTIC TEST RESULTS
#install.packages("nortsTest")
library(nortsTest)
Lm.test(as.matrix(data$GVA_MANU,data$FDI_MANU), lag.max = 5, alpha = 0.05)
Lm.test(as.matrix(data$GVA_SER,data$FDI_SER), lag.max = 5, alpha = 0.05)
Lm.test(as.matrix(data$GVA_ELEC,data$FDI_ELEC), lag.max = 5, alpha = 0.05)
Lm.test(as.matrix(data$GVA_CON, data$FDI_CONS), lag.max = 5, alpha = 0.05)


#JARQUE BERA TEST FOR NORMALITY
jarque.bera.test(as.matrix(data$GVA_MANU, data$FDI_MANU))
jarque.bera.test(as.matrix(data$GVA_SER,data$FDI_SER))
jarque.bera.test(as.matrix(data$GVA_ELEC,data$FDI_ELEC))
jarque.bera.test(as.matrix(data$GVA_CON, data$FDI_CONS))



#granger casuality test
library(lmtest)
grangertest(data$FDI_TOTAL,data$GDP_TOTAL, order = 3)#casuality
grangertest(data$GDP_TOTAL, data$FDI_TOTAL, order = 4)#no casuality

grangertest(data$FDI_AGRI, data$GDP_AGRI, order = 1)#NOcasuality
grangertest(data$GDP_AGRI, data$FDI_AGRI, order = 1)#NOcasuality

grangertest(data$FDI_MINING, data$GDP_MINING, order = 3)#casuality
grangertest(data$GDP_MINING, data$FDI_MINING, order = 1)#NOcasuality

grangertest(data$FDI_MANUFACTURING, data$GDP_MANUFACTURING, order = 1)#NOcasuality
grangertest(data$GDP_MANUFACTURING, data$FDI_MANUFACTURING, order = 1)#casuality

grangertest(data$FDI_WHOLESALE_TRADE, data$GDP_WHOLESALE_TRADE, order = 2)#casuality
grangertest(data$GDP_WHOLESALE_TRADE, data$FDI_WHOLESALE_TRADE, order = 2)#casuality

grangertest(data$FDI_RETAIL_TRADE, data$GDP_RETAIL_TRADE, order = 2)#casuality
grangertest(data$GDP_RETAIL_TRADE, data$FDI_RETAIL_TRADE, order = 2)#casuality

grangertest(data$FDI_FINANCE, data$GDP_FINANCE, order = 4)#casuality
grangertest(data$GDP_FINANCE, data$FDI_FINANCE, order = 5)# NO casuality

grangertest(data$FDI_REAL_ESTATE, data$GDP_REAL_ESTATE, order = 4)#casuality
grangertest(data$GDP_REAL_ESTATE, data$FDI_REAL_ESTATE, order = 5)#NO casuality

grangertest(data$FDI_TECHNICAL_SERVICES, data$GDP_TECHNICAL_SERVICES, order = 4)#casuality
grangertest(data$GDP_TECHNICAL_SERVICES, data$FDI_TECHNICAL_SERVICES, order = 1)#NO casuality

grangertest(data$FDI_MANAGEMENT_COMPANY, data$GDP_MANAGEMENT_COMPANY, order = 3)#NO casuality
grangertest(data$GDP_MANAGEMENT_COMPANY, data$FDI_MANAGEMENT_COMPANY, order = 3)#casuality
#if p value < 0.05, casuality exists







