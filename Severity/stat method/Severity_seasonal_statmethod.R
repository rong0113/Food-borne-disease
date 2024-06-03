set.seed(123)
salm_spr = read.csv('/Users/evelyn/Desktop/Seasonality/salm_spring.csv')
salm_sum = read.csv('/Users/evelyn/Desktop/Seasonality/salm_summer.csv')
salm_aut = read.csv('/Users/evelyn/Desktop/Seasonality/salm_autumn.csv')
salm_win = read.csv('/Users/evelyn/Desktop/Seasonality/salm_winter.csv')
#################################################GAM####################################################################

library(mgcv)
#spring
salm_gam = gam(TotalCases ~ s(TEMP)+s(SLP)+s(WDSP)+s(PRCP)+s(RH)+s(SLP.STP)+s(MXSPD.WDSP)+s(Median.age)+s(Immigrants)+s(Households.Median.Income)+s(X.Food.preparation)+s(X.Farming..fishing..and.forestry)+s(Population.Density),
               data = salm_spr, method="REML", family = Gamma)
summary(salm_gam)

y <- salm_spr$TotalCases
x <- data.matrix(salm_spr[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                          'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

data <- salm_spr[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                 'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density',
                 'TotalCases')]
x <- model.matrix(TotalCases ~ ., data)[,-1]
y <- data$TotalCases
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 13, verbose = FALSE)

result <- rfe(x, y, sizes = c(1:13), rfeControl = ctrl)
print(result)
#Immigrants, Population.Density, X.Farming..fishing..and.forestry, Median.age, PRCP

salm_gam = gam(TotalCases ~ s(TEMP)+s(SLP)+s(WDSP)+s(PRCP)+s(RH)+s(SLP.STP)+s(MXSPD.WDSP)+s(Median.age)+s(Immigrants)+s(Households.Median.Income)+s(X.Food.preparation)+s(X.Farming..fishing..and.forestry)+s(Population.Density),
               data = salm_sum, method="REML", family = Gamma)
summary(salm_gam)

y <- salm_sum$TotalCases
x <- data.matrix(salm_sum[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

data <- salm_sum[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                 'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density',
                 'TotalCases')]
x <- model.matrix(TotalCases ~ ., data)[,-1]
y <- data$TotalCases
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 13, verbose = FALSE)

result <- rfe(x, y, sizes = c(1:13), rfeControl = ctrl)
print(result)
#TEMP, SLP.STP, Population.Density, RH, Immigrants

#autumn
salm_gam = gam(TotalCases ~ s(TEMP)+s(SLP)+s(WDSP)+s(PRCP)+s(RH)+s(SLP.STP)+s(MXSPD.WDSP)+s(Median.age)+s(Immigrants)+s(Households.Median.Income)+s(X.Food.preparation)+s(X.Farming..fishing..and.forestry)+s(Population.Density),
               data = salm_aut, method="REML", family = Gamma)
summary(salm_gam)
draw(salm_gam, parametic = TRUE)

y <- salm_aut$TotalCases
x <- data.matrix(salm_aut[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

data <- salm_aut[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                 'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density',
                 'TotalCases')]
x <- model.matrix(TotalCases ~ ., data)[,-1]
y <- data$TotalCases
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 13, verbose = FALSE)

result <- rfe(x, y, sizes = c(1:13), rfeControl = ctrl)
print(result)
#MXSPD.WDSP, Population.Density, SLP, Median.age, SLP.STP
#winter
salm_gam = gam(TotalCases ~ s(TEMP)+s(SLP)+s(WDSP)+s(PRCP)+s(RH)+s(SLP.STP)+s(MXSPD.WDSP)+s(Median.age)+s(Immigrants)+s(Households.Median.Income)+s(X.Food.preparation)+s(X.Farming..fishing..and.forestry)+s(Population.Density),
               data = salm_win, method="REML", family = Gamma)
summary(salm_gam)


y <- salm_win$TotalCases
x <- data.matrix(salm_win[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

data <- salm_win[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                 'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density',
                 'TotalCases')]
x <- model.matrix(TotalCases ~ ., data)[,-1]
y <- data$TotalCases
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 13, verbose = FALSE)

result <- rfe(x, y, sizes = c(1:13), rfeControl = ctrl)
print(result)
#X.Farming..fishing..and.forestry, SLP, MXSPD.WDSP, TEMP, X.Food.preparation

################################################ LASSO ###############################################################

### Spring######################################
y <- salm_spr$TotalCases
x <- data.matrix(salm_spr[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #3.724937
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) #No coefficient is shown for the predictor drat because the lasso regression shrunk the coefficient all the way to zero. This means it was completely dropped from the model because it wasn’t influential enough.

### Summer######################################


y <- salm_sum$TotalCases
x <- data.matrix(salm_sum[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

yfit=glmnet(x,y,family="gaussian",standardize.response=FALSE,intercept = TRUE,lambda=(exp(seq(log(0.0001), log(100), length.out=100))),upper.limits = 1000,lower.limits = -1000)
ycvfit=cv.glmnet(x,y,family="gaussian",nfolds=3,standardize.response=FALSE,intercept = TRUE,grouped = FALSE,lambda=(exp(seq(log(0.0001), log(100), length.out=100))),upper.limits = 1000,lower.limits = -1000)


#writing results 
lassocoeff=as.matrix(coef(yfit,s=0.1))
print(lassocoeff)

###################################RFE Norovirus ########################################
set.seed(123)
noro_spr = read.csv('/Users/evelyn/Desktop/Seasonality/noro_spring.csv')
noro_sum = read.csv('/Users/evelyn/Desktop/Seasonality/noro_summer.csv')
noro_aut = read.csv('/Users/evelyn/Desktop/Seasonality/noro_autumn.csv')
noro_win = read.csv('/Users/evelyn/Desktop/Seasonality/noro_winter.csv')
#################################################GAM####################################################################

library(mgcv)
#spring
noro_gam = gam(TotalCases ~ s(TEMP)+s(SLP)+s(WDSP)+s(PRCP)+s(RH)+s(SLP.STP)+s(MXSPD.WDSP)+s(Median.age)+s(Immigrants)+s(Households.Median.Income)+s(X.Food.preparation)+s(X.Farming..fishing..and.forestry)+s(Population.Density),
               data = noro_spr, method="REML", family = Gamma)
summary(noro_gam)

y <- noro_spr$TotalCases
x <- data.matrix(noro_spr[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

data <- noro_spr[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                 'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density',
                 'TotalCases')]
x <- model.matrix(TotalCases ~ ., data)[,-1]
y <- data$TotalCases
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 13, verbose = FALSE)

result <- rfe(x, y, sizes = c(1:13), rfeControl = ctrl)
print(result)
#TEMP, SLP.STP, PRCP, Immigrants, Population.Density

noro_gam = gam(TotalCases ~ s(TEMP)+s(SLP)+s(WDSP)+s(PRCP)+s(RH)+s(SLP.STP)+s(MXSPD.WDSP)+s(Median.age)+s(Immigrants)+s(Households.Median.Income)+s(X.Food.preparation)+s(X.Farming..fishing..and.forestry)+s(Population.Density),
               data = noro_sum, method="REML", family = Gamma)
summary(noro_gam)

y <- noro_sum$TotalCases
x <- data.matrix(noro_sum[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

data <- noro_sum[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                 'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density',
                 'TotalCases')]
x <- model.matrix(TotalCases ~ ., data)[,-1]
y <- data$TotalCases
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 13, verbose = FALSE)

result <- rfe(x, y, sizes = c(1:13), rfeControl = ctrl)
print(result)
#Population.Density, PRCP, X.Farming..fishing..and.forestry, Median.age, Immigrants

noro_gam = gam(TotalCases ~ s(TEMP)+s(SLP)+s(WDSP)+s(PRCP)+s(RH)+s(SLP.STP)+s(MXSPD.WDSP)+s(Median.age)+s(Immigrants)+s(Households.Median.Income)+s(X.Food.preparation)+s(X.Farming..fishing..and.forestry)+s(Population.Density),
               data = noro_aut, method="REML", family = Gamma)
summary(noro_gam)
draw(noro_gam, parametic = TRUE)

y <- noro_aut$TotalCases
x <- data.matrix(noro_aut[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

data <- noro_aut[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                 'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density',
                 'TotalCases')]
x <- model.matrix(TotalCases ~ ., data)[,-1]
y <- data$TotalCases
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 13, verbose = FALSE)

result <- rfe(x, y, sizes = c(1:13), rfeControl = ctrl)
print(result)
#X.Farming..fishing..and.forestry, Households.Median.Income, MXSPD.WDSP, Median.age, X.Food.preparation
#winter
noro_gam = gam(TotalCases ~ s(TEMP)+s(SLP)+s(WDSP)+s(PRCP)+s(RH)+s(SLP.STP)+s(MXSPD.WDSP)+s(Median.age)+s(Immigrants)+s(Households.Median.Income)+s(X.Food.preparation)+s(X.Farming..fishing..and.forestry)+s(Population.Density),
               data = noro_win, method="REML", family = Gamma)
summary(noro_gam)


y <- noro_win$TotalCases
x <- data.matrix(noro_win[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

data <- noro_win[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                 'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density',
                 'TotalCases')]
x <- model.matrix(TotalCases ~ ., data)[,-1]
y <- data$TotalCases
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 13, verbose = FALSE)

result <- rfe(x, y, sizes = c(1:13), rfeControl = ctrl)
print(result)

################################################ LASSO ###############################################################

### Spring######################################
y <- salm_spr$TotalCases
x <- data.matrix(salm_spr[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #3.724937
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) #No coefficient is shown for the predictor drat because the lasso regression shrunk the coefficient all the way to zero. This means it was completely dropped from the model because it wasn’t influential enough.

### Summer######################################


y <- salm_sum$TotalCases
x <- data.matrix(salm_sum[, c('TEMP','SLP','WDSP','PRCP','RH','SLP.STP',"MXSPD.WDSP",
                              'Median.age', 'Immigrants','Households.Median.Income','X.Food.preparation', 'X.Farming..fishing..and.forestry', 'Population.Density')])

yfit=glmnet(x,y,family="gaussian",standardize.response=FALSE,intercept = TRUE,lambda=(exp(seq(log(0.0001), log(100), length.out=100))),upper.limits = 1000,lower.limits = -1000)
ycvfit=cv.glmnet(x,y,family="gaussian",nfolds=3,standardize.response=FALSE,intercept = TRUE,grouped = FALSE,lambda=(exp(seq(log(0.0001), log(100), length.out=100))),upper.limits = 1000,lower.limits = -1000)


#writing results 
lassocoeff=as.matrix(coef(yfit,s=0.1))
print(lassocoeff)

#SALM_spr: Immigrants, Population.Density, %Farming, Median.age, PRCP
#SALM_sum: TEMP, SLP.STP, Population.Density, RH, Immigrants
#SALM_aut: MXSPD.WDSP, Population.Density, SLP, Median.age, SLP.STP
#SALM_win: %Farming, SLP, MXSPD.WDSP, TEMP, %Food

#NORO_spr: TEMP, SLP.STP, PRCP, Immigrants, Population.Density
#NORO_sum: Population.Density, PRCP, %Farming, Median.age, Immigrants
#NORO_aut: %Farming, Households.Median.Income, MXSPD.WDSP, Median.age, %Food
#NORO_win: Immigrants, Population.Density, %Farming, Median.age, MXSPD.WDSP


# SALM: %Farming, MXSPD.WDSP, Households.Median.Income, Median.age, %Food
# NORO: Population.Density, %Farming, Median.age, PRCP, Immigrants


