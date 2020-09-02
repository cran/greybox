## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align="center",
  fig.height=4,
  fig.width=6
)

library(greybox)

## ----normalDistributionData---------------------------------------------------
xreg <- cbind(rnorm(100,10,3),rnorm(100,50,5))
xreg <- cbind(500+0.5*xreg[,1]-0.75*xreg[,2]+rs(100,0,3),xreg,rnorm(100,300,10))
colnames(xreg) <- c("y","x1","x2","Noise")

inSample <- xreg[1:80,]
outSample <- xreg[-c(1:80),]

## ----normalRegression---------------------------------------------------------
ourModel <- alm(y~x1+x2, data=inSample, distribution="dnorm")
summary(ourModel)
plot(predict(ourModel,outSample,interval="p",level=c(0.9,0.95)))

## ----ALaplaceRegression-------------------------------------------------------
ourModel <- alm(y~x1+x2, data=inSample, distribution="dalaplace",alpha=0.95)
summary(ourModel)
plot(predict(ourModel,outSample))

## ----dataRound----------------------------------------------------------------
xreg[,1] <- round(abs(xreg[,1]))
inSample <- xreg[1:80,]
outSample <- xreg[-c(1:80),]

## ----negBinRegression---------------------------------------------------------
ourModel <- alm(y~x1+x2, data=inSample, distribution="dnbinom")
summary(ourModel)

## ----negBinRegressionWithSize-------------------------------------------------
ourModel <- alm(y~x1+x2, data=inSample, distribution="dnbinom", size=30)
summary(ourModel)

## ----mixtureExampleData-------------------------------------------------------
xreg[,1] <- round(exp(xreg[,1]-400) / (1 + exp(xreg[,1]-400)),0) * xreg[,1]
# Sometimes the generated data contains huge values
xreg[is.nan(xreg[,1]),1] <- 0;
inSample <- xreg[1:80,]
outSample <- xreg[-c(1:80),]

## ----mixtureExampleOccurrence-------------------------------------------------
modelOccurrence <- alm(y~x1+x2+Noise, inSample, distribution="plogis")

## ----mixtureExampleFinal------------------------------------------------------
modelMixture <- alm(y~x1+x2+Noise, inSample, distribution="dlnorm", occurrence=modelOccurrence)

## ----mixtureSummary-----------------------------------------------------------
summary(modelMixture)
summary(modelMixture$occurrence)

## ----mixtureDiagnostics-------------------------------------------------------
par(mfcol=c(3,3))
plot(modelMixture, c(1:9))

## ----mixturePredict-----------------------------------------------------------
predict(modelMixture,outSample,interval="p",level=c(0.8,0.9,0.95))

## ----mixtureExampleFinalAR----------------------------------------------------
modelMixtureAR <- alm(y~x1+x2+Noise, inSample, distribution="dlnorm", occurrence=modelOccurrence, ar=1)
summary(modelMixtureAR)
plot(predict(modelMixtureAR,outSample,interval="p",side="u"))

## ----mixtureExampleFinalARForecast--------------------------------------------
plot(forecast(modelMixtureAR, h=10, interval="p",side="u"))

## -----------------------------------------------------------------------------
lossFunction <- function(actual, fitted, B, xreg){
  return(mean(abs(actual-fitted)^3));
}
modelLossCustom <- alm(y~x1+x2+Noise, inSample, distribution="dnorm", loss=lossFunction)
summary(modelLossCustom)

## -----------------------------------------------------------------------------
summary(modelLossCustom, bootstrap=TRUE, nsim=100)

