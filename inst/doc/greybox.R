## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, message=FALSE------------------------------------------
library(greybox)

## ----BJxreg1-------------------------------------------------------------
BJxreg <- xregExpander(BJsales.lead,lags=c(-5,-10))

## ----BJxreg2-------------------------------------------------------------
BJxreg <- xregExpander(BJsales.lead,lags=c(7,-5,-10))

## ----BJxreg3-------------------------------------------------------------
BJxreg <- xregExpander(BJsales.lead,lags=c(-10:10))

## ----BJData--------------------------------------------------------------
BJxreg <- as.data.frame(xregExpander(BJsales.lead,lags=c(-10:10)))
BJxreg <- cbind(as.matrix(BJsales),BJxreg)
colnames(BJxreg)[1] <- "y"
ourModel <- stepwise(BJxreg)

## ----BJStepwise----------------------------------------------------------
ourModel <- stepwise(BJxreg)

## ----BJStepwiseResult----------------------------------------------------
ourModel

## ----BJCombiner1---------------------------------------------------------
ourModel <- combiner(BJxreg[,-c(3:7,18:22)],bruteForce=TRUE)
summary(ourModel)

## ----BJCombiner2---------------------------------------------------------
ourModel <- combiner(BJxreg,bruteForce=FALSE)
summary(ourModel)

## ----BJCombiner3---------------------------------------------------------
BJInsample <- BJxreg[1:130,];
BJHoldout <- BJxreg[-(1:130),];
ourModel <- combiner(BJInsample,bruteForce=FALSE)

## ----BJCombinerPlot------------------------------------------------------
plot(ourModel)

## ----BJCombinerForecast--------------------------------------------------
ourForecast <- forecast(ourModel,BJHoldout)
plot(ourForecast)

