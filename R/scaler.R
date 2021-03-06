#' Scale Model
#'
#' This method produces a model for scale of distribution for the provided pre-estimated model.
#' The model can be estimated either via \code{lm} or \code{alm}.
#'
#' This function is useful, when you suspect a heteroscedasticity in your model and want to
#' fit a model for the scale of the pre-specified distribution. This function is complementary
#' for \code{lm} or \code{alm}.
#'
#' @template author
#'
#' @param model The pre-estimated \code{alm} or \code{lm} model.
#' @param formula The formula for scale. It should start with ~ and contain all variables
#' that should impact the scale.
#' @param data The data, on which the scale model needs to be estimated. If not provided,
#' then the one used in the \code{model} is used.
#' @param parameters The parameters to use in the model. Only needed if you know the parameters
#' in advance or want to test yours.
#' @param ... Other parameters to pass to the method, including those explained in
#' \link[greybox]{alm} (e.g. parameters for optimiser).
#' @examples
#'
#' xreg <- cbind(rnorm(100,10,3),rnorm(100,50,5))
#' xreg <- cbind(100+0.5*xreg[,1]-0.75*xreg[,2]+rnorm(100,0,3),xreg,rnorm(100,300,10))
#' colnames(xreg) <- c("y","x1","x2","Noise")
#'
#' # Estimate the location model
#' ourModel <- alm(y~.,xreg)
#' # Estimate the scale model
#' ourScale <- sm(ourModel,formula=~x1+x2)
#' # Summary of the scale model
#' summary(ourScale)
#'
#' @rdname sm
#' @export
sm <- function(model, formula=NULL, data=NULL, parameters=NULL, ...) UseMethod("sm")

#' @rdname sm
#' @export
sm.default <- function(model, formula=NULL, data=NULL, parameters=NULL, ...){
    # The function creates a scale model for the provided model
    distribution <- "dnorm";
    if(is.null(data)){
        if(is.matrix(model$model)){
            data <- model$model;
        }
        else if(is.matrix(model$data)){
            data <- model$data;
        }
    }
    if(is.null(formula)){
        formula <- formula(model);
        formula[[2]] <- NULL;
    }
    return(do.call("scaler",list(formula, data, model$call$subset, model$call$na.action,
                                 distribution, fitted(model), actuals(model), residuals(model),
                                 parameters, cl=model$call, ...)));
}

#' @rdname sm
#' @export
sm.lm <- function(model, formula=NULL, data=NULL,
                  parameters=NULL, ...){
    # The function creates a scale model for the provided model
    distribution <- "dnorm";
    if(is.null(data)){
        data <- model$model;
    }
    if(is.null(formula)){
        formula <- formula(model);
        formula[[2]] <- NULL;
    }
    return(do.call("scaler",list(formula, data, model$call$subset, model$call$na.action,
                                 distribution, fitted(model), actuals(model), residuals(model),
                                 parameters, cl=model$call, ...)));
}

#' @rdname sm
#' @export
sm.alm <- function(model, formula=NULL, data=NULL,
                   # orders=c(0,0,0),
                   parameters=NULL, ...){
    # The function creates a scale model for the provided model
    distribution <- model$distribution;
    cl <- model$call;
    if(is.null(data)){
        data <- model$call$data;
    }
    if(is.null(formula)){
        formula <- formula(model);
        formula[[2]] <- NULL;
    }
    return(do.call("scaler",list(formula, data, model$call$subset, model$call$na.action,
                                 distribution, model$mu, actuals(model), residuals(model),
                                 parameters, model$occurrence, model$other, cl=cl, ...)));
}

scaler <- function(formula, data, subset=NULL, na.action=NULL, distribution, mu, y, residuals,
                   parameters=NULL, occurrence=NULL, other=NULL, ...){
    # The function estimates the scale model

    # Start measuring the time of calculations
    startTime <- Sys.time();

    cl <- match.call();

    obsInsample <- length(y);

    # Extract the other value
    if(!is.null(other)){
        other <- switch(distribution,
                        "dgnorm"=,
                        "dlgnorm"=other$shape,
                        "dalaplace"=other$alpha,
                        "dnbinom"=other$size,
                        "dchisq"=other$nu,
                        "dbcnorm"=other$lambdaBC,
                        "dt"=other$df,
                        NULL);
    }

    #### Ellipsis values ####
    ellipsis <- list(...);
    # Fisher Information
    if(is.null(ellipsis$FI)){
        FI <- FALSE;
    }
    else{
        FI <- ellipsis$FI;
    }
    # Starting values for the optimiser
    if(is.null(ellipsis$B)){
        B <- NULL;
    }
    else{
        B <- ellipsis$B;
    }
    # Parameters for the nloptr from the ellipsis
    if(is.null(ellipsis$xtol_rel)){
        xtol_rel <- 1E-6;
    }
    else{
        xtol_rel <- ellipsis$xtol_rel;
    }
    if(is.null(ellipsis$algorithm)){
        # if(recursiveModel){
        # algorithm <- "NLOPT_LN_BOBYQA";
        # }
        # else{
        algorithm <- "NLOPT_LN_SBPLX";
        # }
    }
    else{
        algorithm <- ellipsis$algorithm;
    }
    if(is.null(ellipsis$maxtime)){
        maxtime <- -1;
    }
    else{
        maxtime <- ellipsis$maxtime;
    }
    if(is.null(ellipsis$xtol_abs)){
        xtol_abs <- 1E-8;
    }
    else{
        xtol_abs <- ellipsis$xtol_abs;
    }
    if(is.null(ellipsis$ftol_rel)){
        ftol_rel <- 1E-6;
    }
    else{
        ftol_rel <- ellipsis$ftol_rel;
    }
    if(is.null(ellipsis$ftol_abs)){
        ftol_abs <- 0;
    }
    else{
        ftol_abs <- ellipsis$ftol_abs;
    }
    if(is.null(ellipsis$print_level)){
        print_level <- 0;
    }
    else{
        print_level <- ellipsis$print_level;
    }
    print_level_hidden <- print_level;
    if(print_level==41){
        print_level[] <- 0;
    }
    if(is.null(ellipsis$stepSize)){
        stepSize <- .Machine$double.eps^(1/4);
    }
    else{
        stepSize <- ellipsis$stepSize;
    }

    if(is.null(ellipsis$cl)){
        responseName <- "y";
    }
    else{
        responseName <- formula(ellipsis$cl)[[2]];
    }

    occurrenceModel <- FALSE;
    obsZero <- 0;
    otU <- rep(TRUE,obsInsample);
    if(is.occurrence(occurrence)){
        occurrenceModel[] <- TRUE;
        otU[] <- actuals(occurrence)!=0;
        obsZero[] <- sum(!otU);
    }
    # Deal with subset
    if(is.null(subset)){
        subset <- c(1:obsInsample);
    }
    else if(is.logical(subset)){
        otU <- otU & subset;
    }
    else{
        otU <- which(otU) == subset;
    }

    if(any(residuals[otU]==0)){
        otU <- otU & residuals[subset]!=0;
    }

    # Prepare the data
    mf <- match.call(expand.dots = FALSE);
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L);
    mf <- mf[c(1L, m)];
    mf$drop.unused.levels <- TRUE;
    if(is.name(mf$data)){
        mf$data <- eval(mf$data);
    }
    if(!is.data.frame(mf$data)){
        mf$data <- data.frame(mf$data);
    }
    if(is.logical(mf$subset)){
        mf$subset <- which(mf$subset)
    }
    mf[[1L]] <- quote(stats::model.frame);
    dataWork <- eval(mf, parent.frame());
    dataTerms <- terms(dataWork);
    matrixXregScale <- model.matrix(dataWork,data=dataWork);
    # Extract number of variables and their names
    nVariables <- ncol(matrixXregScale);
    variablesNames <- colnames(matrixXregScale);

    fitterScale <- function(B, distribution){
        scale <- exp(matrixXregScale %*% B);
        scale[] <- switch(distribution,
                          "dnorm"=,
                          "dlnorm"=,
                          "dbcnorm"=,
                          "dlogitnorm"=,
                          "dfnorm"=,
                          "dlogis"=sqrt(scale),
                          "dlaplace"=,
                          "dllaplace"=,
                          "dalaplace"=scale,
                          "ds"=,
                          "dls"=scale^2,
                          "dgnorm"=,
                          "dlgnorm"=scale^{1/other},
                          "dgamma"=,
                          "dinvgauss"=scale,
                          # "dgamma"=sqrt(scale)+1,
                          # This is based on polynomial from y = (x-1)^2/x
                          # "dinvgauss"=(scale+2+sqrt(scale^2+4*scale))/2,
                          scale);
        return(scale);
    }

    #### The function estimates parameters of scale model ####
    CFScale <- function(B){
        scale <- fitterScale(B, distribution);
        CFValue <- -sum(switch(distribution,
                               "dnorm" = dnorm(y[otU], mean=mu[otU], sd=scale, log=TRUE),
                               "dlaplace" = dlaplace(y[otU], mu=mu[otU], scale=scale, log=TRUE),
                               "ds" = ds(y[otU], mu=mu[otU], scale=scale, log=TRUE),
                               "dgnorm" = dgnorm(y[otU], mu=mu[otU], scale=scale,
                                                 shape=other, log=TRUE),
                               "dlogis" = dlogis(y[otU], location=mu[otU], scale=scale, log=TRUE),
                               "dt" = dt(y[otU]-mu[otU], df=scale, log=TRUE),
                               "dalaplace" = dalaplace(y[otU], mu=mu[otU], scale=scale,
                                                       alpha=other, log=TRUE),
                               "dlnorm" = dlnorm(y[otU], meanlog=mu[otU], sdlog=scale, log=TRUE),
                               "dllaplace" = dlaplace(log(y[otU]), mu=mu[otU],
                                                      scale=scale, log=TRUE)-log(y[otU]),
                               "dls" = ds(log(y[otU]), mu=mu[otU], scale=scale, log=TRUE)-log(y[otU]),
                               "dlgnorm" = dgnorm(log(y[otU]), mu=mu[otU], scale=scale,
                                                  shape=other, log=TRUE)-log(y[otU]),
                               "dbcnorm" = dbcnorm(y[otU], mu=mu[otU], sigma=scale,
                                                   lambda=other, log=TRUE),
                               "dfnorm" = dfnorm(y[otU], mu=mu[otU], sigma=scale, log=TRUE),
                               "dinvgauss" = dinvgauss(y[otU], mean=mu[otU],
                                                       dispersion=scale/mu[otU], log=TRUE),
                               "dgamma" = dgamma(y[otU], shape=1/scale,
                                                 scale=scale*mu[otU], log=TRUE),
                               "dchisq" = dchisq(y[otU], df=scale, ncp=mu[otU], log=TRUE),
                               "dpois" = dpois(y[otU], lambda=mu[otU], log=TRUE),
                               "dnbinom" = dnbinom(y[otU], mu=mu[otU], size=scale, log=TRUE),
                               "dlogitnorm" = dlogitnorm(y[otU], mu=mu[otU], sigma=scale, log=TRUE)
                               # "dbeta" = dbeta(y[otU], shape1=mu[otU], shape2=scale, log=TRUE),
                               # "pnorm" = c(pnorm(mu[otU][ot], mean=0, sd=1, log.p=TRUE),
                               #             pnorm(mu[otU][!ot], mean=0, sd=1, lower.tail=FALSE, log.p=TRUE)),
                               # "plogis" = c(plogis(mu[otU][ot], location=0, scale=1, log.p=TRUE),
                               #              plogis(mu[otU][!ot], location=0, scale=1, lower.tail=FALSE, log.p=TRUE))
        ));

        # The differential entropy for the models with the missing data
        if(occurrenceModel){
            CFValue[] <- CFValue + sum(switch(distribution,
                                              "dnorm" =,
                                              "dfnorm" =,
                                              "dbcnorm" =,
                                              "dlogitnorm" =,
                                              "dlnorm" = obsZero*(log(sqrt(2*pi)*scale[!otU])+0.5),
                                              "dgnorm" =,
                                              "dlgnorm" =obsZero*(1/other-
                                                                      log(other /
                                                                              (2*scale[!otU]*gamma(1/other)))),
                                              # "dinvgauss" = 0.5*(obsZero*(log(pi/2)+1+suppressWarnings(log(scale[!otU])))-
                                              #                                 sum(log(mu[!otU]))),
                                              "dinvgauss" = obsZero*(0.5*(log(pi/2)+1+suppressWarnings(log(scale[!otU])))),
                                              "dgamma" = obsZero*(1/scale[!otU] + log(scale[!otU]) +
                                                                      log(gamma(1/scale[!otU])) +
                                                                      (1-1/scale[!otU])*digamma(1/scale[!otU])),
                                              "dlaplace" =,
                                              "dllaplace" =,
                                              "ds" =,
                                              "dls" = obsZero*(2 + 2*log(2*scale[!otU])),
                                              "dalaplace" = obsZero*(1 + log(2*scale[!otU])),
                                              "dlogis" = obsZero*2,
                                              "dt" = obsZero*((scale[!otU]+1)/2 *
                                                                  (digamma((scale[!otU]+1)/2)-digamma(scale[!otU]/2)) +
                                                                  log(sqrt(scale[!otU]) * beta(scale[!otU]/2,0.5))),
                                              "dchisq" = obsZero*(log(2)*gamma(scale[!otU]/2)-
                                                                      (1-scale[!otU]/2)*digamma(scale[!otU]/2)+
                                                                      scale[!otU]/2),
                                              # "dbeta" = sum(log(beta(mu[otU],scale[!otU][otU]))-
                                              #                   (mu[otU]-1)*
                                              #                   (digamma(mu[otU])-
                                              #                        digamma(mu[otU]+scale[!otU][otU]))-
                                              #                   (scale[!otU][otU]-1)*
                                              #                   (digamma(scale[!otU][otU])-
                                              #                        digamma(mu[otU]+scale[!otU][otU]))),
                                              # This is a normal approximation of the real entropy
                                              # "dpois" = sum(0.5*log(2*pi*scale[!otU])+0.5),
                                              # "dnbinom" = obsZero*(log(sqrt(2*pi)*scale[!otU])+0.5),
                                              0
            ));
        }
        return(CFValue);
    }

    # Prepare parameters
    if(is.null(B)){
        if(any(distribution==c("dnorm","dlnorm","dbcnorm","dlogitnorm","dfnorm","dlogis"))){
            B <- .lm.fit(matrixXregScale[otU,,drop=FALSE],2*log(abs(residuals[otU])))$coefficients;
        }
        else if(any(distribution==c("dlaplace","dllaplace","dalaplace"))){
            B <- .lm.fit(matrixXregScale[otU,,drop=FALSE],log(abs(residuals[otU])))$coefficients;
        }
        else if(any(distribution==c("ds","dls"))){
            B <- .lm.fit(matrixXregScale[otU,,drop=FALSE],0.5*log(abs(residuals[otU])))$coefficients;
        }
        else if(any(distribution==c("dgnorm","dlgnorm"))){
            B <- .lm.fit(matrixXregScale[otU,,drop=FALSE],other+other*log(abs(residuals[otU])))$coefficients;
        }
        else if(distribution=="dgamma"){
            B <- .lm.fit(matrixXregScale[otU,,drop=FALSE],2*log(abs(residuals[otU]-1)))$coefficients;
        }
        else if(distribution=="dinvgauss"){
            B <- .lm.fit(matrixXregScale[otU,,drop=FALSE],log(abs(residuals[otU]-1)^2/residuals[otU]))$coefficients;
        }
        # Other distributions: dt, dchisq, dnbinom, dpois, pnorm, plogis, dbeta
        else{
            B <- .lm.fit(matrixXregScale[otU,,drop=FALSE],log(abs(residuals[otU])))$coefficients;
        }
    }
    names(B) <- variablesNames;

    BLower <- rep(-Inf,nVariables);
    BUpper <- rep(Inf,nVariables);

    #### Define what to do with the maxeval ####
    if(is.null(ellipsis$maxeval)){
        maxeval <- length(B) * 40;
    }
    else{
        maxeval <- ellipsis$maxeval;
    }

    res <- nloptr(B, CFScale,
                  opts=list(algorithm=algorithm, xtol_rel=xtol_rel, maxeval=maxeval, print_level=print_level,
                            maxtime=maxtime, xtol_abs=xtol_abs, ftol_rel=ftol_rel, ftol_abs=ftol_abs),
                  lb=BLower, ub=BUpper);

    B[] <- res$solution;
    CFValue <- res$objective;

    if(print_level_hidden>0){
        print(res);
    }

    scale <- fitterScale(B, distribution);
    #### !!!! This needs to be double checked
    errors <- switch(distribution,
                     "dnorm"=,
                     "dlnorm"=,
                     "dbcnorm"=,
                     "dlogitnorm"=,
                     "dfnorm"=,
                     "dlogis"=,
                     "dlaplace"=,
                     "dllaplace"=,
                     "dalaplace"=abs(residuals[subset]),
                     "ds"=,
                     "dls"=residuals[subset]^2,
                     "dgnorm"=,
                     "dlgnorm"=abs(residuals[subset])^{1/other},
                     "dgamma"=abs(residuals[subset]),
                     "dinvgauss"=abs(residuals[subset]));

    #### Produce Fisher Information ####
    if(FI){
        # Only vcov is needed, no point in redoing the occurrenceModel
        occurrenceModel <- FALSE;
        FI <- hessian(CFScale, B, h=stepSize);

        if(any(is.nan(FI))){
            warning("Something went wrong and we failed to produce the covariance matrix of the parameters.\n",
                    "Obviously, it's not our fault. Probably Russians have hacked your computer...\n",
                    "Try a different distribution maybe?", call.=FALSE);
            FI <- diag(1e+100,nVariables);
        }
        dimnames(FI) <- list(variablesNames,variablesNames);
    }

    # Write the original residuals as the response variable
    interceptIsNeeded <- attr(dataTerms,"intercept")!=0;
    if(interceptIsNeeded){
        matrixXregScale[,1] <- errors;
    }
    else{
        matrixXregScale <- cbind(errors,matrixXregScale);
    }
    colnames(matrixXregScale)[1] <- "residuals";
    errors[] <- errors / scale;

    # If formula does not have response variable, update it.
    # This is mainly needed for the proper plots and outputs
    if(length(formula)==2){
        cl$formula <- update.formula(formula,paste0(responseName,"~."))
    }

    # Form the scale object
    finalModel <- structure(list(formula=formula, coefficients=B, fitted=scale, residuals=errors,
                                 df.residual=obsInsample-nVariables, df=nVariables, call=cl, rank=nVariables,
                                 data=matrixXregScale, terms=dataTerms, logLik=-CFValue,
                                 occurrence=occurrence, subset=subset, other=ellipsis, B=B, FI=FI,
                                 distribution=distribution, other=other, loss="likelihood",
                                 timeElapsed=Sys.time()-startTime),
                            class=c("scale","alm","greybox"));
    return(finalModel);
}
