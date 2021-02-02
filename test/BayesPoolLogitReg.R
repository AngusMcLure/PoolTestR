#' Bayesian Logistic Regression with Presence/Absence Tests on Pooled Samples
#'
#' @export
#' @param data A \code{data.frame} with one row for each pooled sampled and columns for
#'             the size of the pool (i.e. the number of specimens / isolates / insects
#'             pooled to make that particular pool), the result of the test of the pool
#'             and any number of columns to be used as the dependent variables in the
#'             logistic regression
#' @param PoolSize A \code{string} giving the name of the column with number of specimens/isolates/insects in each pool
#' @param formula A \code{formula} of the kind used to define models in \code{glm}. The left-hand
#'                side should be the name of column with the result of each test on each pooled
#'                sample. The result must be stored with 1 indicating a positive test result and
#'                0 indicating a negative test result.
#' @param alpha The confidence level to be used for the confidence and credible intervals.
#'              Defaults to 0.5\% (i.e. 95\% intervals)
#' @param verbose Logical indicating whether to print progress to screen. Defaults to false
#'                (no printing to screen)
#' @return TBC
#'
#' @examples
#' #Create a synthetic dataset of prevalence in different times and places
#' #Locations A-C have different baseline prevalences.
#' #Prevalence at D = Prevalence at A.
#' #All locations have the same expoential growth rate, -0.5, i.e. falling prevalence
#'
#' N <- 500
#' DataTrend <- data.frame(Place = sample(c("A","B","C","D"),N, replace = TRUE),
#'                         Year = sample(c(0:2), N, replace = TRUE),
#'                         NumInPool = sample(25:30, N, replace = TRUE)
#'                         )
#' BasePrev <- c(A = 0.1, B = 0.05, C = 0.02, D = 0.1) #The baseline prevalences at each location
#' GrowthRate = -0.5 #The (negative) growth rate
#' #Calculate the true prevalence at each time and place
#' DataTrend$TruePrev <- with(DataTrend,
#'                           BasePrev[Place] * exp(GrowthRate*(Year-min(Year))))
#' #Simulate some pooled samples from each location
#' DataTrend$Result <- with(DataTrend,
#'                          runif(N) < 1-(1-TruePrev)^NumInPool)
#' #Perform logistic regression with explanatory variables Place and Year
#' Reg <- BayesPoolLogitReg(DataTrend,"NumInPool",Result ~ Year + Place)
#' Reg$OR

BayesPoolLogitReg <- function(data, PoolSize, formula, alpha=0.05,verbose = FALSE){

  if(!attr(terms(formula),"response")){
    stop("formula needs left hand side")
  }
  Result <- all.vars(formula)[1]

  #Make a count of the number of cases in each time period and each group (defined by covariates supplied on the rhs of Formula)

  GroupingVars <- labels(terms(formula))
  if(length(GroupingVars) == 0){
    stop("formula needs a right-hand side")
  }

  MM <- model.matrix(formula, data = data)

  #This is now done inside stan
  # NormaliseColumns <- function(matrix){
  #   for(n in 1:ncol(matrix)){
  #     clmn <- matrix[,n]
  #     maxc <- max(clmn)
  #     minc <- min(clmn)
  #     if(maxc != minc){
  #       matrix[,n] <- (clmn - minc)/(maxc-minc)
  #     }else{
  #       matrix[,n] <- 1
  #     }
  #   }
  #   return(matrix)
  # }

  #MMNorm <- NormaliseColumns(MM)

  NumParams <- dim(MM)[2] -1

  sdata <- list(N = dim(MM)[1],
                A = NumParams + 1,
                Result = data[,Result],
                PoolSize = data[,PoolSize],
                MM = MM)
  sfit <- sampling(stanmodels$PooledLogisticRegression,
                   data = sdata,
                   pars = c('Alpha'),
                   chains = 1,
                   iter = 2000,
                   warmup = 1000,
                   refresh = ifelse(verbose,200,0),
                   cores = 1,
                   init = 0)

  OR <- summary(sfit,probs = c(alpha/2,1-alpha/2))$summary[2:(NumParams+1),,drop=FALSE]  %>%
    as.data.frame() %>%
    select_at(c("mean",paste0(as.character(sort(c(alpha/2,1-alpha/2)*100)),'%'))) %>%
    rename(OR = 'mean') %>%
    exp
  rownames(OR) <- dimnames(MM)[[2]][2:(NumParams+1)]

  return(list(OR = OR, fit = sfit, input = sdata))
}

