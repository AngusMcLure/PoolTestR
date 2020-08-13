#' Predicting Prevalence from a Bayesian Mixed or Fixed Effect Logistic
#' Regression with Presence/Absence Tests on Pooled Samples
#'
#' This function works somewhat like a \code{predict} or \code{fitted} generic
#' function returning the model predicted prevalence for a given set of data;
#' however, as the quantity of interest (prevalence) is neither on the response
#' or link scale we do not use either of these generic functions. Further, when
#' the model accounts for the heirarchical structure of the sampling frame (e.g.
#' Region/Village/Site), it is typical to want to know the predicted values at
#' each level of sampling (e.g. Prevalence at each region, village or site) so
#' these are calculated automatically.
#'
#' @export
#' @param model An object of class \code{brms} with the regression outputs from
#'   a call to \code{PoolRegBayes}
#' @param data The data for which prevalence needs to be estimated/predicted. If
#'   not provided, defaults to using the data used to train the model (i.e.
#'   returns the fitted values of the prevalence)
#' @return A \code{list} with at least one field \code{PopulationEffects} and an
#'   additional field for every random/group effect variable. The field
#'   \code{PopulationEffects} contains a \code{data.frame} with the prevalence
#'   estimated based only the fixed/population effects. When the intercept is
#'   the only fixed/popultion effect, this is just the population mean (possibly
#'   adjusted for random/group effects). When there are also
#'
#' @example examples/LogisticRegression.R!!!

getPrevalence <- function(model,data = model$data){
  formula <- model$formula$pforms$eta
  PoolSizeName <- sub(".*\\^", "",as.character(model$formula$formula)[[3]]) #The name of the pool size variable in data
  GroupTermNames <- (as.character(formula)[[3]] %>%
                       str_match_all("\\|\\s*(.*?)\\s*\\)"))[[1]][,2] %>%
    strsplit(split = "\\/") %>%
    unlist
  NGroupTerms <- length(GroupTermNames)

  AllTerms <- intersect(colnames(data),unique(c(GroupTermNames, attr(terms(formula),"term.labels"))))
  PopTerms <- setdiff(AllTerms,GroupTermNames)

  #Datapoints to make prediction at
  PredData <- data %>% select_at(AllTerms) %>% unique

  #Make predictions based only on population effects - there might not be any in which case we feed in null data
  if(length(PopTerms)){
    PredDataSub <- PredData %>% select_at(PopTerms) %>% unique()
  }else{
    PredDataSub <- data.frame(DummyVar = 1)
  }
  rownames(PredDataSub) <- NULL
  Prev <- fitted(model,
                scale = 'response',
                re_formula = NA,
                newdata = PredDataSub %>% mutate(!!PoolSizeName := 1)) %>% #Setting PoolSize to 1 means that the response scale is the same as the prevalence scale
    as.data.frame %>%
    select(-Est.Error)
  colnames(Prev) <- c("Estimate", "CrILow", "CrIHigh")
  if(length(PopTerms)){
    predlist <- list(cbind(PredDataSub,Prev))
  }else{
    predlist <- list(cbind(PredDataSub %>% select(-DummyVar),Prev))
  }

  #Make predictions based on group effects
  if(NGroupTerms){
    for(n in 1:NGroupTerms){
      PredDataSub <-  PredData[,c(PopTerms,GroupTermNames[1:n]),drop =F] %>% unique
      rownames(PredDataSub) <- NULL
      GroupEffectForm <- paste("(1|",GroupTermNames[1:n] %>% paste(collapse = "/"),")",sep = "") %>% reformulate()
      Prev = fitted(model,
                    scale = 'response',
                    re_formula = GroupEffectForm,
                    newdata = PredDataSub %>% mutate(!!PoolSizeName := 1)) %>%
        as.data.frame %>%
        select(-Est.Error)
      colnames(Prev) <- c("Estimate", "CrILow","CrIHigh")
      predlist <- c(predlist,
                    list(cbind(PredDataSub,Prev)))
    }
  }
  names(predlist) <- c("PopulationEffects", GroupTermNames)

  return(predlist)
}

