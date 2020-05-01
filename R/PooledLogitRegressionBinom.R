#' Logistic regression with presence/absence tests on pooled samples
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

PooledRegressionBinom <- function(data, PoolSize, formula, alpha=0.05,verbose = F){

  if(!attr(terms(formula),"response")){
    stop("formula needs left hand side")
  }
  Result <- all.vars(formula)[1]

  #Make a count of the number of cases in each time period and each group (defined by covariates supplied on the rhs of Formula)

  GroupingVars <- labels(terms(formula))
  print(GroupingVars)
  if(length(GroupingVars) == 0){
    stop("formula needs a right-hand side")
  }

  #Drop irrelevant columns and rows with missing values
  data <- data[,c(PoolSize,GroupingVars,Result),drop = F] %>% na.omit


  dataCount <- table(data[,c(PoolSize,GroupingVars,Result)]) %>%
    as.data.frame() %>%
    unite(Group,GroupingVars,sep=".") %>%
    structable(data = ., reformulate(c(PoolSize,Result,"Group"))) %>%
    as.table

  #Get the model matrix
  ReducedModelMatrix<- function(formula, data){
    ResVarsNames <- labels(terms(formula))
    Counts = data %>%
      select_at(ResVarsNames) %>%
      group_by_at(ResVarsNames) %>%
      count()
    ModelFormula <- formula
    ModelFormula[[2]] <- as.name('n')
    MM <- model.matrix(ModelFormula, Counts)
    rownames(MM) <-  unite(Counts[,labels(terms(formula)),drop=F],name,sep=".")$name
    return(MM)
  }

  MM <- ReducedModelMatrix(formula, data)

  sdata <- list(N = dim(dataCount)[PoolSize],
                PoolSizes = dimnames(dataCount)[['NumInPool']] %>% as.numeric(),
                G = dim(MM)[1],
                PosPools = dataCount[,"1",],
                NegPools = dataCount[,"0",],
                A = dim(MM)[2],
                MM = MM)
  sfit <- sampling(stanmodels$PooledLogisticRegressionBinom,
                   data = sdata,
                   pars = c('p','alpha'),
                   chains = 1,
                   iter = 2000,
                   warmup = 1000,
                   refresh = ifelse(verbose,200,0),
                   cores = 1)
  return(list(fit = sfit, input = sdata))
}

