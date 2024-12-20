# Check whether the SimpleExampleData is formatted 
# appropriately for estimating prevalence in 
# PoolTestR
SimpleExample_output <- 
  CheckInputData(
    data = SimpleExampleData, 
    result = "Result", poolSize = "NumInPool"
  )
# No errors/warnings were raised
identical(SimpleExample_output, SimpleExampleData)
# The hierarchical scheme is formatted properly so
# the output is identical to the input


\dontrun{
  # Error raised when input data is not class data.frame
  CheckInputData(
    data = 1, 
    result = "Result", poolSize = "NumInPool"
  )
  # Error raised when result/poolSize column names are incorrect
  CheckInputData(
    data = SimpleExampleData, 
    result = "WrongResultName", poolSize = "WrongNumInPoolName"
  )
  # Error raised when optional stratifying variable column names are incorrect
  CheckInputData(
    data = SimpleExampleData, 
    result = "Result", poolSize = "NumInPool",
    "WrongRegionName", "WrongYearName"
  )
  # Error raised when Result/poolSize columns are not numeric/integer
  CheckInputData(
    data = SimpleExampleData %>%
      mutate(Result = as.character(.data$Result),
             .keep = "all"), 
    result = "Result", poolSize = "NumInPool"
  )
  CheckInputData(
    data = SimpleExampleData %>%
      mutate(NumInPool = as.character(.data$NumInPool),
             .keep = "all"), 
    result = "Result", poolSize = "NumInPool"
  )
  # Error raised when Result column values are not numeric 0 and 1
  CheckInputData(
    data = SimpleExampleData %>%
      mutate(Result = 2,
             .keep = "all"), 
    result = "Result", poolSize = "NumInPool"
  )
  # Error raised when poolSize column values are not positive
  CheckInputData(
    data = SimpleExampleData %>%
      mutate(NumInPool = (-1*.data$NumInPool),
             .keep = "all"), 
    result = "Result", poolSize = "NumInPool"
  )
}

