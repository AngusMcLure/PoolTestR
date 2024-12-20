# Check whether the SimpleExampleData is nested 
# appropriately for estimating prevalence in 
# HierPoolPrev()
SimpleExample_output <- 
  PrepareClusterData(
    data = SimpleExampleData, 
    result = "Result", poolSize = "NumInPool", 
    hierarchy = c("Region", "Village", "Site") 
  )
# No errors/warnings were raised
identical(SimpleExample_output, SimpleExampleData)
# The hierarchical scheme is formatted properly so
# the output is identical to the input


\dontrun{
  # Checking another example data set for clustering issues
  # Create a test data frame that has incorrectly nested 
  # Village and Site variables
  check_data <- data.frame(
    Region = rep(c("A", "B"), each = 4),
    Village = rep(rep(c("W", "X"), each = 2), 2),
    Site = c(1:4, 4:1),
    Year = rep(0, 8),
    NumInPool = rep(10, 8),
    Result = c(rep(0, 8))
  )
  # Test whether the data.frame is formatted appropriately
  # for HierPoolPrev()
  check_output <- PrepareClusterData(
    data = check_data, 
    result = "Result", poolSize = "NumInPool", 
    hierarchy = c("Region", "Village", "Site")
  )
  # New column has been added with unique identifier for
  # each location
  check_output$PoolTestR_ID
}

