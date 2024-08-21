run_data_analysis <- function(method_info, data, overwrite = TRUE) {
  
  # Combine the method information and simulation information
  analysis_arguments <- c(
    method_info, data
  )
  
  # Run the simulation
  analysis_results <- do.call(data_analysis, analysis_arguments)
  analysis_results$call[1] <- "data_analysis" ## Need more elegant solution
  
  # Save the results
  indexr::save_objects("./rds", analysis_results, overwrite = overwrite)
  
}