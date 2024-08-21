run_sim <- function(method_info, simulation_info, overwrite = TRUE) {
  
  # Combine the method information and simulation information
  simulation_arguments <- c(
    method_info, simulation_info
  )
  
  # Run the simulation
  simulation_results <- do.call(sim, simulation_arguments)
  simulation_results$call[1] <- "sim" ## Need more elegant solution
  
  # Save the results
  indexr::save_objects("./rds", simulation_results, overwrite = overwrite)
  
}