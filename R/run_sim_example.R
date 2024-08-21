run_sim_example <- function(method_info, simulation_info, overwrite = TRUE) {
  
  # Combine the method information and simulation information
  simulation_arguments <- c(
    method_info, simulation_info
  )
  
  # Run the simulation
  simulation_results <- do.call(sim_example, simulation_arguments)
  simulation_results$call[1] <- "sim_example" ## Need more elegant solution
  
  print(simulation_results)
  
  # Save the results
  indexr::save_objects("./rds", simulation_results, overwrite = overwrite)
  
}