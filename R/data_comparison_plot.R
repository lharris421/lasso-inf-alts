compare_methods <- function(res_list) {
  
  # Combine all results into one data frame, using the list names as method names
  combined_res <- purrr::imap_dfr(res_list, ~ .x %>%
                                    dplyr::select(variable, estimate, lower, upper) %>%
                                    dplyr::mutate(method = .y))
  
  # Create the plot
  plot <- ggplot(combined_res) +
    geom_errorbar(aes(xmin = lower, xmax = upper, y = variable, color = method), alpha = 0.8) +
    geom_point(aes(x = estimate, y = variable, color = method), alpha = 0.8) +
    theme_bw() +
    labs(y = "Variable", x = "Estimate")
  
  return(plot)
  
}

