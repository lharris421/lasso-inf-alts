library(targets)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

tar_option_set(
  packages = c("stringr")
)

# compile_manuscript <- function(plots) {
#   
#   print("PLOTS")
#   print(plots)
#   plots = str_remove(plots, "\\/manuscript")
#   # plots = plots[[1]][length(plots[[1]])]
#   
#   # system2("make", args = c("-C", "manuscript"),
#   #         env = c(PLOTS = plots))
#   
#   system2("make", args = c("-C", "manuscript"))
#   
#   return("./manuscript/lasso-inf-alts.pdf")
# }

compile_pdf <- function(tex_file, output_dir = "manuscript") {
  # Construct the full path to the .tex file
  tex_path <- file.path(output_dir, tex_file)
  
  # Run the cleantex command
  withr::with_dir("manuscript",
    system2("./cleantex", args = c("-btq", tex_file))
  )
  
  # Return the path to the compiled PDF
  pdf_file <- sub("\\.tex$", ".pdf", tex_file)
  return(file.path(output_dir, pdf_file))
}



# Replace the target list below with your own:
list(
  tar_target(whoari_comparison, "whoari/objects/comparison_plot", format = "file"),
  tar_target(
    name = whoari_plot,
    command = save_plot(readRDS(whoari_comparison), "./manuscript/fig/comparison_data.pdf"),
    format = "file"
  ),
  # tar_target(
  #   name = manuscript_pdf,
  #   command = compile_manuscript(whoari_plot),
  #   format = "file"
  # ),
  tar_target(
    name = manuscript_pdf,
    command = compile_pdf("lasso-inf-alts.tex"),
    format = "file"
  )
)
