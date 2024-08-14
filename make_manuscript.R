library(targets)
library(tarchetypes)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

tar_option_set(
  packages = c("stringr")
)

compile_pdf <- function(tex_file, output_dir = "manuscript", dependencies) {
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
  # tar_target(whoari_comparison, "shared/objects/comparison_plot", format = "file"),
  tar_target(
    name = whoari_plot,
    command = save_plot(readRDS(whoari_comparison), "./manuscript/fig/comparison_data.pdf"),
    format = "file"
  ),
  tar_file_read(
    main,
    "./manuscript/main.tex",
    readLines(!!.x)
  ),
  tar_file_read(
    format,
    "./manuscript/lasso-inf-alts.tex",
    readLines(!!.x)
  ),
  tar_file_read(
    bib,
    "./manuscript/lasso-inf-alts.bib",
    readLines(!!.x)
  ),
  tar_target(
    name = manuscript_pdf,
    command = compile_pdf("lasso-inf-alts.tex", dependencies = c(whoari_plot, main, format, bib)),
    format = "file"
  )
)
