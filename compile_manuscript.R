library(targets)
tar_make(script = "make_whoari_comparison.R", store = "whoari")
tar_make(script = "make_manuscript.R", store = "manuscript_store")

## Main piece missing is an actual dependency structure 
## The user has to make sure that all files are included and being updated
## Not that there isn't a way, I just haven't found one I like yet