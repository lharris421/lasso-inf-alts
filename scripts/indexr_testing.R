library(glue)
devtools::load_all()
indexr::update_hash_table("./rds_testing/current_sims.csv", "./rds_testing")
