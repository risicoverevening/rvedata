## code to prepare `example_phd_data` dataset goes here

example_phd_data <- rvedata::phd_data(10000, 1234)

usethis::use_data(example_phd_data, overwrite = TRUE)
