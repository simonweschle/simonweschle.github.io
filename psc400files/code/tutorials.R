

# clear everything in the workspace. put this at the beginning of ALL your code files
rm(list=ls(all=TRUE))


# Tutorial

# execute one by one -- if it asks to update packages, enter 1
remotes::install_github("kosukeimai/qss-package", build_vignettes = TRUE)

remotes::install_github("rstudio/learnr")

remotes::install_github("rstudio-education/gradethis")

remotes::install_github("mattblackwell/qsslearnr")


# start the tutorial
learnr::run_tutorial("00-intro", package = "qsslearnr")
# when done, click ESC in R





