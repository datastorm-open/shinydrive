library(shiny)
library(shinydrive)

tmpdir <- tempdir()

utils::unzip("test_files.zip", exdir = tmpdir)

sd_path <- file.path(tmpdir, "test")
