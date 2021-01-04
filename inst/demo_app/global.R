library(shiny)
library(shinydrive)

tmpdir <- tempdir()

utils::unzip("test_files.zip", exdir = tmpdir)

sd_path <- file.path(tmpdir, "test")

# to show / test update save_dir as reactive :

tmpdir2 <- file.path(tmpdir, "test2")

if(!dir.exists(tmpdir2)){
  dir.create(tmpdir2)
}

utils::unzip("test_files.zip", exdir = tmpdir2)

sd_path_2 <- file.path(tmpdir2, "test")

suppress_file_in_dir(id = "1",
                     yml = file.path(sd_path_2, "files_desc.yaml"),
                     dir = sd_path_2)


