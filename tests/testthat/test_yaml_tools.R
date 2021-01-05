context("yaml tools")

test_that("yaml", {

  ##Test files generation
  demo_zip <- system.file("demo_app/test_files.zip", package = "shinydrive")
  dir_tp <- tempdir()
  unzip(demo_zip, exdir = dir_tp)
  dir <- file.path(dir_tp, "test")
  files_test <- list.files(dir, full.names = TRUE)

  ##Yaml load
  yml <- grep("yaml", files_test)
  yml <- files_test[yml]

  ##Yml test
  yml_load <- yaml::yaml.load_file(yml)

  file <- system.file("translate/translate.csv", package = "shinydrive")

  # add one first file
  add_file_in_dir(
    file = file,
    dir = dir,
    yml = yml,
    name = "translate1",
    description = ""
  )

  ##Yml test
  yml_load2 <- yaml::yaml.load_file(yml)

  expect_true(length(yml_load) == length(yml_load2)-1)
  nm1 <- unlist(lapply(yml_load2, function(X)X$name))
  nm2 <- unlist(lapply(yml_load, function(X)X$name))
  expect_true(setdiff(nm1, nm2) == "translate1")

  # add second file
  add_file_in_dir(
    file = file,
    dir= dir,
    yml = yml,
    name = "translate_2",
    description = "This is cool"
  )

  yml_load3 <- yaml::yaml.load_file(yml)

  expect_true(length(yml_load) == length(yml_load3)-2)
  nm1 <- unlist(lapply(yml_load2, function(X)X$name))
  nm2 <- unlist(lapply(yml_load3, function(X)X$name))

  expect_true(unname(nm2[!nm2%in%nm1]) == "translate_2")

  # suppress first file
  suppress_file_in_dir(id = "1", dir = dir, yml = yml)

  yml_load4 <- yaml::yaml.load_file(yml)
  nm1 <- unlist(lapply(yml_load3, function(X)X$name))
  nm2 <- unlist(lapply(yml_load4, function(X)X$name))
  expect_false(unname(nm1[1]) %in% unname(nm2))

  #Dir
  expect_error(  add_file_in_dir(
    file = file,
    dir = "toto",
    yml = yml,
    name = "translate_2",
    description = "This is cool"
  ))
})
