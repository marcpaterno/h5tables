test_that("reading table names works", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  names <- table_names(filename)
  expect_equal(names,
               c("AK4Puppi", "AddCA15Puppi", "CA15Puppi", "Electron", "GenEvtInfo", "Info", "Muon", "Photon", "Tau"))
})

test_that("reading a single data set works", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  file <- hdf5r::h5file(filename, mode = "r")
  on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  taus <- file[["Tau"]]
  pt <- .read_dset_data(taus, "pt")
  expect_equal(length(pt), 2L)
})

test_that("reading a whole table works", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  d <- read_h5_table(filename, "Muon")
  expect_s3_class(d, "tbl_df")
})