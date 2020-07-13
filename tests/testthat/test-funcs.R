test_that("reading table names works", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  names <- table_names(filename)
  expect_equal(names,
               c("AK4Puppi", "AddCA15Puppi", "CA15Puppi", "Electron", "GenEvtInfo", "Info", "Muon", "Photon", "Tau"))
})

test_that("reading tables names with a pattern work", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  info_names <- table_names(filename, pattern = "Info$")
  expect_equal(info_names, c("GenEvtInfo", "Info"))
})

test_that("reading a single whole dataset works", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  file <- hdf5r::h5file(filename, mode = "r")
  on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  taus <- file[["Tau"]]
  pt <- read_dset_data(taus, "pt")
  expect_equal(length(pt), 2L)
})

test_that("reading part of a dataset works", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  file <- hdf5r::h5file(filename, mode = "r")
  on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  ak4 <- file[["AK4Puppi"]]
  partonFlavor <- read_dset_data(ak4, "partonFlavor", 2, 3)
  expect_equal(partonFlavor, c(21, 1))
})

test_that("reading a whole table works", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  taus <- read_h5_table(filename, "Tau")
  expect_s3_class(taus, "tbl_df")
  expect_named(taus, c("antiEleMVA5",
                       "antiEleMVA5Cat",
                       "dzLeadChHad",
                       "e",
                       "eta",
                       "evtNum",
                       "hpsDisc",
                       "lumisec",
                       "m",
                       "nSignalChHad",
                       "nSignalGamma",
                       "phi",
                       "pt",
                       "puppiChHadIso",
                       "puppiChHadIsoNoLep",
                       "puppiGammaIso",
                       "puppiGammaIsoNoLep",
                       "puppiNeuHadIso",
                       "puppiNeuHadIsoNoLep",
                       "q",
                       "rawIso3Hits",
                       "rawIsoMVA3newDMwLT",
                       "rawIsoMVA3newDMwoLT",
                       "rawIsoMVA3oldDMwLT",
                       "rawIsoMVA3oldDMwoLT",
                       "rawMuonRejection",
                       "runNum"))
  expect_equal(taus$q, c(1,-1))
})

test_that("reading part of a table works", {
  root <- rprojroot::is_r_package
  filename <- root$find_file("inst/extdata/cms_two_events.h5")
  ak4s <-   read_h5_table(filename, "AK4Puppi", first = 2, last = 3)
  expect_s3_class(ak4s, "tbl_df")
  expect_equal(nrow(ak4s), 2L)

})

