cmsfile <- function(){
  root <- rprojroot::is_r_package
  root$find_file("inst/extdata/cms_two_events.h5")
}

novafile <- function() {
  root <- rprojroot::is_r_package
  root$find_file("inst/extdata/small.h5")
}

test_that("reading table names works from CMS style file", {
  names <- table_names(cmsfile())
  expect_equal(names, c("AK4Puppi", "AddCA15Puppi", "CA15Puppi", "Electron", "GenEvtInfo", "Info", "Muon", "Photon", "Tau"))
})

test_that("reading table names works from NOvA style file", {
  names <- table_names(novafile())
  expect_equal(names, c("rec.spill", "rec.vtx"))
})

test_that("reading tables names with a pattern work from CMS style file", {
  info_names <- table_names(cmsfile(), pattern = "Info$")
  expect_equal(info_names, c("GenEvtInfo", "Info"))
})

test_that("reading tables names with a pattern work from NOvA style file", {
  info_names <- table_names(novafile(), pattern = "spill$")
  expect_equal(info_names, c("rec.spill"))
})

test_that("reading columns names works from CMS style file", {
  columns <- column_names(cmsfile(), "GenEvtInfo")
  expect_equal(columns, c("evtNum",
                          "id_1",
                          "id_2",
                          "lumisec",
                          "runNum",
                          "scalePDF",
                          "weight",
                          "x_1",
                          "x_2",
                          "xs"))
})

test_that("reading columns names works from NOvA style file", {
  columns <- column_names(novafile(), "rec.vtx")
  expect_equal(columns, c("cycle",
                          "eid",
                          "evt",
                          "evtseq",
                          "nelastic",
                          "nhough",
                          "nvdt",
                          "run",
                          "subevt",
                          "subrun"))
})

test_that("reading the number of rows in a table works from CMS style file", {
  nr <- count_rows(cmsfile(), "AK4Puppi")
  expect_equal(nr, 5L)
})

test_that("reading the number of rows in a table works from NOvA style file", {
  nr <- count_rows(novafile(), "rec.vtx")
  expect_equal(nr, 13658L)
})

test_that("reading a single whole dataset works from CMS style file", {
  file <- hdf5r::h5file(cmsfile(), mode = "r")
  on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  taus <- file[["Tau"]]
  pt <- read_dset_data(taus, "pt")
  expect_equal(length(pt), 2L)
})

test_that("reading a single whole dataset works from NOvA style file", {
  file <- hdf5r::h5file(novafile(), mode = "r")
  on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  spills <- file[["rec.spill"]]
  run <- read_dset_data(spills, "run")
  expect_equal(length(run), 13658L)
  expect_equal(run, rep(11503, 13658))
})

test_that("reading part of a dataset works from CMS style file", {
  file <- hdf5r::h5file(cmsfile(), mode = "r")
  on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  ak4 <- file[["AK4Puppi"]]
  partonFlavor <- read_dset_data(ak4, "partonFlavor", 2, 3)
  expect_equal(partonFlavor, c(21, 1))
})

test_that("reading part of a dataset works from NOvA style file", {
  file <- hdf5r::h5file(novafile(), mode = "r")
  on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  spills <- file[["rec.spill"]]
  run <- read_dset_data(spills, "run", 2, 11)
  expect_equal(run, rep(11503, 10))
})

test_that("reading a whole table works from CMS style file", {
  taus <- read_h5_table(cmsfile(), "Tau")
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

test_that("reading part of a table works from CMS style file", {
  ak4s <-   read_h5_table(cmsfile(), "AK4Puppi", first = 2, last = 3)
  expect_s3_class(ak4s, "tbl_df")
  expect_equal(nrow(ak4s), 2L)
})

test_that("reading selected columns from a table works from CMS style file", {
  ak4s <-   read_h5_table(cmsfile(), "AK4Puppi", columns = c("area", "axis2", "nParticles"))
  expect_s3_class(ak4s, "tbl_df")
  expect_named(ak4s, c("area", "axis2", "nParticles"))
})
