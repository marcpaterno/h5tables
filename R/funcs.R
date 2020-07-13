#'  Read a tibble from a tabular HDF5 file.
#'
#' @param file an H5File object representing an HDF5 "tabular data" file, or the name of a file.
#' @param tablename The name of the table to be read.
#'
#' @return a tibble containing the data read from the named table.
#' @export
#'
read_h5_table <- function(file, tablename)
{
  checkmate::assertCharacter(tablename, len = 1)
  checkmate::assertMultiClass(file, c("H5File","character"))
  if (is.character(file)) {
    file <- hdf5r::h5file(file, mode = "r")
    # We only close the file if we were responsible for opening it.
    on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  }
  do_read_h5_table(file, tablename)
}

#' Read a single dataset from an open group
#'
#' @param group an open H5Group object.
#' @param name the name of a dataset in the group.
#'
#' @return a vector containing the data that was read.
#'
read_dset_data <- function(group, name)
{
  dset <- group[[name]]
  on.exit(hdf5r::h5close(dset), add = TRUE, after = FALSE)
  hdf5r::readDataSet(dset)
}

#' do_read_h5_table
#'
#' @param h5f an open H5File object.
#' @param tablename The name of the table to be read.
#'
#' @return a tibble, created from the table.
#'
do_read_h5_table <- function(h5f, tablename)
{
  group <- h5f[[tablename]]
  on.exit(hdf5r::h5close(group), add = TRUE, after = FALSE)
  dsetnames <- hdf5r::list.datasets(group, full.names = FALSE, recursive = FALSE)
  dsets     <- purrr::map(dsetnames, function(n){read_dset_data(group, n)})
  names(dsets) <- dsetnames
  res       <- tibble::as_tibble(data.frame(dsets))
  names(res) <- dsetnames
  res
}

#' Return the names of all tables in a tabular HDF5 file.
#'
#' @param file an H5File object representing an HDF5 "tabular data" file, or the name of such a file.
#'
#' @return a vector (mode character) containing the names of the tables in the file.
#' @export
#'
table_names <- function(file)
{
  checkmate::assertMultiClass(file, c("H5File","character"))
  if (is.character(file)) {
    file <- hdf5r::h5file(file, "r")
    on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  }
  hdf5r::list.groups(file)
}


