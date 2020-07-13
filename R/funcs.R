#'  Read a tibble from a tabular HDF5 file.
#'
#' @param file an H5File object representing an HDF5 "tabular data" file, or the name of a file.
#' @param tablename The name of the table to be read.
#' @param first first index to read; must be >= 1
#' @param last last index to read; NULL means read to end of data; must be >= first
#' @param columns vector of column names to read; if NULL, read all columns
#'
#' @return a tibble containing the data read from the named table.
#' @export
#'
read_h5_table <- function(file, tablename, first = 1L, last = NULL, columns = NULL)
{
  checkmate::assertCharacter(tablename, len = 1)
  checkmate::assertMultiClass(file, c("H5File","character"))
  checkmate::assert_count(first, positive = TRUE)
  checkmate::assert_count(last, positive = TRUE, null.ok = TRUE)
  checkmate::assert_character(columns, null.ok = TRUE)

  if (is.character(file)) {
    file <- hdf5r::h5file(file, mode = "r")
    # We only close the file if we were responsible for opening it.
    on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  }
  do_read_h5_table(file, tablename, first, last, columns)
}

#' Read a single dataset from an open group
#'
#' @param group an open H5Group object.
#' @param name the name of a dataset in the group.
#' @param first first index to read; must be >= 1
#' @param last last index to read; NULL means read to end of data; must be >= first
#'
#' @return a vector containing the data that was read.
#'
read_dset_data <- function(group, name, first = 1L, last = NULL)
{
  checkmate::assert_count(first, positive = TRUE)
  checkmate::assert_count(last, positive = TRUE, null.ok = TRUE)
  dset <- group[[name]]
  if (is.null(last)) last <- dset$maxdims
  if (last < first) last <- first
  on.exit(hdf5r::h5close(dset), add = TRUE, after = FALSE)
  dset$read(args = list(first:last))
}

#' do_read_h5_table
#'
#' @param h5f an open H5File object.
#' @param tablename The name of the table to be read.
#' @param first index of the first row to read
#' @param last index of the last row to read
#' @param columns vector of column names to read; if NULL, read all columns
#'
#' @return a tibble, created from the table.
#'
do_read_h5_table <- function(h5f, tablename, first, last, columns)
{
  group <- h5f[[tablename]]
  on.exit(hdf5r::h5close(group), add = TRUE, after = FALSE)
  if (is.null(columns)) {
    dsetnames <- hdf5r::list.datasets(group, full.names = FALSE, recursive = FALSE)
  } else {
    dsetnames <- columns
  }
  dsets     <- purrr::map(dsetnames, function(n){read_dset_data(group, n, first, last)})
  names(dsets) <- dsetnames
  res       <- tibble::as_tibble(data.frame(dsets))
  names(res) <- dsetnames
  res
}

#' Return the names of all tables in a tabular HDF5 file.
#'
#' If a regular expression \code{pattern} is supplied, only the names matching that pattern
#' are returned.
#'
#' @param file an H5File object representing an HDF5 "tabular data" file, or the name of such a file.
#' @param pattern a regular expression to match against files; NULL if none is to be used
#'
#' @return a vector (mode character) containing the names of the tables in the file.
#' @export
#'
table_names <- function(file, pattern = NULL)
{
  checkmate::assertMultiClass(file, c("H5File","character"))
  if (is.character(file)) {
    file <- hdf5r::h5file(file, "r")
    on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  }
  names <- hdf5r::list.groups(file)
  if(! is.null(pattern)) names <- grep(pattern, names, value = TRUE)
  names
}

column_names <- function(file, tablename)
{
  checkmate::assertMultiClass(file, c("H5File","character"))
  checkmate::assert_character(tablename, min.len = 1, max.len = 1)
  if (is.character(file)) {
    file <- hdf5r::h5file(file, "r")
    on.exit(hdf5r::h5close(file), add = TRUE, after = FALSE)
  }
  group <- file[[tablename]]
  on.exit(hdf5r::h5close(group), add = TRUE, after = FALSE)
  hdf5r::list.datasets(group, full.names = FALSE, recursive = FALSE)
}

