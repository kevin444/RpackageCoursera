# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Name: Kévin Allan Sales Rodrigues
#'
#' @param filename The filename of the .csv file you want to import
#'
#' @return This function returns an R tibble containing the data from the .csv
#'   file that was read in. This function will return Error in
#'   file.exists(filename) : invalid 'file' argument if no file exists at the
#'   filepath specified as the filename argument.
#'
#' @examples
#' \dontrun{fars_read("coursera/data/accident_2013.csv.bz2")}
#' \dontrun{fars_read("data.csv")}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' Make Filename From Year
#'
#' This function takes a year as an argument and uses that to
#' construct a valid filepath to the accident dataset for that year.
#'
#' @param year The year you want to pull data from.
#'
#' @return This function returns a valid filepath within
#' your R working directory.
#'
#' @examples
#' \dontrun{
#' make_filename(2015)
#' make_filename(2014)
#' make_filename(10)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import data for multiple years
#'
#' @param years The list or vector of four-digit years for which you want to
#'   import information.
#'
#' @return This function returns a list in which each element corresponds to
#'   an element in the argument "years." For each valid year in "years," the
#'   corresponding element of the list will be a tibble with rows equal to the
#'   number of observations and two columns: MONTH and year.
#'
#'   If an invalid year
#'   is provided as an argument, the corresponding element of the returned list
#'   will be NULL. A warning message will then print: \code{Warning message: In
#'   value[[3L]](cond) : invalid year}
#'
#' @examples
#' \dontrun{
#' fars_read_years(2015)
#' fars_read_years(c(2013, 2014))
#' fars_read_years(c(2013, "invalid input"))
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize Monthly Accident Totals
#'
#' This is a function that takes a set of years and, for each
#' year, returns a tibble showing the total number of accidents
#' recorded in each month of that year.
#'
#' For this function to work properly, a year must be provided that
#' corresponds to a year of data in a properly-named spreadsheet in the
#' working directory. If the year is not valid or the spreadsheet is not
#' in the directory, the function will return an error.
#'
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a tibble with 12 rows - one corresponding to
#' each month of the year - and one column, titled by the four-digit year,
#' for each valid year provided in the argument plus an additional column
#' MONTH listing the months in the data from 1-12.
#'
#' Each column named with a year shows the total month-by-month accident
#' reports for each month of that year.
#'
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' fars_summarize_years(2014)
#' fars_summarize_years(2010)
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}

#' State Accident Mapping
#'
#' This function takes years and a state number and plots
#' the accident locations within that state.
#'
#' @param state.num A state number from the alphabetical list of states.
#' @param year The year from which you want to draw data.
#'
#' @return This function, even when successfully run, returns NULL invisibly.
#' However, it generates a plot in the course of running.
#'
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2015)
#' fars_map_state(4,2013)
#' }
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
