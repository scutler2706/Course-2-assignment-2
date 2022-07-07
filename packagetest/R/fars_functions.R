#' load a file with filename
#'
#' @param filename a file name of a .csv file
#' @return A data.frame
#' @examples
#' \dontrun{fars_read(User/Document/file.csv)}
#' @note file name must exist in your drive, if not with stop with error.
#' @return dataframe in environment and tibble in console
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' create a file name
#'
#' @param year an integer that represents a year
#' @return A generated file name for a given year
#' @examples
#' \dontrun{make_filename(2021)}
#' @return character string in form of "accident_2021.csv.bz2"
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' reads files based on vector of one or more years
#'
#' @param years a vector of integers that represents years
#' @return list of dataframes, selecting month and year
#' @import dplyr
#' @examples
#' \dontrun{fars_summarize_years(2022)}
#' \dontrun{fars_summarize_years(c(2020, 2021, 2022))}
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(.data$MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' summarizes data by month and year
#'
#' @param years a vector of integers that represents years
#' @return returns a data.frame summarizing n by month and year
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @examples
#' \dontrun{fars_summarize_years(2022)}
#' \dontrun{fars_summarize_years(c(2020, 2021, 2022))}
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(.data$year, .data$MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(.data$year, n)
}

#' make a map from data using state number and year
#'
#' @param state.num an integer that represents state number
#' @param year an integer that represents a year
#' @return A map of accidents for the given state during the given year
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{fars_map_state(4, 2022)}
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, .data$STATE == state.num)
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
