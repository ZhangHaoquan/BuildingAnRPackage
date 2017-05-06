#' @title Read csv
#'
#' @description This function makes use of readr's \code{\link[readr]{read_csv}} to read a filename.
#'
#' @param filename A character string giving the text the file will read
#'
#' @return This function returns a tbl_df object if file exist. (Warning: Function may fail if file does not exist)
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export fars_read
#'
#' @note function is \strong{part of an exercise} in the week2 assignment of a \href{https://coursera.org}{coursera} course titled, Building R Packages
#' @note function is for illustrative purposes, \strong{use with care}
#' @note function assumes data is from \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{nhtsa.gov}
#' @note function assumes data can be found in the working directory
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#' \dontrun{fars_read(make_filename(2013))}
#'
#' @seealso \code{\link{make_filename}} for making a valid file name, \code{\link{fars_read_years}},
#' for extension of this functionality
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate Filename
#'
#' This function creates the correct file format to read based on a year parameter
#'
#' @param year valid numeric vector representing year
#'
#' @return This function returns a character vector each formatted to "accident_<\code{year}>_.csv.bz2"
#' @export make_filename
#'
#' @note function is \strong{part of an exercise} in the week2 assignment of a \href{https://coursera.org}{coursera} course titled, Building R Packages
#' @note function is for illustrative purposes, \strong{use with care}
#' @note function assumes data is from \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{nhtsa.gov}
#' @note function assumes data can be found in the working directory
#' @examples \dontrun{make_filename(2013)}
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read Accident
#'
#' This function loops through a vector of years, read them, and output a \code{data.frame}
#' containing month and year info
#'
#' @param years valid numeric vector representing year
#'
#' @return This function returns a \code{list} of \code{tbl_df} each with two columns MONTH and year (Warning: Function may fail if file does not exist)
#' @export fars_read_years
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df mutate select '%>%'
#'
#' @note function is \strong{part of an exercise} in the week2 assignment of a \href{https://coursera.org}{coursera} course titled, Building R Packages
#' @note function is for illustrative purposes, \strong{use with care}
#' @note function assumes data is from \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{nhtsa.gov}
#' @note function assumes data can be found in the working directory
#' @examples \dontrun{fars_read_years(2013)}
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

#' Count Accidents
#'
#' This function loops through a vector of years, read them, and summarize the counts (\code{n}) for each year-month combination
#'
#' @param years valid numeric vector representing year
#'
#' @return This function returns a \code{tbl_df} with the first column being MONTH and subsequent onces being the valid years passed into the function (Warning: Function may fail if file does not exist)
#' @export fars_summarize_years
#'
#' @importFrom dplyr bind_rows summarize summarize
#' @importFrom tidyr spread
#'
#' @note function is \strong{part of an exercise} in the week2 assignment of a \href{https://coursera.org}{coursera} course titled, Building R Packages
#' @note function is for illustrative purposes, \strong{use with care}
#' @note function assumes data is from \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{nhtsa.gov}
#' @note function assumes data can be found in the working directory
#' @examples \dontrun{fars_summarize_years(2013)}
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot Accidents
#'
#' @param state.num single valid number representing a state number in the dataset
#' @param year      single valid number representing year
#'
#' @return This function returns a base graphics plot outlining the shape of the state with dots overlaying to represent accident occurance (Warning: Function may fail if file does not exist)
#' @export fars_map_state
#'
#' @import mapdata
#' @importFrom tidyr spread
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @note function is \strong{part of an exercise} in the week2 assignment of a \href{https://coursera.org}{coursera} course titled, Building R Packages
#' @note function is for illustrative purposes, \strong{use with care}
#' @note function assumes data is from \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{nhtsa.gov}
#' @note function assumes data can be found in the working directory
#' @note maps package requires a database of maps that can be found in mapdata package
#' @examples \dontrun{fars_map_state(1, 2013)}
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
