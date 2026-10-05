


# An H:MM:SS-shaped GTFS time (hours may exceed 24; MM and SS are not range-checked,
# as before). Shared by stop_times_to_seconds() and filter_valid_stop_times().
valid_gtfs_time <- "^[0-9]+:[0-9]{2}:[0-9]{2}$"

#' Convert stop_times time strings to seconds after midnight
#'
#' Replaces the \code{arrival_time} and \code{departure_time} columns of
#' \code{gtfs$stop_times} with integer seconds after midnight, using
#' \code{gtfstools::convert_time_to_seconds()}. Strings that are not
#' well-formed H:MM:SS times (after trimming whitespace) become \code{NA}.
#' The conversion is done in place, so callers must copy first. Both columns
#' must exist: gtfstools creates \code{arrival_time_secs} only when
#' \code{departure_time} is present.
#'
#' @param gtfs A GTFS object (a list of data.tables). Plain lists are promoted
#'   with \code{gtfstools::as_dt_gtfs()}, which copies every table.
#'
#' @return The GTFS object, with integer time columns in \code{stop_times}.
#'
#' @noRd
stop_times_to_seconds <- function(gtfs) {

  gtfs <- gtfstools::convert_time_to_seconds(
    gtfstools::as_dt_gtfs(gtfs),
    file = "stop_times",
    by_reference = TRUE
  )

  # the RHS is evaluated before assignment, so the original strings are still
  # available to mask malformed values (gtfstools parses those as 0)
  gtfs$stop_times[, `:=`(
    departure_time = fifelse(grepl(valid_gtfs_time, trimws(departure_time)),
                             departure_time_secs, NA_integer_)
    , arrival_time = fifelse(grepl(valid_gtfs_time, trimws(arrival_time)),
                             arrival_time_secs, NA_integer_)
    , departure_time_secs = NULL
    , arrival_time_secs = NULL)]

  return(gtfs)

}



#' Convert seconds after midnight to time string
#'
#' Converts seconds after midnight as integers to strings in the "HH:MM:SS"
#' format.
#'
#' @param seconds An integer.
#'
#' @return A time-representing string.
#'
#' @noRd
seconds_to_string <- function(seconds) {
  
  checkmate::assert_integer(seconds)
  
  time_string <- data.table::fifelse(
    is.na(seconds),
    "",
    paste(
      formatC(seconds %/% 3600, width = 2, format = "d", flag = 0),
      formatC((seconds %% 3600) %/% 60, width = 2, format = "d", flag = 0),
      formatC((seconds %% 3600) %% 60, width = 2, format = "d", flag = 0),
      sep = ":"
    )
  )
  
  return(time_string)
  
}
