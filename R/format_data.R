check_mastitis_data <- function(.data = NULL){

  ## .data should be data.frame or tibble
  if(!is.data.frame(.data)){

    stop(".data should be a data.frame or a tibble")

  }
  ## looking for specific columns
  colmns <- c("cow_id", "calv_date", "rec_date", "test_res")
  col_id <- match(colmns, colnames(.data))

  if(length(which(is.na(col_id))) > 0){


  stop("Missing column(s)")

  }

  }


#' Format the data for analysis
#'
#' @param .data
#' @param level Level considered for the analysis. Either 'cow' or 'lactation'
#' @param nrec_min Minimum number of records for a unit (cow or lactation) to be included
#'
#' @return
#' @export
#'
#' @examples
format_mastitis_data <- function(.data, level = "lactation", nrec_min = 3){

  check_mastitis_data(.data)

  .data <- expand_scc_data(.data)

  ## dataset with numbered lactations and number of records per lactation
  if(level == "lactation"){

  lac <- make_lactation(.data)

  n_removed <- nrow(lac[lac$nrec < nrec_min, ])

  if(length(n_removed) > 0){

    lac <- lac |>
      dplyr::filter(nrec >= nrec_min)

    ## renumbering lactation ids so that they go from 1 to number of lactations
    lac$lac_id <- match(lac$lac_id, unique(lac$lac_id))

    message(paste0(n_removed, " lactation(s) removed because of less than ", nrec_min, " records"))

  }

  z <- dplyr::left_join(lac, .data) |>
       dplyr::select(-nrec)


  } else {

    cow <- .data |>
      dplyr::group_by(cow_id) |>
      dplyr::summarise(nrec = dplyr::n()) |>
      dplyr::ungroup()

    n_removed <- nrow(cow[cow$nrec < nrec_min, ])

    if(length(n_removed) > 0){

      cow <- cow |>
        dplyr::filter(nrec >= nrec_min)

      cow <- cow |>
        dplyr::rename(
          old_cow_id = cow_id
        ) |>
        dplyr::mutate(cow_id = match(old_cow_id, unique(old_cow_id)))

      message(paste0(n_removed, " cow(s) removed because of less than ", nrec_min, " records"))

      z <- dplyr::left_join(cow, .data) |>
           dplyr::select(-nrec)

    } else {

      z <- cow

      }
  }

  z$test_res[is.na(z$test_res)] <- 2

 return(z)

}

#' Create a lactation dataset from milk recording data
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
make_lactation <- function(.data){

  check_mastitis_data(.data)

  z <- .data |>
    dplyr::group_by(cow_id, calv_date) |>
    dplyr::summarise(
      lac_id = dplyr::cur_group_id(),
      nrec = dplyr::n()) |>
    dplyr::ungroup()

  return(z)

}



add_month <- function(.data, start_date = NULL, end_date = NULL){

  check_mastitis_data(.data)

  ## checking start date
  ## if not provided, first date in the dataset is used
  if(is.null(start_date)){

    start_date <- format(min(as.Date(.data$rec_date)), "%Y-%m-01")

  } else {

    start_date <- format(as.Date, "%Y-%m-01")

     }
  ## checking end date
  ## if not provided, last date in the dataset is used
  if(is.null(end_date)){

    end_date <- format(max(as.Date(.data$rec_date)), "%Y-%m-01")

  } else {

    end_date <- format(as.Date(end_date), "%Y-%m-01")

  }

  ## list of all months
  ls_month <- tibble::tibble(
    .month = as.Date(seq(as.Date(start_date), as.Date(end_date), by = "1 month"))) |>
    dplyr::mutate(month_id = 1:dplyr::n())

  ## add month to the original dataset
  z <- .data |>
    dplyr::mutate(.month = as.Date(format(as.Date(rec_date), "%Y-%m-01")))

  z <- dplyr::inner_join(ls_month, z) |>
    dplyr::select(-.month) |>
    dplyr::arrange(cow_id, rec_date)

  return(z)


}


## this function expands the scc data to have
## 1 row per cow per month until the last month of recording
expand_scc_data <- function(.data){

  ## adding month id to each row
  z <- add_month(.data)

  ## expanding data
  month_min <- min(z$month_id)
  month_max <- max(z$month_id)
  month_sq  <- month_min:month_max

  ## all cows with all existing months
  z_expanded <- expand.grid(
    cow_id = unique(z$cow_id),
    month_id = month_sq
  )

  ## for each cow
  ## first and last month of recording in the data
  by_cow <- z |>
    dplyr::group_by(cow_id) |>
    dplyr::summarise(
            month_first = min(month_id),
            month_last = max(month_id))

  ## merging the 2 datasets
  z_expanded <- dplyr::left_join(z_expanded, z)
  ## merging with the dataset aggregated by cow
  z_expanded <- dplyr::left_join(z_expanded, by_cow) |>
    dplyr::filter(month_id >= month_first, month_id <= month_last) |>
    dplyr::select(-month_first, -month_last) |>
    dplyr::arrange(cow_id, month_id)

  ## adding missing data
  ## missing recording dates
  ls_rec_dates <- z |>
    dplyr::group_by(month_id) |>
    dplyr::summarise(
      rec_date = unique(rec_date)
    )

  ## full list of month between first and last in the dataset
  ls_month <- data.frame(
    month_id = month_sq,
    rec_month = seq(as.Date(min(z$rec_date)), length.out = length(month_sq), by = "1 month")
  )

  ls_month$rec_month <- format(ls_month$rec_month, "%Y-%m-%15")

  ## adding missing months to the dataset
  ls_rec_dates <- dplyr::left_join(ls_month, ls_rec_dates)

  ls_rec_dates$rec_date[is.na(ls_rec_dates$rec_date)] <- ls_rec_dates$rec_month[is.na(ls_rec_dates$rec_date)]


  z_expanded$rec_date[is.na(z_expanded$rec_date)] <- ls_rec_dates$rec_date[z_expanded$month_id[is.na(z_expanded$rec_date)]]

  ## make lactation list
  lac_list <- z |>
    dplyr::group_by(cow_id, calv_date) |>
    dplyr::summarise(parity = unique(parity)) |>
    dplyr::mutate(next_calv = dplyr::lead(calv_date)) |>
    dplyr::ungroup()

  ## list of lactation id corresponding to each recording date
  lac_rec <- tidyr::expand_grid(
              rec_date = unique(ls_rec_dates$rec_date), lac_list) |>
    dplyr::mutate(rec_date = as.Date(rec_date),
                  calv_date = as.Date(calv_date),
                  next_calv = as.Date(next_calv)) |>
    dplyr::filter(rec_date >= calv_date & (rec_date < next_calv | is.na(next_calv))) |>
    dplyr::arrange(cow_id, rec_date) |>
    dplyr::select(-next_calv)

  ## calving date added back when missing
  z_expanded_miss <- z_expanded |>
    dplyr::filter(is.na(parity)) |>
    dplyr::select(-calv_date, -parity)

  z_expanded_miss <- dplyr::left_join(z_expanded_miss, lac_rec) |>
    dplyr::select(colnames(z_expanded))

  z_expanded <- z_expanded |>
    dplyr::filter(!is.na(parity))

  z_expanded <- dplyr::bind_rows(z_expanded, z_expanded_miss) |>
    dplyr::arrange(cow_id, rec_date)

  z_expanded

}

