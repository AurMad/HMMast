aggregate_by_cow <- function(.data){

  ## checking the input dataset
  check_mastitis_data(.data)

  ## aggregating data by cow
  by_cow <- .data |>
    dplyr::arrange(cow_id, rec_date) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::filter(test_res < 2) |>
    dplyr::group_by(cow_id) |>
    dplyr::summarise(
      row_first = min(id),
      row_last = max(id)
    )

  return(by_cow)

    }


aggregate_by_lac <- function(.data){

  ## checking the input dataset
  check_mastitis_data(.data)

  ## aggregating data by cow
  by_lac <- .data |>
    dplyr::arrange(cow_id, rec_date) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::filter(test_res < 2) |>
    dplyr::group_by(cow_id, calv_date) |>
    dplyr::summarise(
      row_first = min(id),
      row_last = max(id)
    )


  return(by_lac)

}





