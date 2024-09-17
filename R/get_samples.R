#' Get sample information from all samples in an irida project
#'
#' @param project_id The IRIDA project ID
#'
#' @return A dataframe with all the samples in a project, along with some
#'         of the metadata fields I expect to always be filled.
#' @importFrom tibble tibble
#' @importFrom lubridate as_datetime
#' @import httr2
#' @export
project_samples <- function(project_id){

  samples <-
    req_irida() |>
    req_url_path_append("projects") |>
    req_url_path_append(project_id) |>
    req_url_path_append("samples") |>
    req_perform()

  df <- resp_irida_to_dataframe(list(samples))

  return(df)
}
