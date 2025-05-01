#' Get all the project information that the user has access to.
#'
#' @import httr2
#' @export
get_projects <- function(){

  projects <-
    req_irida() %>%
    req_url_path_append("projects") %>%
    req_perform()

  df <- resp_irida_to_dataframe(list(projects))

  return(df)
}
