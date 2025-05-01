#' Get sequences associated with a list of IRIDA samples
#'
#' Use this function to get a table of sequences associated with the
#' supplied IRIDA sample IDs. This function does two API calls per
#' sample, the first to get ALL sequences, and the second to get more
#' information on paired sequences. The returned dataframe contains
#' one record per unique sequence.
#'
#' @param samples A list of IRIDA sample IDs
#' @param n_con How many connections to send to IRIDA in parralel?
#'
#' @return dataframe of sequences associated with each IRIDA sample
#'
#' @import dplyr
#' @export
get_all_sequence_info <- function(samples, n_con = 10){

  # Get all sequences
  resps <- req_sequences_parallel(samples = samples, type = "all", n_con)
  all_seqs <- resp_irida_to_dataframe(resps)
  # Get pairs of sequences
  resps <- req_sequences_parallel(samples = samples, type = "pairs", n_con)
  all_pairs <- pair_resps_to_df(resps = resps)
  # Filter out sequences found in the pairs call
  df <- bind_rows(all_pairs, filter(all_seqs, !id %in% all_pairs$id))
  return(df)
}

#' Convert IRIDA timestamps into a datetime
#'
irida_timecode_to_datetime <- function(x){
  res <- lubridate::as_datetime(x/1000)
  return(res)
}

#' Request the sequence information for a list of samples in parallel
#'
#' Sends requests for sequence information in parallel.
#'
#' @param samples A vector of IRIDA sample IDs to retrieve information for
#' @param type "all" to retrieve entire sequence collection, "pairs" to
#'    retrieve only information on pairs
#' @param n_con Number of connections to send to IRIDA at once.
#'
#' @returns A list of httr2 responses
#'
#' @export
req_sequences_parallel <- function(samples, type = c("all", "pairs"), n_con=10){

  type <- match.arg(type)
  pool <- curl::new_pool(total_con = n_con)
  resps <-
    lapply(req_irida_sequences, X = samples, type = type) |>
    httr2::req_perform_parallel(on_error = "continue",  pool = pool,
                         progress = paste("Retrieving Seqs:", type))
  n_fail <- length(resps_failures(resps))
  if ( n_fail>0 ){  warning("Failures: ", n_fail)
  } else { message("All successful") }
  names(resps) <- samples
  return(resps)
}

#' Get sample ID from the URL of an API call
get_sample_id_from_api_call <- function(url){
  sub(x = url, ".+samples/([0-9]+)/.+", "\\1")
}

#' Retrieve sequences related to an IRIDA sample ID
#'
#' @param sample_id IRIDA sample ID
#' @param type If all, get all sequences, if pairs, get only paired end reads
#'
#' @importFrom httr2 req_url_path_append
#' @export
req_irida_sequences <- function(sample_id, type = c('all', 'pairs')){
  type = match.arg(type)
  if (type == "all") type <- "sequenceFiles"
  req <-
    req_irida() %>%
    req_url_path_append("samples") %>%
    req_url_path_append(sample_id) %>%
    req_url_path_append(type)
  return(req)
}

#' Attempts to format IRIDA api responses to a single dataframe
#'
#' @param resps A list of responses, ideally named
#'
#' @importFrom tibble tibble
#' @export
resp_irida_to_dataframe <- function(resps){

  strings <- lapply(resps, httr2::resp_body_string)

  df <-
    lapply(strings, jsonlite::fromJSON) |>
    lapply(function(x) x$resource$resources) |>
    dplyr::bind_rows(.id = "id")

  if (nrow(df)==0) {
    warning("Attempting to coerce to dataframe, but response appears to be empty")
    return(NULL)
  } else {
    df <-
      df |>
      dplyr::mutate(across(contains("Date"), ~irida_timecode_to_datetime(.x))) |>
      dplyr::select(identifier, everything()) |>
      as_tibble()
    return(df)
  }
}

#' Convert a list of IRIDA API responses for sequence pairs into a dataframe
#'
#' @param resps description
#'
pair_resps_to_df <- function(resps){
  df <-
    lapply(resps, httr2::resp_body_json) |>
    lapply(function(x) x$resource$resources) |>
    lapply(get_forward_and_reverse_files_as_df) |>
    dplyr::bind_rows(.id = "id") |>
    dplyr::mutate(across(contains("Date"), ~irida_timecode_to_datetime(.x)))
  return(df)
}

#' Retrieve the forward and reverse sequencing file information
#'
get_forward_and_reverse_files_as_df <- function(x){
  l <- list()
  for (i in seq(length(x))){
    f <- x[[i]]$forwardSequenceFile
    f$direction = "forward"
    r <- x[[i]]$reverseSequenceFile
    r$direction = "reverse"
    l[[i]] <- bind_rows(f, r)
    l[[i]]$pair_id = x[[i]]$identifier
  }
  df <- dplyr::bind_rows(l)
  return(df)
}
