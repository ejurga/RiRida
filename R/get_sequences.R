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
#' @export
get_all_sequence_info <- function(samples, n_con = 10){

  # Get all sequences
  resps <- req_sequences_parallel(samples = samples, type = "all", n_con)
  all_seqs <- format_sequence_resp_to_df(resps)
  # Get pairs of sequences
  resps <- req_sequences_parallel(samples = samples, type = "pairs", n_con)
  all_pairs <- format_pair_resp_to_df(resps)
  # Filter out sequences found in the pairs call
  df <- dplyr::bind_rows(all_pairs, filter(all_seqs, !id %in% all_pairs$id))
  return(df)

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

#' Format output from retrieval of sequences
#'
#' @param resps A list of responses from an IRIDA API call
#'
#' @importFrom tibble tibble
#' @export
format_sequence_resp_to_df <- function(resps){

  l <- list()
  for (i in seq(length(resps))){
    resp <- resps[[i]]
    x <- httr2::resp_body_json(resp)
    seqs <- x$resource$resources
    l[[i]] <-
      tibble(sampleID = get_sample_id_from_api_call(resp$url),
                   id = sapply(seqs, function(seq) seq$identifier),
             fileLink = sapply(seqs, function(seq) seq$file),
             fileName = sapply(seqs, function(seq) seq$fileName),
               Sha256 = sapply(seqs, function(seq) seq$uploadSha256))
  }

  df <- dplyr::bind_rows(l)
  return(df)
}

#' Format response from retrieval of paired end sequences
#'
#' @param resps A list of responses from an IRIDA API call
#'
#' @importFrom tibble tibble
#' @export
format_pair_resp_to_df <- function(resps){

  l <- list()
  for (i in seq(length(resps))){
    resp <- resps[[i]]

    x <- httr2::resp_body_json(resp)
    pairs <- x$resource$resources

    l[[i]] <-
      tibble(
                sampleID = get_sample_id_from_api_call(resp$url),
                  pairId = sapply(pairs, function(pair) pair$identifier),
              forward_id = sapply(pairs, function(pair) pair$forwardSequenceFile$identifier),
        forward_fileLink = sapply(pairs, function(pair) pair$forwardSequenceFile$file),
        forward_fileName = sapply(pairs, function(pair) pair$forwardSequenceFile$fileName),
          forward_Sha256 = sapply(pairs, function(pair) pair$forwardSequenceFile$uploadSha256),
              reverse_id = sapply(pairs, function(pair) pair$reverseSequenceFile$identifier),
        reverse_fileLink = sapply(pairs, function(pair) pair$reverseSequenceFile$file),
        reverse_fileName = sapply(pairs, function(pair) pair$reverseSequenceFile$fileName),
          reverse_Sha256 = sapply(pairs, function(pair) pair$reverseSequenceFile$uploadSha256)
      ) |>
        pivot_longer(cols = matches(c("forward", "reverse")),
                     names_to = c("direction", ".value"),
                     names_sep = "_")
  }
  df <- dplyr::bind_rows(l)

  return(df)
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

  strings <- lapply(resps, resp_body_string)

  df <-
    lapply(strings, jsonlite::fromJSON) |>
    lapply(function(x) x$resource$resources) |>
    dplyr::bind_rows(.id = "id") |>
    as_tibble()
  return(df)

}
