require(httr2)
require(tidyverse)

#' Base irida API link with authentication
#'
req_irida <- function(){
  request(irida_api_link()) %>%
  irida_oauth()
}

#' Base link for IRIDA API
#'
irida_api_link <- function(){
  "http://ngs-archive.corefacility.ca/irida/api"
}

#' Send authentication token using password method
#'
irida_oauth <- function(req){
  req_oauth_password(req, client = irida_client(), username = "ejurga")
}

#' Access irida client
#'
irida_client <- function(){
  oauth_client(
    id = "api-ejurga",
    secret = "QKYj7NISXV9KuAbDk4cnzXAiOfzn62AOJGWp8riMUV",
    token_url = paste0(irida_api_link(), "/oauth/token"),
    name = "irida-api")
}

#' Retrieve sequences related to an IRIDA sample ID
#'
#' @param sample_id IRIDA sample ID
#' @param type If all, get all sequences, if pairs, get only paired end reads
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

#' Format json from retrieval of paired end sequences
format_pairs_json_to_df <- function(jsons){

  l <- list()
  for (i in seq(length(jsons))){
    json <- jsons[[i]]

    x <- resp_body_json(json)
    pairs <- x$resource$resources

    l[[i]] <-
      tibble(
                sampleID = sample_id,
                  pairId = sapply(pairs, function(pair) pair$identifier),
              forward_id = sapply(pairs, function(pair) pair$forwardSequenceFile$identifier),
        forward_fileLink = sapply(pairs, function(pair) pair$forwardSequenceFile$file),
        forward_fileName = sapply(pairs, function(pair) pair$forwardSequenceFile$fileName),
              reverse_id = sapply(pairs, function(pair) pair$reverseSequenceFile$identifier),
        reverse_fileLink = sapply(pairs, function(pair) pair$reverseSequenceFile$file),
        reverse_fileName = sapply(pairs, function(pair) pair$reverseSequenceFile$fileName)
      ) %>%
        pivot_longer(cols = matches(c("forward", "reverse")),
                     names_to = c("direction", ".value"),
                     names_sep = "_")
  }
  df <- bind_rows(l)

  return(df)
}

#' Format output from retrieval of sequences
format_sequences_json_to_df <- function(json){

  l <- list()
  for (i in seq(length(jsons))){
    json <- jsons[[i]]
    x <- resp_body_json(json)
    seqs <- x$resource$resources

    l[[i]] <-
      tibble(sampleID = sample_id,
                   id = sapply(seqs, function(seq) seq$identifier),
             fileLink = sapply(seqs, function(seq) seq$file),
             fileName = sapply(seqs, function(seq) seq$fileName))
  }

  df <- bind_rows(l)
  return(df)
}

#' Get dataframe of all sequences associated with an irida sample
#'
#' @param sample_id IRIDA sample identifier
#'
#' @return A dataframe, in which each row is a sequence file associated with
#'         the sample.
#'
get_sequences <- function(sample_id){
  pairs <-
    req_irida_sequences(sample_id, "pairs") %>%
    req_perform() %>%
    resp_body_json() %>%
    format_pairs_json_to_df()

  all_seqs <-
    req_irida_sequences(sample_id, "all") %>%
    req_perform() %>%
    resp_body_json() %>%
    format_sequences_json_to_df()

  seqs_df <- bind_rows(pairs, filter(all_seqs, !id %in% pairs$id))
  return(seqs_df)
}

#' Get sample information from all samples in an irida project
#'
#' @param project_id The IRIDA project ID
#'
#' @return A dataframe with all the samples in a project, along with some
#'         of the metadata fields I expect to always be filled.
project_samples <- function(project_id){

  samples <-
    req_irida() %>%
    req_url_path_append("projects") %>%
    req_url_path_append(project_id) %>%
    req_url_path_append("samples") %>%
    req_perform() %>%
    resp_body_json()

  x <- samples$resource$resources

  df <-
    tibble(
              id = sapply(x, function(x) x$identifier),
     sample_name = sapply(x, function(x) x$sampleName),
     createdDate = as_datetime(sapply(x, function(x) x$createdDate/1000)),
    modifiedDate = as_datetime(sapply(x, function(x) x$modifiedDate/1000)),
     description = sapply(x, function(x) x$description)
    )

  return(df)
}

ecoli_seqs <-
  lapply(X = ecoli_samples$id[1:100], FUN = get_sequences) %>%
  bind_rows()

req_all_pairs <- lapply(req_irida_sequences, X = ecoli_seqs$sampleID[1:20], type = "pairs")

resps <-
  req_all_pairs %>%
  req_perform_parallel(on_error = "continue")

format_pairs_json_to_df(jsons = resps)

bind_rows(lapply(resps, format_pairs_json_to_df))





