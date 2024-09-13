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
