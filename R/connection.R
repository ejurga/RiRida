#' Load options
#' @export
.onLoad <- function(libname, pkgname){
  options(RiRida.config_path = "./config.yml")
}

#' Base irida API link with authentication
#'
req_irida <- function(){
  request(irida_api_link()) %>%
  irida_oauth()
}

#' Base link for IRIDA API
#'
irida_api_link <- function(){
  config::get("irida-api-url", file = getOption("RiRida.config_path"))
}

#' Send authentication token using password method
#'
irida_oauth <- function(req){
  req_oauth_password(req,
                     client = irida_client(),
                     username = config::get("irida-user",
                                            file = getOption("RiRida.config_path")))
}

#' Access irida client
#'
irida_client <- function(){
  oauth_client(
    id = config::get("irida-api-client",
                     file = getOption("RiRida.config_path")),
    secret = config::get("irida-api-secret",
                         file = getOption("RiRida.config_path")),
    token_url = paste0(irida_api_link(), "/oauth/token"),
    name = "irida-api")
}
