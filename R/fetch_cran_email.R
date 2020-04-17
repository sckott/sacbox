#' fetch the package submission email from cran in your gmail inbox
#' @export
fetch_cran_email <- function(pkg = ".") {
  pkg <- devtools:::as.package(pkg)
  gmailr::gm_auth_configure(Sys.getenv("GMAIL_ID"),
    Sys.getenv("GMAIL_SECRET"))
  gmailr::gm_auth(scopes = 'readonly', path = ".secret")
  z <- gmailr::gm_threads(
    paste("CRAN submission", pkg$package, pkg$version,
      paste0("after:", Sys.Date()-1)))
  ids <- vapply(z[[1]]$threads, "[[", "", "id")
  fetch_url(ids)
}

fetch_url <- function(id) {
  if (length(id) > 1) {
    base::message("more than one gmail thread found, go to email yourself")
  } else {
    mssg <- gmailr::gm_message(id)
    subject <- gmailr::gm_subject(mssg)
    body <- gmailr::gm_body(mssg)
    pat <- 
    'https?://xmpalantir\\.wu\\.ac\\.at\\/cransubmit\\/conf_mail\\.php\\?code=[0-9A-Za-z]+'
    submit_url <- strextract(body, pat)
    base::message("submission link: ", submit_url)
  }
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
