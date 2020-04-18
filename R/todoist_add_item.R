todoist_add_item <- function(pkg = ".") {
  pkg <- devtools:::as.package(pkg)
  key <- Sys.getenv("TODOIST_KEY")
  if (!nzchar(key)) {
    stop("env variable 'TODOIST_KEY' not found")
  }
  con <- crul::HttpClient$new("https://api.todoist.com",
    headers = list(
      Authorization = paste0('Bearer ', key),
      'Content-Type' = "application/json"
    )
  )
  body <- list(
    content = 
      sprintf("tweet: new ver %s on cran, release notes %s",
        pkg$package, sr$release_url),
    due_date = as.character(Sys.Date() + 1),
    priority = 4
  )
  res <- con$post("rest/v1/tasks", body = body, encode = "json")
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"))
}
