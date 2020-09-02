#' @export
news_latest <- function(pkg = ".") {
  get_news(usethis:::read_utf8(usethis::proj_path("NEWS.md")))
}

# modified from usethis:::news_latest
get_news <- function(lines) {
  # latest version
  versions <- which(grepl("=+", lines))
  # news lines for latest version
  if (length(versions) == 1) {
    news <- lines[rlang::seq2(versions[1] + 2, length(lines))]
  } else {
    news <- lines[rlang::seq2(versions[1] + 2, versions[2] - 2)]
  }
  # Remove leading and trailing empty lines
  text <- which(news != "")
  if (length(text) == 0) return("")
  paste0(news, "\n", collapse = "")
}

