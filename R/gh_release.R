sr <- new.env()

#' @export
#' @note modified from usethis::use_github_release
gh_release <- function() {
  usethis:::check_uses_github()
  path <- usethis::proj_path("NEWS.md")
  if (!file.exists(path)) {
    usethis::ui_stop("{usethis::ui_path('NEWS.md')} not found")
  }
  # sacbox::news_latest - NOT usethis::news_latest
  news <- news_latest()
  package <- usethis:::package_data()
  release <- gh::gh("POST /repos/:owner/:repo/releases",
    owner = usethis:::github_owner(),
    repo = usethis:::github_repo(),
    tag_name = paste0("v", package$Version),
    target_commitish = usethis:::git_commit_find()$sha,
    name = paste0(package$Package, " v", package$Version),
    body = news,
    draft = FALSE, .api_url = NULL,
    .token = Sys.getenv("GITHUB_PAT_CRAN_SUBMIT"))
  sr$release_url <- release$html_url
  usethis:::view_url(release$html_url)
}
