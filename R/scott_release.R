#' Release to cran steps
#' @name release
#' @examples
#' # scott_release()

#' @export
scott_release <- function() {
  scott_release_submit()
  scott_release_cleanup()
  scott_release_git_tag()
  scott_release_publish_release()
  cli::cat_line(crayon::blue("adding a Todoist to do item"))
  todoist_add_item()
  Sys.sleep(1) # sleep for 1 sec to wait a bit for email to come in
  scott_release_fetch_cran_email()
}

#' @export
#' @rdname release
scott_release_submit <- function() {
  cli::cat_line(crayon::blue("submitting to CRAN"))
  submit2cran()
}

#' @export
#' @rdname release
scott_release_cleanup <- function() {
  cli::cat_line(crayon::blue("deleting CRAN-RELEASE file & undoing .Rbuildignore changes"))
  unlink("CRAN-RELEASE")
  sys::exec_wait("git", c("checkout", ".Rbuildignore"))
}

#' @export
#' @rdname release
scott_release_git_tag <- function() {
  cli::cat_line(crayon::blue("git tagging"))
  last_tag = paste0("v", desc::desc_get_version())
  gert::git_tag_create(last_tag, "new tag")
  gert::git_push("origin", file.path("refs/tags", last_tag))
}

#' @export
#' @rdname release
scott_release_publish_release <- function() {
  cli::cat_line(crayon::blue("creating new github release"))
  gh_release()
}

#' @export
#' @rdname release
scott_release_fetch_cran_email <- function() {
  cli::cat_line(crayon::blue("fetching cran submission link from gmail"))
  fetch_cran_email()
}
