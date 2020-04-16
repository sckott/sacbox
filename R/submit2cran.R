#' @export
submit2cran <- function(pkg = ".", args = NULL) {
  pkg <- devtools:::as.package(pkg)
  built_path <- devtools:::build_cran(pkg, args = args)
  devtools:::upload_cran(pkg, built_path)
  usethis::with_project(pkg$path, devtools:::flag_release(pkg))
}
