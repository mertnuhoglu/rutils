#' syncs/copies Rmd files to blog directories
#' For example: I keep my blog files as hugo+bookdown files inside some directory.
#' But I keep my Rmd files in some external directories.
#' By using sync_rmd_to_blog_dirs(".") I can copy all Rmd files in current directory to 
#' those blog directories with external dependency files included.
#' Note that, external dependency resources, should be explicitly specified in yaml header
#' such as:
#' resource_files:
#' - ./globals.R
#'
#' @param dir source directory for Rmd files
#' @examples
#' sync_rmd_to_blog_dirs(".")
#' @export
sync_rmd_to_blog_dirs = function(dir = ".") {
  `%>%` <- magrittr::`%>%`
  files = list.files(dir) %>%
    rutils::grepv("\\.Rmd$")
  for (f in files) {
    yml = rmarkdown::yaml_front_matter(f)
    blog = yml$blog
    if (!rutils::is.blank(blog)) {
      deps = rmarkdown::find_external_resources("~/projects/study/r/ex_r_rmarkdown.Rmd")$path
      if (blog == "mertnuhoglu.com") {
        cmd = sprintf("rsync -va ./%s /Users/mertnuhoglu/projects/jekyll/mertnuhoglu.com/content/tech", c(f, deps))
        lapply(cmd, system, intern = T)
      }
    }
  }
}

