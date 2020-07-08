#' Function to write the different versions.
#'
#' @param v Version list
#' @param orig_name Original file name
#' @param orig_dir Original file directory
#' @param orig_text Original file text
#' @param sec_info Sections info
#' @param all_info All chunks info
#' @param folders Folders to store the new generated files
#'
#' @return Doesn't return anything, but generates the new RMD files.

write_version <- function(v, orig_name, orig_dir, orig_text, sec_info, all_info, folders){
  options(knitr.duplicate.label = 'allow')

  new_name <- get_new_name(v, orig_name, orig_dir, folders)

  temp <- version_cleaner(v, orig_text, sec_info, all_info, orig_dir)

  writeLines(temp, new_name)

  rmarkdown::render(new_name, envir = new.env())
}
