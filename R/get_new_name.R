#' Function to get new file names
#'
#' @param v Version list
#' @param orig_name Original file name
#' @param orig_dir Original file directory
#' @param folders Folders to store the new generated files
#'
#' @return New file name

get_new_name <- function(v, orig_name, orig_dir, folders){
  new_name <- paste0(stringr::str_remove(orig_name, ".Rmd"),
                     glue::glue("_{v}.Rmd"))

  if (is.null(folders[[v]])) folders[[v]] <- v

  fol_name <- paste0(orig_dir, folders[[v]], "/")

  if (!dir.exists(fol_name)) { dir.create(fol_name) }

  new_name <- paste0(fol_name, new_name)

  return(new_name)
}
