#' Function to write the different versions.
#'
#' @param v Version list
#' @param orig_name Original file name
#' @param orig_dir Original file directory
#' @param orig_text Original file text
#' @param sec_info Sections info
#' @param all_info All chunks info
#' @param folders Folders to store the new generated files
#' @param global_eval Should eval option be set back to TRUE?
#' @param to_jupyter Should we generate an ipynb copy?
#'
#' @return Doesn't return anything, but generates the new RMD files.

write_version_quarto <- function(v, orig_name, orig_dir, orig_text, sec_info, all_info, folders, global_eval, to_jupyter = FALSE){
  options(knitr.duplicate.label = 'allow')

  new_name <- get_new_name_quarto(v, orig_name, orig_dir, folders)

  temp <- version_cleaner(v, orig_text, sec_info, all_info, orig_dir)

  if (global_eval) {
    eval_line <- stringr::str_which(temp, ".*eval:\\s*false")
    temp[eval_line] <- stringr::str_replace(temp[eval_line],
                                            "eval:\\s*false",
                                            "eval: true")
  }

  if (stringr::str_detect(v, "python")) {
    temp = insert_yaml_jupyter(temp)
  }

  writeLines(temp, new_name)

  system(glue::glue("quarto render {new_name}"))

  if (stringr::str_detect(v, "python") & to_jupyter){
      system(glue::glue("quarto convert {new_name}"))
  }
}

#' @export
insert_yaml_jupyter <- function(text) {


  end_yaml <- stringr::str_which(text, "---")[2] - 1

  text <- c(text[1:end_yaml], "jupyter: python3", text[-c(1:end_yaml)])

  return(text)

}
