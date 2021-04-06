#' Function to clean the different versions.
#'
#' @param v Version list
#' @param orig_text Original file text
#' @param sec_info Sections info
#' @param all_info All chunks info
#' @param orig_dir Original file directory
#'
#' @return Versioned cleaned text

version_cleaner <- function(v, orig_text, sec_info, all_info, orig_dir){
  # Remove sections/chunks from other versions

  temp <- orig_text

  delete_me <- !all_info[,v]

  lines_to_delete <- c()

  if (any(delete_me)) {

    lines_to_delete <- c(lines_to_delete,
                         all_info[delete_me, c("starts", "ends")] %>%
                           purrr::pmap( ~.x:.y) %>%
                           unlist())

  }


  # Remove version labels on text sections

  if (!is.null(sec_info) && nrow(sec_info) > 0) {

    lines_to_delete <- c(lines_to_delete,
                         sec_info$starts,
                         sec_info$starts + 1,
                         sec_info$ends)

  }

  # Get rid of duplicate white space

  where_blank <- which(!stringr::str_detect(temp, "\\S"))

  dump_em <- (where_blank + 1) %in% where_blank

  lines_to_delete <- c(lines_to_delete, where_blank[dump_em])

  # Drop everything
  temp <- temp[-unique(lines_to_delete)]

  # Erase chunk labels
  temp <- temp %>%
    stringr::str_remove(",?\\s*version\\s*=\\s*c\\([^\\)]*\\)") %>%
    stringr::str_remove(",?\\s*version\\s*=\\s*[^\\s,\\}]*")

  temp <-
    temp %>%
    stringr::str_replace_all(stringr::fixed("./"), orig_dir)

  return(temp)
}
