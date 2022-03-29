#' Gets version tag information from text
#' This function extracts the sections %%%
#' Helper for \code{version()}
#'
#' @param source_text input text
#'
#' @return Versioned sections

get_version_text <- function(source_text) {

  secs <- source_text %>% stringr::str_which("%%%")

  if (length(secs) == 0){
    return(NULL)
  }

  sec_info <- data.frame(
    starts = secs[c(TRUE, FALSE)],
    ends = secs[c(FALSE, TRUE)]
  )

  sec_info$is_versioned = stringr::str_detect(source_text[sec_info$starts + 1], "version")

  version_opts <- source_text[sec_info$starts + 1] %>%
    stringr::str_extract("(?<=version:).*") %>%
    stringr::str_split(",") %>%
    purrr::map(stringr::str_trim)

  all_versions <- version_opts %>% unlist() %>% unique()

  print(all_versions)


  for (v in all_versions) {

    sec_info[!sec_info$is_versioned, v] <- TRUE
    sec_info[sec_info$is_versioned, v] <- purrr::map_lgl(version_opts,
                                                         ~any(.x == v))

  }

  return(sec_info)

}
