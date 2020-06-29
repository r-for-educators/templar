#' Gets version tag information from text
#' Helper for \code{version()}
#' @param source_text input text
#' @importFrom stringr str_which str_detect str_extract str_split str_trim
get_version_text <- function(source_text) {

  secs <- source_text %>% str_which("%%%")

  if (length(secs) == 0) return(NULL)

  sec_info <- data.frame(
    starts = secs[c(TRUE, FALSE)],
    ends = secs[c(FALSE, TRUE)]
  )

  sec_info$is_versioned = str_detect(source_text[sec_info$starts + 1], "version")

  version_opts <- source_text[sec_info$starts + 1] %>%
    str_extract("(?<=version:).*") %>%
    str_split(",") %>%
    purrr::map(str_trim)

  all_versions <- version_opts %>% unlist() %>% unique()


  for (v in all_versions) {

    sec_info[!sec_info$is_versioned, v] <- TRUE
    sec_info[sec_info$is_versioned, v] <- purrr::map_lgl(version_opts,
                                                         ~any(.x == v))

  }

  return(sec_info)

}
