#' Gets version tag information from chunks
#' Helper for \code{version()}
#' @param source_text input text
#' @importFrom stringr str_which str_subset str_detect str_extract str_extract_all
#' str_split str_trim
get_version_chunks <- function(source_text) {

  starts <- source_text %>% str_which("```\\{")
  all_ends <- source_text %>% str_which("```$")

  ends <- purrr::map_int(starts, ~min(all_ends[all_ends > .x]))


  chunk_info <- data.frame(
    starts = starts,
    ends = ends,
    is_versioned = source_text %>%
      str_subset("```\\{") %>%
      str_detect("version\\s*=")

  )

  version_opts <- source_text %>%
    str_subset("```\\{") %>%
    str_subset("version\\s*=")

  version_opts_sing <- version_opts %>%
    str_extract_all('version\\s*=\\s*\\"[^\\"]*\\"') %>%
    unlist() %>%
    str_extract_all('\\"[^\\",]+\\"') %>%
    unlist()

  version_opts_mult <- version_opts %>%
    str_extract_all("version\\s*=\\s*c\\([^\\)]*\\)") %>%
    unlist() %>%
    str_extract_all('\\"[^\\",]+\\"') %>%
    unlist()

  all_versions <- unique(c(version_opts_sing, version_opts_mult))


  # version_opts_where <- version_opts %>%
  #   str_extract_all(",?\\s*[:alpha:]+\\s*=\\s*") %>%
  #   purrr::map(~str_which(.x, "version"))


  for (v in all_versions) {

    col_name <- str_remove_all(v, fixed('"'))

    chunk_info[!chunk_info$is_versioned, col_name] <- TRUE
    chunk_info[chunk_info$is_versioned, col_name] <-
      purrr::map_lgl(version_opts,  ~any(str_detect(.x, fixed(v))))
  }

  return(chunk_info)

}
