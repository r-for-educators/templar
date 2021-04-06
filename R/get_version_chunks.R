#' Gets version tag information from chunks
#' This function extracts the chunks ```
#' Helper for \code{version()}
#'
#' @param source_text input text
#' @param engine_to_version Should un-versioned chunks inherit their engine as
#' a version label (for multilingual versions)
#'
#' @return Version tags from chunks


get_version_chunks <- function(source_text,
                               engine_to_version = FALSE) {

  starts <- source_text %>% stringr::str_which("```\\{")
  all_ends <- source_text %>% stringr::str_which("```$")

  ends <- purrr::map_int(starts, ~min(all_ends[all_ends > .x]))


  chunk_info <- data.frame(
    starts = starts,
    ends = ends,
    is_versioned = source_text %>%
      stringr::str_subset("```\\{") %>%
      stringr::str_detect("version\\s*=")

  )

  version_opts <- source_text %>%
    stringr::str_subset("```\\{") %>%
    stringr::str_subset("version\\s*=")

  version_opts_sing <- version_opts %>%
    stringr::str_extract_all('version\\s*=\\s*\\"[^\\"]*\\"') %>%
    unlist() %>%
    stringr::str_extract_all('\\"[^\\",]+\\"') %>%
    unlist()

  version_opts_mult <- version_opts %>%
    stringr::str_extract_all("version\\s*=\\s*c\\([^\\)]*\\)") %>%
    unlist() %>%
    stringr::str_extract_all('\\"[^\\",]+\\"') %>%
    unlist()

  if (engine_to_version) {

    chunk_engines <- source_text %>%
      stringr::str_subset("```\\{") %>%
      stringr::str_extract_all("(?<=\\{)[A-z]+") %>%
      unlist() %>%
      paste0('"', . , '"')

    all_versions <- unique(c(chunk_engines, version_opts_sing, version_opts_mult))

  } else {

    all_versions <- unique(c(version_opts_sing, version_opts_mult))

  }


  for (v in all_versions) {

    col_name <- stringr::str_remove_all(v, stringr::fixed('"'))

    chunk_info[!chunk_info$is_versioned, col_name] <- TRUE
    chunk_info[chunk_info$is_versioned, col_name] <-
      purrr::map_lgl(version_opts,  ~any(stringr::str_detect(.x, stringr::fixed(v))))


    # Drop all non-matching engines, unless pre-versioned
    if (engine_to_version && v %in% chunk_engines) {

      chunk_info[!chunk_info$is_versioned & chunk_engines != v, col_name] <- FALSE

    }

  }

  return(chunk_info)

}
