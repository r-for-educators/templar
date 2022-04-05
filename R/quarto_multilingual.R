#' Knit different versions of a file from chunk and section options
#'
#' \code{versions_quarto_multilingual} is a function that should be included in the setup chunk of
#' a Quarto document.  Its purpose is to write multiple Quarto source
#' files for several versions of a document, specifically when multiple languages
#' are being used for different versions. Python versions must contain the word "python".
#'
#' @param to_knit Character vector specifying which versions to write and knit
#' into separate files.  If not specified, all versions are produced.
#' @param folders List of versions and subfolder to put them in. Use pattern
#' \code{version_name = folder_name}.  Default is each version in its own folder.
#' @param global_eval Reset global eval to TRUE?  (parent file sets it to FALSE)
#' @param to_jupyter Should a .ipynb document be generated from the python version?
#' @param warn_edit Should a "don't edit this" message be added to the auto-created files?
#'
#' @returns none
#'
#' @export
versions_quarto_multilingual <- function(to_knit = NULL,
                                folders = NULL,
                                global_eval = TRUE,
                                to_jupyter = FALSE,
                                warn_edit = TRUE) {

  if (!isTRUE(getOption('knitr.in.progress'))){
    return()
  }


  orig_file <- knitr::current_input(dir = TRUE)
  orig_dir <- orig_file %>% stringr::str_remove("[^\\/]*\\.qmd")
  orig_name <- orig_file %>% stringr::str_extract("[^\\/]*\\.qmd")

  orig_text <- readLines(orig_file)

  orig_text <- prep_orig_text(orig_text,
                              eval_orig = FALSE,
                              warn_edit)

  orig_opts <- knitr::opts_chunk$get()

  # Pull out chunk label info pertaining to versions

  chunk_info <- get_version_chunks(orig_text,
                                   engine_to_version = TRUE)


  # Pull out text sections pertaining to versions

  sec_info <- get_version_text(orig_text)

  # This condition is to support when people don't submit %%% sections

  if (!is.null(sec_info)){
    all_info <- dplyr::full_join(chunk_info, sec_info) %>%
      dplyr::mutate_all(~tidyr::replace_na(.,FALSE))
  } else {
    all_info <- chunk_info %>%
      dplyr::mutate_all(~tidyr::replace_na(.,FALSE))
  }

  not_versions <- c("starts", "ends", "is_versioned", "none")


  # In case we only want to knit a few of the versions

  if (!is.null(to_knit)) {

    all_info <- all_info[, c(not_versions, to_knit)]

  } else {

    to_knit <- setdiff(names(all_info), not_versions)

  }

  # Handles the absence of solution code chunks

  if (is.null(all_info[["solution"]])) {
    all_info[["solution"]] <- FALSE
  }

  to_knit <- stringr::str_subset(to_knit, "solution", negate = TRUE)

  all_info <- purrr::map_df(to_knit, get_solution_chunks, all_info)

  all_info <- all_info %>%
    dplyr::select(-"solution")

  to_knit <- setdiff(names(all_info), not_versions)

  # Write and knit file for each version

  purrr::map(to_knit, templar:::write_version_quarto, orig_name, orig_dir, orig_text, sec_info, all_info, folders, global_eval, to_jupyter)

  ## Deliberate error, so we don't render the original doc
  stop("Quitting knitting process for parent document.")

}

