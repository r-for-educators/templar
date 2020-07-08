#' Knit different versions of a file from chunk and section options
#'
#' \code{versions} is a function that should be included in the setup chunk of
#' an R Markdown document.  Its purpose is to write, then knit, R Markdown source
#' files for several versions of a document, such as different exams in a course.
#'
#'
#' @param global_eval Logical.
#' @param to_knit Character vector specifying which versions to write and knit
#' into separate files.  If not specified, all versions are produced.
#' @param folders List of versions and subfolder to put them in. Use pattern
#' \code{version_name = folder_name}.  Default is each version in its own folder.
#'
#' @returns none
#'
#' @details
#'
#' Code chunks may be tagged as version-specific using the option \code{version}.
#' Text sections may also be tagged as version-specific using \code{%%%} wrappers.
#' See example Rmd source below.
#'
#' The version label \code{"solution"} is special,
#' as it is combined with the other version labels to make a solution set.
#'
#' The version label \code{"none"} is special; it will be ignored in the creation
#' of the child documents.  Use it to leave yourself notes in the original document.
#'
#' Example R Markdown source:
#'
#' \preformatted{
#' ---
#' title: "Example"
#' output: html_document
#' ---
#'
#' ```{r, include=FALSE}
#' knitr::opts_chunk$set(echo = TRUE)
#' templar::versions()
#' ```
#'
#' \%\%\%
#' version: A
#'
#' You are taking **Exam A**
#' \%\%\%
#'
#' \%\%\%
#' version: B
#'
#' You are taking **Exam B**
#' \%\%\%
#'
#' ## Question 1: Means
#'
#' Find the mean of the vector `a`
#'
#' ```{r, version = "A"}
#' set.seed(123)
#' ```
#'
#' ```{r, version = "B"}
#' set.seed(456)
#' ```
#'
#' ```{r, version = "A"}
#' a <- rnorm(10)
#' ```
#'
#' \%\%\%
#' version: A
#'
#' The mean is `r mean(a)`
#' \%\%\%
#'
#' \%\%\%
#' version: none
#'
#' Note to self: make a version C later.
#' \%\%\%
#'
#' }
#' @export
versions <- function(global_eval = TRUE,
                     to_knit = NULL,
                     folders = NULL) {

  if (!isTRUE(getOption('knitr.in.progress'))){
    return()
  }

  orig_file <- knitr::current_input(dir = TRUE)
  orig_dir <- orig_file %>% stringr::str_remove("[^\\/]*\\.Rmd")
  orig_name <- orig_file %>% stringr::str_extract("[^\\/]*\\.Rmd")

  orig_text <- readLines(orig_file)

  orig_text <- prep_orig_text(orig_text, global_eval)

  orig_opts <- knitr::opts_chunk$get()

  if (global_eval){
    knitr::opts_chunk$set(eval = TRUE)
  }

  # Pull out chunk label info pertaining to versions

  chunk_info <- get_version_chunks(orig_text)

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

  purrr::map(to_knit, write_version, orig_name, orig_dir, orig_text, sec_info, all_info, folders)

  knitr::opts_chunk$set(orig_opts)

}
