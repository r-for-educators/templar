#' Knit different versions of a file from chunk and section options
#'
#' \code{versions} is a function that should be included in the setup chunk of
#' an R Markdown document.  Its purpose is to write, then knit, R Markdown source
#' files for several versions of a document, such as different exams in a course.
#'
#'
#' @param global_eval Logical. Should we replaces the global \code{eval = FALSE} option
#' with \code{TRUE} in the individual versions?
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
#' ```{r}
#' a <- rnorm(10)
#' ```
#'
#' \%\%\%
#' version: solution
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
#' @import stringr
#' @export
versions <- function(global_eval = TRUE,
                     to_knit = NULL, folders = NULL) {

  if (!isTRUE(getOption('knitr.in.progress'))) return()

  orig_file <- knitr::current_input(dir = TRUE)
  orig_dir <- orig_file %>% str_remove("[^\\/]*\\.Rmd")
  orig_name <- orig_file %>% str_extract("[^\\/]*\\.Rmd")

  orig_text <- readLines(orig_file)

  orig_text <- prep_orig_text(orig_text, orig_file, global_eval)

  orig_opts <- knitr::opts_chunk$get()

  if (global_eval) knitr::opts_chunk$set(eval = TRUE)


  # Pull out chunk label info pertaining to versions

  chunk_info <- get_version_chunks(orig_text)
  sec_info <- get_version_text(orig_text)

  all_info <- dplyr::full_join(chunk_info, sec_info) %>%
    dplyr::mutate_all(~tidyr::replace_na(.,FALSE))

  not_versions <- c("starts", "ends", "is_versioned", "none")


  # In case we only want to knit a few of the versions

  if (!is.null(to_knit)) {

    all_info <- all_info[, c(not_versions, to_knit)]

  } else {

    to_knit <- setdiff(names(all_info), not_versions)

  }


  if (is.null(all_info[["solution"]])) {
    all_info[["solution"]] <- FALSE
  }

  to_knit <- str_subset(to_knit, "solution", negate = TRUE)

  for (v in to_knit) {

    sol_name <- glue::glue("solution_{v}")

    if (is.null(all_info[[sol_name]])) {
      all_info[[sol_name]] <- FALSE
    }

    all_info[[sol_name]] <-
      all_info[[v]] |
      all_info[["solution"]] |
      all_info[[sol_name]]

  }

  all_info <- all_info %>%
    dplyr::select(-solution)

  to_knit <- setdiff(names(all_info), not_versions)

  # Write and knit file for each version

  for (v in to_knit) {

    temp = orig_text

    # Remove sections/chunks from other versions

    delete_me <- all_info$is_versioned & !all_info[,v]

    lines_to_delete <- c()

    if (any(delete_me)) {

      lines_to_delete <- c(lines_to_delete,
                           all_info[delete_me, c("starts", "ends")] %>%
                              purrr::pmap( ~.x:.y) %>%
                              unlist())

    }


    # Remove version labels on text sections

    if (nrow(sec_info) > 0) {

      lines_to_delete <- c(lines_to_delete,
                           sec_info$starts,
                           sec_info$starts + 1,
                           sec_info$ends)

    }

    # Get rid of duplicate white space

    where_blank <- which(!str_detect(temp, "\\S"))

    dump_em <- (where_blank + 1) %in% where_blank

    lines_to_delete <- c(lines_to_delete, where_blank[dump_em])


    # Drop everything
    temp <- temp[-unique(lines_to_delete)]

    # Erase chunk labels
    temp <- temp %>%
      str_remove(",?\\s*version\\s*=\\s*c\\([^\\)]*\\)") %>%
      str_remove(",?\\s*version\\s*=\\s*[^\\s,\\}]*")


    new_name <- paste0(str_remove(orig_name, ".Rmd"),
                       glue::glue("_{v}.Rmd"))

    if (is.null(folders[[v]])) folders[[v]] <- v

    fol_name <- paste0(orig_dir, folders[[v]], "/")

    if (!dir.exists(fol_name)) { dir.create(fol_name) }

    new_name <- paste0(fol_name, new_name)

    options(knitr.duplicate.label = 'allow')

    # Fix relative file references

    temp <-
      temp %>%
      str_replace_all(fixed("./"), orig_dir)

    writeLines(temp, new_name)

    rmarkdown::render(new_name, envir = new.env())

  }


  knitr::opts_chunk$set(orig_opts)

}

#' Gets version tag information from chunks
#' Helper for \code{version()}
#' @importFrom stringr str_which str_subset str_detect str_extract str_extract_all
#' str_split str_trim
get_version_chunks <- function(source_text) {

  starts <- source_text %>% str_which("```\\{")
  all_ends <- source_text %>% str_which("```$")

  ends <- map_int(starts, ~min(all_ends[all_ends > .x]))


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


#' Gets version tag information from text
#' Helper for \code{version()}
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

#' @import stringr
prep_orig_text <- function(orig_text, orig_file, global_eval = TRUE) {

  # Remove lines that call templar::versions()
  start_call <- orig_text %>% str_which("versions\\(")
  end_call <- which(orig_text == "" | orig_text == "```")
  end_call <- min(end_call[end_call > start_call]) - 1

  orig_text <- orig_text[-c(start_call:end_call)]

  # Fix global eval
  where_global <-
    orig_text %>%
    str_which(fixed("opts_chunk$set"))

  if (global_eval & length(where_global) > 0) {

    orig_text[where_global] <- orig_text[where_global] %>%
      str_replace("eval\\s*=\\s*F(ALSE)?", "eval = TRUE")

  }


  # Put a warning at the top about editing the sub-files
  top_alert <- c(glue::glue("# Warning:  File created automatically"),
                 "# Do NOT edit this file directly, as it may be overwritten.")

  end_yaml <- stringr::str_which(orig_text, "---")[2] - 1

  orig_text <- c(orig_text[1:end_yaml], top_alert, orig_text[-c(1:end_yaml)])

  return(orig_text)

}


