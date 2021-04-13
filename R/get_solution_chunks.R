#' Function to label the solution chunks correctly
#'
#' @param version_chunk Chunks to knit in the documents
#' @param all_info All the code chunks in the data
#'
#' @return Returns all the code chunks with the solutions correctly labeled as so

get_solution_chunks <- function(version_chunk, all_info){
  sol_name <- glue::glue("solution_{version_chunk}")

  general_sol_exists <- sum(all_info[["solution"]]) > 0
  specific_sol_exists <- !is.null(all_info[[sol_name]])

  if (specific_sol_exists | general_sol_exists) {

    if (!specific_sol_exists) { all_info[[sol_name]] <- FALSE }

    all_info[[sol_name]] <-
      all_info[[version_chunk]] |
      all_info[["solution"]] |
      all_info[[sol_name]]

  }

  return(all_info)

}
