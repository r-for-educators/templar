bob <- readLines("./dev/test-test.Rmd")

chunk_info <- data.frame(

  chunk_starts = bob %>% str_which("```\\{"),
  chunk_ends = bob %>% str_which("```$"),
  is_versioned = bob %>%
    str_subset("```\\{") %>%
    str_detect("version\\s*=")

)

bob_opts <- bob %>%
  str_subset("```\\{") %>%
  str_subset("version\\s*=")

bob_opts_where <- bob_opts %>%
  str_extract_all(",\\s*[:alpha:]+\\s*=\\s*") %>%
  map(~str_which(.x, "version"))

chunk_versions <- bob_opts  %>%
  str_split(",\\s*[:alpha:]+\\s*=\\s*") %>%
  map2_chr(bob_opts_where, ~.x[[.y+1]]) %>%
  map(~unlist(str_extract_all(.x, '(?<=\\")[:alnum:]+')))

my_versions <- chunk_versions %>% unlist() %>% unique()


for (v in my_versions) {

  chunk_info[!is_versioned, v] <- TRUE
  chunk_info[is_versioned, v] <- chunk_versions %>% map_lgl(~any(str_detect(.x, v)))

}
