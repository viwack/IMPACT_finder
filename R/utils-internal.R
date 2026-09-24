# Internal helpers - not exported. See find_files() for the public API.

#' @noRd
.tokenize <- function(text) {
  text <- tolower(text)
  tokens <- unlist(strsplit(text, "[\\\\/_\\-\\s\\.]+", perl = TRUE))
  tokens[nzchar(tokens)]
}

#' @noRd
.token_matches <- function(query_token, path_tokens, fuzzy_threshold = 0.75) {
  for (pt in path_tokens) {
    if (!nzchar(pt)) next
    # substring match in either direction (handles plurals, abbreviations
    # that are prefixes, etc.)
    if (grepl(query_token, pt, fixed = TRUE) || grepl(pt, query_token, fixed = TRUE)) {
      return(TRUE)
    }
    # fuzzy fallback via normalized edit distance, catches things like
    # "ctrct" vs "contract"
    max_len <- max(nchar(query_token), nchar(pt))
    if (max_len > 0) {
      similarity <- 1 - (utils::adist(query_token, pt)[1, 1] / max_len)
      if (similarity >= fuzzy_threshold) return(TRUE)
    }
  }
  FALSE
}
