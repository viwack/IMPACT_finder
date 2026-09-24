#' Find files in the IMPACT project folder using fuzzy keywords
#'
#' Search recursively under the IMPACT project root for files whose path
#' (folders + filename) matches the given search terms, without needing to
#' know or hardcode the exact filename or folder structure.
#'
#' Matching is token-based and fuzzy:
#' \itemize{
#'   \item Each search term is split into tokens on spaces, underscores,
#'     hyphens, dots, and slashes (e.g. \code{"MA_contracts"} becomes
#'     \code{"ma"}, \code{"contracts"}).
#'   \item Each token is checked against every token in the file's full
#'     path using substring matching in both directions (so
#'     \code{"contracts"} matches \code{"contract"}), plus a fuzzy
#'     edit-distance fallback that catches abbreviations like
#'     \code{"ctrct"} for \code{"contract"}.
#'   \item By default a file must match every search term
#'     (\code{match_all = TRUE}) to be returned.
#' }
#'
#' @param ... One or more keywords/phrases, e.g. \code{"2023"},
#'   \code{"MA_contracts"}, \code{"network providers"}.
#' @param root Folder to search under. Defaults to \code{\link{get_impact_root}()}.
#' @param match_all If \code{TRUE} (default), a file must match every
#'   search term. If \code{FALSE}, matching any term is enough.
#' @param fuzzy_threshold Similarity cutoff (0-1) for the fuzzy fallback
#'   match. Lower values match more loosely.
#' @param extensions Optional character vector to filter by file type,
#'   e.g. \code{c(".xls", ".xlsx")}.
#' @param max_results Maximum number of results to return, best matches
#'   first.
#'
#' @return A data.frame with columns \code{path} and \code{score} (number
#'   of search terms matched), sorted best match first.
#'
#' @examples
#' \dontrun{
#' find_files("2023", "MA_contracts")
#' find_files("Ideon", "2024", "network providers")
#' find_files("readme", match_all = FALSE)
#' }
#'
#' @export
find_files <- function(...,
                        root = NULL,
                        match_all = TRUE,
                        fuzzy_threshold = 0.75,
                        extensions = NULL,
                        max_results = 50) {
  if (is.null(root)) root <- get_impact_root()

  if (!dir.exists(root)) {
    stop("root does not exist: ", root, call. = FALSE)
  }

  search_terms <- c(...)
  query_tokens <- unlist(lapply(search_terms, .tokenize))

  empty <- data.frame(path = character(0), score = integer(0), stringsAsFactors = FALSE)
  if (length(query_tokens) == 0) return(empty)

  all_files <- list.files(root, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[!dir.exists(all_files)]

  if (!is.null(extensions)) {
    extensions <- tolower(extensions)
    extensions <- ifelse(grepl("^\\.", extensions), extensions, paste0(".", extensions))
    ext_pattern <- paste0("(", paste(gsub("\\.", "\\\\.", extensions), collapse = "|"), ")$")
    all_files <- all_files[grepl(ext_pattern, tolower(all_files))]
  }

  if (length(all_files) == 0) return(empty)

  paths <- character(0)
  scores <- integer(0)

  for (f in all_files) {
    path_tokens <- .tokenize(f)
    matched <- vapply(query_tokens, function(qt) {
      .token_matches(qt, path_tokens, fuzzy_threshold)
    }, logical(1))
    score <- sum(matched)

    keep <- if (match_all) all(matched) else any(matched)
    if (keep) {
      paths <- c(paths, f)
      scores <- c(scores, score)
    }
  }

  if (length(paths) == 0) return(empty)

  df <- data.frame(path = paths, score = scores, stringsAsFactors = FALSE)
  # Best matches first (higher score), then shorter paths (usually the more
  # "canonical" file rather than something buried in an archive).
  df <- df[order(-df$score, nchar(df$path)), ]
  rownames(df) <- NULL
  head(df, max_results)
}
