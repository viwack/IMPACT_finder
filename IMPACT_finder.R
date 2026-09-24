# find_impact_file.R
# -------------------
# Search for files anywhere under the IMPACT project folder using loose
# keywords, instead of hardcoding filenames or paths. R version of
# find_impact_file.py - same behavior, same matching logic.
#
# Matching is TOKEN-BASED and FUZZY:
#   - Search terms are split on spaces, underscores, hyphens, dots, slashes.
#     e.g. "MA_contracts" -> "ma", "contracts"
#   - Each token is checked against every token in the full file path
#     (folders + filename): substring match in both directions (so
#     "contracts" matches "contract") plus a fuzzy edit-distance fallback
#     for abbreviations/near-misses (e.g. "ctrct" vs "contract").
#   - By default a file must match ALL search terms (match_all = TRUE),
#     but you can loosen that with match_all = FALSE.
#
# After searching, it also PREVIEWS the top matches (file size, last
# modified date, and - for .csv/.xlsx/.dta files - the column names and
# first few rows) so you can confirm it found the right file before
# opening it.
#
# USAGE (from a terminal, with R installed):
#
#   Rscript find_impact_file.R "2023" "MA_contracts"
#   Rscript find_impact_file.R "Ideon" "2024" "network providers"
#   Rscript find_impact_file.R "Ideon" "2024" --no-preview
#   Rscript find_impact_file.R "Ideon" "2024" --preview-all
#   Rscript find_impact_file.R "Ideon" "2024" --root "some/other/path"
#
# Or source it and call the function directly from an R / RStudio session:
#
#   source("find_impact_file.R")
#   results <- find_files("Ideon", "2024", "network providers")
#   print(results)
#   preview_file(results$path[1])
#
# SETUP FOR THE TEAM (read this once):
#   Drop this script anywhere inside the "IMPACT NCI R01 - Documents" folder
#   on your own OneDrive (top level is easiest). You do NOT need to edit any
#   paths - it automatically finds the project root by walking up from its
#   own location until it finds the "Data" folder, so it works no matter
#   whose computer it's on or what your local OneDrive path looks like
#   (Windows or Mac).

# Name of a folder that reliably marks the top of the project, used for
# auto-detecting the project root regardless of whose machine this runs on.
.ROOT_MARKER <- "Data"
.MAX_LEVELS_UP <- 6

# ---- root auto-detection -----------------------------------------------

.get_script_dir <- function() {
  # Works when run via `Rscript file.R` (most common for this tool).
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  # Fallback for sourcing inside RStudio.
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path)) {
      return(dirname(normalizePath(ctx$path)))
    }
  }
  # Last resort: current working directory.
  getwd()
}

.autodetect_root <- function(marker = .ROOT_MARKER, max_levels = .MAX_LEVELS_UP) {
  candidate <- .get_script_dir()
  start <- candidate
  for (i in seq_len(max_levels)) {
    if (dir.exists(file.path(candidate, marker))) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break  # reached filesystem root
    candidate <- parent
  }
  start
}

DEFAULT_ROOT <- .autodetect_root()

# ---- tokenizing + fuzzy matching ---------------------------------------

.tokenize <- function(text) {
  text <- tolower(text)
  tokens <- unlist(strsplit(text, "[\\\\/_\\-\\s\\.]+", perl = TRUE))
  tokens[nzchar(tokens)]
}

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

# ---- main search function ----------------------------------------------

#' Search `root` recursively for files matching search_terms.
#'
#' @param ... One or more keywords/phrases, e.g. "2023", "MA_contracts",
#'   "network providers". Each is tokenized and matched independently.
#' @param root Folder to search under. Defaults to the auto-detected
#'   IMPACT project root.
#' @param match_all If TRUE (default), a file must match every search term
#'   to be returned. If FALSE, matching any term is enough.
#' @param fuzzy_threshold 0-1 similarity cutoff for the fuzzy fallback
#'   match. Lower = looser matching.
#' @param extensions Optional character vector filter, e.g. c(".xls",
#'   ".xlsx") to only return spreadsheet files.
#' @param max_results Cap on number of results returned (best matches first).
#'
#' @return A data.frame with columns `path` and `score` (number of search
#'   terms that matched), sorted best match first.
find_files <- function(...,
                        root = DEFAULT_ROOT,
                        match_all = TRUE,
                        fuzzy_threshold = 0.75,
                        extensions = NULL,
                        max_results = 50) {
  search_terms <- c(...)
  query_tokens <- unlist(lapply(search_terms, .tokenize))

  empty <- data.frame(path = character(0), score = integer(0), stringsAsFactors = FALSE)
  if (length(query_tokens) == 0) return(empty)

  all_files <- list.files(root, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[!dir.exists(all_files)]  # files only, not folders

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

# ---- preview -------------------------------------------------------------

#' Print file info (size, last modified) plus a peek at contents when
#' possible, so you can confirm this is really the file you wanted.
preview_file <- function(path, n_rows = 5) {
  cat(strrep("-", 70), "\n")
  cat("File:", path, "\n")

  info <- tryCatch(file.info(path), error = function(e) NULL)
  if (!is.null(info) && !is.na(info$size)) {
    size_kb <- info$size / 1024
    cat(sprintf("Size: %.1f KB  |  Last modified: %s\n",
                 size_kb, format(info$mtime, "%Y-%m-%d %H:%M")))
  }

  ext <- tolower(tools::file_ext(path))

  tryCatch({
    if (ext %in% c("csv", "tsv", "txt")) {
      sep <- if (ext == "tsv") "\t" else ","
      df <- utils::read.csv(path, sep = sep, nrows = n_rows, stringsAsFactors = FALSE)
      cat(sprintf("Columns (%d): %s\n", ncol(df), paste(names(df), collapse = ", ")))
      cat(sprintf("First %d row(s):\n", nrow(df)))
      print(df)

    } else if (ext %in% c("xlsx", "xls")) {
      if (requireNamespace("readxl", quietly = TRUE)) {
        sheets <- readxl::excel_sheets(path)
        cat("Sheets:", paste(sheets, collapse = ", "), "\n")
        df <- readxl::read_excel(path, sheet = sheets[1], n_max = n_rows)
        cat(sprintf("Columns in '%s' (%d): %s\n",
                     sheets[1], ncol(df), paste(names(df), collapse = ", ")))
        cat(sprintf("First %d row(s):\n", nrow(df)))
        print(df)
      } else {
        cat("(Install the 'readxl' package to preview Excel contents: ",
            "install.packages('readxl'))\n", sep = "")
      }

    } else if (ext == "dta") {
      if (requireNamespace("haven", quietly = TRUE)) {
        df <- haven::read_dta(path, n_max = n_rows)
        cat(sprintf("Columns (%d): %s\n", ncol(df), paste(names(df), collapse = ", ")))
        cat(sprintf("First %d row(s):\n", nrow(df)))
        print(df)
      } else {
        cat("(Install the 'haven' package to preview Stata .dta contents: ",
            "install.packages('haven'))\n", sep = "")
      }

    } else {
      cat("(No content preview for this file type - file info shown above.)\n")
    }
  }, error = function(e) {
    cat("(Could not preview contents:", conditionMessage(e), ")\n")
  })

  cat(strrep("-", 70), "\n")
}

.print_results <- function(results, terms, preview_count = 3,
                            do_preview = TRUE, n_rows = 5) {
  if (nrow(results) == 0) {
    cat("No files found matching:", paste(terms, collapse = ", "), "\n")
    return(invisible(NULL))
  }

  cat(sprintf("Found %d match(es) for: %s\n\n", nrow(results), paste(terms, collapse = ", ")))
  for (i in seq_len(nrow(results))) {
    cat(sprintf("  [%d/%d terms matched]  %s\n",
                 results$score[i], length(terms), results$path[i]))
  }

  if (do_preview) {
    n <- min(preview_count, nrow(results))
    cat(sprintf("\nPreviewing top %d match(es) so you can confirm this is what you're looking for:\n\n", n))
    for (i in seq_len(n)) {
      preview_file(results$path[i], n_rows = n_rows)
    }
    if (nrow(results) > n) {
      cat(sprintf("\n(%d more match(es) not previewed - use --preview-all to preview every match, or --no-preview to skip previews entirely.)\n",
                   nrow(results) - n))
    }
  }
}

# ---- command-line entry point -------------------------------------------

.is_run_via_rscript <- function() {
  any(grepl("^--file=", commandArgs(trailingOnly = FALSE)))
}

if (.is_run_via_rscript()) {
  raw_args <- commandArgs(trailingOnly = TRUE)

  pop_flag <- function(args, name) {
    list(present = name %in% args, args = args[args != name])
  }
  pop_value <- function(args, name) {
    idx <- which(args == name)
    if (length(idx) == 0) return(list(value = NULL, args = args))
    idx <- idx[1]
    if (idx == length(args)) {
      cat(sprintf("Usage: %s must be followed by a value\n", name))
      quit(status = 1)
    }
    value <- args[idx + 1]
    args <- args[-c(idx, idx + 1)]
    list(value = value, args = args)
  }

  root_step <- pop_value(raw_args, "--root")
  root_override <- root_step$value
  raw_args <- root_step$args

  np_step <- pop_flag(raw_args, "--no-preview")
  no_preview <- np_step$present
  raw_args <- np_step$args

  pa_step <- pop_flag(raw_args, "--preview-all")
  preview_all <- pa_step$present
  raw_args <- pa_step$args

  if (length(raw_args) == 0) {
    cat('Usage: Rscript find_impact_file.R "term1" "term2" ... ',
        '[--root "path"] [--no-preview] [--preview-all]\n', sep = "")
    quit(status = 1)
  }

  search_root <- if (!is.null(root_override)) root_override else DEFAULT_ROOT
  cat("Searching under:", search_root, "\n\n")

  matches <- do.call(find_files, c(as.list(raw_args), list(root = search_root)))

  preview_count <- if (preview_all) nrow(matches) else 3
  .print_results(matches, raw_args, preview_count = preview_count, do_preview = !no_preview)
}
