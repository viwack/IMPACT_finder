#!/usr/bin/env Rscript
# Command-line wrapper around the findIMPACT package.
#
# Usage:
#   Rscript find_impact_file.R "2023" "MA_contracts"
#   Rscript find_impact_file.R "Ideon" "2024" "network providers"
#   Rscript find_impact_file.R "Ideon" "2024" --preview-all
#   Rscript find_impact_file.R "Ideon" "2024" --no-preview
#   Rscript find_impact_file.R "Ideon" "2024" --root "/path/to/project"
#
# Works whether or not the package is installed:
#   - If findIMPACT is installed, it's loaded normally.
#   - If not (e.g. you just cloned the repo and want to try it first),
#     it sources the package's R/ files directly.

suppressWarnings({
  if (requireNamespace("findIMPACT", quietly = TRUE)) {
    library(findIMPACT)
  } else {
    this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (length(this_file) == 1) {
      repo_r_dir <- file.path(dirname(dirname(normalizePath(this_file))), "R")
      r_files <- list.files(repo_r_dir, pattern = "\\.R$", full.names = TRUE)
      if (length(r_files) == 0) {
        stop(
          "findIMPACT is not installed and the package R/ files could not be ",
          "found relative to this script. Install the package with ",
          "remotes::install_github(\"YOUR-ORG/findIMPACT\"), or run this script ",
          "from inside the cloned repo (inst/scripts/find_impact_file.R)."
        )
      }
      invisible(lapply(r_files, source))
    } else {
      stop("findIMPACT package is not installed. Install it with ",
           "remotes::install_github(\"YOUR-ORG/findIMPACT\").")
    }
  }
})

args <- commandArgs(trailingOnly = TRUE)

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
  list(value = args[idx + 1], args = args[-c(idx, idx + 1)])
}

root_step <- pop_value(args, "--root")
root_override <- root_step$value
args <- root_step$args

np_step <- pop_flag(args, "--no-preview")
no_preview <- np_step$present
args <- np_step$args

pa_step <- pop_flag(args, "--preview-all")
preview_all <- pa_step$present
args <- pa_step$args

if (length(args) == 0) {
  cat('Usage: Rscript find_impact_file.R "term1" "term2" ... ',
      '[--root "path"] [--no-preview] [--preview-all]\n', sep = "")
  quit(status = 1)
}

search_root <- if (!is.null(root_override)) {
  root_override
} else {
  tryCatch(get_impact_root(), error = function(e) {
    cat(conditionMessage(e), "\n")
    quit(status = 1)
  })
}

cat("Searching under:", search_root, "\n\n")

results <- do.call(find_files, c(as.list(args), list(root = search_root)))

if (nrow(results) == 0) {
  cat("No files found matching:", paste(args, collapse = ", "), "\n")
} else {
  cat(sprintf("Found %d match(es) for: %s\n\n", nrow(results), paste(args, collapse = ", ")))
  for (i in seq_len(nrow(results))) {
    cat(sprintf(
      "  [%d/%d terms matched]  %s\n",
      results$score[i], length(args), results$path[i]
    ))
  }

  if (!no_preview) {
    n_preview <- if (preview_all) nrow(results) else min(3, nrow(results))
    cat(sprintf("\nPreviewing top %d match(es):\n\n", n_preview))
    preview_top(results, n = n_preview)
  }
}
