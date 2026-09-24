#' Preview a matched file
#'
#' Prints file info (size, last modified date) and, for common tabular
#' file types, a peek at the contents (sheet names, column names, first
#' rows) so you can confirm this is really the file you're looking for
#' before opening it.
#'
#' @param path Path to the file to preview (typically from \code{\link{find_files}()}).
#' @param n_rows Number of rows to preview for tabular files. Default 5.
#'
#' @return Invisibly returns \code{NULL}. Called for its printed output.
#'
#' @examples
#' \dontrun{
#' results <- find_files("Ideon", "2024", "network providers")
#' preview_file(results$path[1])
#' }
#'
#' @export
preview_file <- function(path, n_rows = 5) {
  cat(strrep("-", 70), "\n")
  cat("File:", path, "\n")

  info <- tryCatch(file.info(path), error = function(e) NULL)
  if (!is.null(info) && !is.na(info$size)) {
    size_kb <- info$size / 1024
    cat(sprintf(
      "Size: %.1f KB  |  Last modified: %s\n",
      size_kb, format(info$mtime, "%Y-%m-%d %H:%M")
    ))
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
        cat(sprintf(
          "Columns in '%s' (%d): %s\n",
          sheets[1], ncol(df), paste(names(df), collapse = ", ")
        ))
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
  invisible(NULL)
}

#' Preview the top matches from find_files()
#'
#' Convenience wrapper that runs \code{\link{preview_file}()} on the top
#' \code{n} rows of a \code{\link{find_files}()} result, so you don't have
#' to loop over the data.frame yourself.
#'
#' @param results A data.frame returned by \code{\link{find_files}()}.
#' @param n Number of top matches to preview. Default 3.
#' @param n_rows Rows to preview per file. Default 5.
#'
#' @return Invisibly returns \code{NULL}. Called for its printed output.
#'
#' @examples
#' \dontrun{
#' results <- find_files("2023", "MA_contracts")
#' preview_top(results)
#' }
#'
#' @export
preview_top <- function(results, n = 3, n_rows = 5) {
  if (nrow(results) == 0) {
    message("No results to preview.")
    return(invisible(NULL))
  }
  n <- min(n, nrow(results))
  for (i in seq_len(n)) {
    preview_file(results$path[i], n_rows = n_rows)
  }
  if (nrow(results) > n) {
    message(sprintf(
      "(%d more match(es) not previewed - call preview_top(results, n = %d) to see more.)",
      nrow(results) - n, nrow(results)
    ))
  }
  invisible(NULL)
}
