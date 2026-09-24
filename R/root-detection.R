#' Get the IMPACT project root directory
#'
#' Locates the top-level project folder (e.g. `".../IMPACT NCI R01 -
#' Documents"`) so you never have to hardcode a path that's different on
#' every collaborator's computer.
#'
#' Resolution order:
#' \enumerate{
#'   \item `getOption("findIMPACT.root")`, if set.
#'   \item `Sys.getenv("IMPACT_ROOT")`, if set.
#'   \item Walk upward from `start` (default: current working directory)
#'     looking for a folder that contains a subfolder named `marker`
#'     (default `"Data"`).
#' }
#'
#' @param start Directory to start walking up from. Defaults to `getwd()`.
#' @param marker Name of the folder used to recognize the project root.
#'   Defaults to `"Data"`.
#' @param max_levels Maximum number of parent directories to check before
#'   giving up.
#'
#' @return A normalized path to the project root.
#'
#' @examples
#' \dontrun{
#' get_impact_root()
#'
#' # If auto-detection can't find it, set it once per session:
#' options(findIMPACT.root = "C:/path/to/IMPACT NCI R01 - Documents")
#' get_impact_root()
#' }
#'
#' @export
get_impact_root <- function(start = getwd(), marker = "Data", max_levels = 10) {
  opt <- getOption("findIMPACT.root")
  if (!is.null(opt) && nzchar(opt)) return(opt)

  env <- Sys.getenv("IMPACT_ROOT", unset = NA)
  if (!is.na(env) && nzchar(env)) return(env)

  candidate <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(max_levels)) {
    if (dir.exists(file.path(candidate, marker))) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break
    candidate <- parent
  }

  stop(
    "Could not auto-detect the IMPACT project root starting from '", start, "'.\n",
    "Fix this by doing ONE of the following:\n",
    "  1. Set your R/RStudio working directory to somewhere inside the project folder, or\n",
    "  2. options(findIMPACT.root = \"C:/path/to/IMPACT NCI R01 - Documents\"), or\n",
    "  3. Sys.setenv(IMPACT_ROOT = \"C:/path/to/IMPACT NCI R01 - Documents\"), or\n",
    "  4. pass root = \"...\" directly to find_files().",
    call. = FALSE
  )
}
