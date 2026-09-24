# findIMPACT

Find files anywhere in the IMPACT NCI R01 project folder using loose,
fuzzy keywords - no hardcoded filenames or paths required, and it works
the same way on every team member's computer.

```r
find_files("2023", "MA_contracts")
#> path                                                                score
#> .../Data/MA_Plans/MA_ctrct/Monthly_Report_By_Contract_2023_01_updated/...   2

find_files("Ideon", "2024", "network providers")
```

Search terms don't need to exactly match folder/file names. `"MA_contracts"`
still finds a path containing `MA_ctrct` because search terms are split
into tokens and matched fuzzily (substring + edit-distance), so
abbreviations and plurals are caught automatically.

After searching, preview the top matches to confirm you found the right
file before opening it - shows file size, last modified date, and for
`.csv`/`.xlsx`/`.xls`/`.dta` files, the column names and first few rows:

```r
results <- find_files("Ideon", "2024", "network providers")
preview_top(results)
```

## Install

```r
# install.packages("remotes")  # if you don't have it
remotes::install_github("YOUR-ORG/findIMPACT")
```

Replace `YOUR-ORG` with wherever this repo actually lives once it's
pushed to GitHub.

## Usage

```r
library(findIMPACT)

# Basic search - requires ALL terms to match (default)
find_files("2023", "MA_contracts")

# Looser search - matches ANY term
find_files("readme", "session", match_all = FALSE)

# Filter by file type
find_files("Ideon", "2024", extensions = c(".xls", ".xlsx"))

# Preview to confirm you got the right file
results <- find_files("Ideon", "2024", "network providers")
preview_top(results)          # preview top 3 (default)
preview_top(results, n = 10)  # preview more
preview_file(results$path[1]) # preview just one specific file
```

### Setting the project root

`find_files()` auto-detects the project root by looking for a folder
named `Data` at or above your current R working directory. If that
doesn't work for you (e.g. you're running R from somewhere outside the
project), set it once per session:

```r
options(findIMPACT.root = "C:/Users/you/OneDrive - University of Pittsburgh/IMPACT NCI R01 - Documents")
```

or as an environment variable (e.g. in your `.Renviron`):

```
IMPACT_ROOT=C:/Users/you/OneDrive - University of Pittsburgh/IMPACT NCI R01 - Documents
```

or just pass it directly:

```r
find_files("2023", "MA_contracts", root = "C:/path/to/IMPACT NCI R01 - Documents")
```

### Command line

If you'd rather not open an R session, there's a CLI wrapper at
`inst/scripts/find_impact_file.R`. After installing the package (or from
inside a cloned copy of this repo):

```bash
Rscript find_impact_file.R "2023" "MA_contracts"
Rscript find_impact_file.R "Ideon" "2024" "network providers"
Rscript find_impact_file.R "Ideon" "2024" --no-preview
Rscript find_impact_file.R "Ideon" "2024" --preview-all
Rscript find_impact_file.R "Ideon" "2024" --root "/path/to/project"
```

## Functions

| Function | What it does |
|---|---|
| `find_files(...)` | Fuzzy keyword search under the project root; returns a data.frame of matches. |
| `preview_file(path)` | Prints size, modified date, and a content peek for one file. |
| `preview_top(results, n = 3)` | Runs `preview_file()` on the top `n` rows of a `find_files()` result. |
| `get_impact_root()` | Returns the auto-detected (or manually set) project root path. |

## Development

This package uses [roxygen2](https://roxygen2.r-lib.org/) comments for
documentation. If you edit a function's roxygen comments, regenerate
`NAMESPACE` and the `man/` pages with:

```r
devtools::document()
```

Run the test suite with:

```r
devtools::test()
```
