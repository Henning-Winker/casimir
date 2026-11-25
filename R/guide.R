# The guides ship in inst/guide (always installed); the vignettes include
# them. casimir_guide() copies a guide to a folder as a stand-alone R
# Markdown file, and can render it.

casimir_guides <- list(
  `how-it-works` = list(file = "how-it-works.md", out = "casimir-00-how-it-works",
                        title = "How casimir works"),
  `user-guide`   = list(file = "user-guide.Rmd", out = "casimir-02-user-guide",
                        title = "casimir user guide: one survey on Island X"),
  teaching       = list(file = "teaching.Rmd", out = "casimir-03-teaching",
                        title = "casimir teaching cards: bet, roll, spin"))

#' Copy, and optionally render, a casimir guide
#'
#' The guides are installed with the package (folder `guide`), whether or not
#' the vignettes were built. `casimir_guide()` writes a guide to `dir` as a
#' stand-alone R Markdown file that trainers can edit and knit.
#'
#' @param guide `"how-it-works"`, `"user-guide"`, `"teaching"` or `"all"`.
#' @param dir Folder to write to (created).
#' @param render Also render to HTML (needs 'rmarkdown' and pandoc).
#' @param overwrite Replace existing files.
#' @return The paths of the files written, invisibly.
#' @export
#' @examples
#' \dontrun{
#' casimir_guide("teaching", dir = "course")
#' }
casimir_guide <- function(guide = c("how-it-works", "user-guide", "teaching", "all"),
                          dir = ".", render = FALSE, overwrite = FALSE) {
  guide <- match.arg(guide)
  if (guide == "all") {
    return(invisible(unlist(lapply(names(casimir_guides), casimir_guide, dir = dir,
                                   render = render, overwrite = overwrite))))
  }
  g <- casimir_guides[[guide]]
  src <- system.file("guide", g$file, package = "casimir")
  if (!nzchar(src)) stop("Guide not found: reinstall casimir")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  out <- file.path(dir, paste0(g$out, ".Rmd"))
  if (file.exists(out) && !overwrite) stop(out, " exists; use overwrite = TRUE")
  head <- c("---", paste0("title: \"", g$title, "\""), "output:",
            "  html_document:", "    toc: true", "---", "")
  writeLines(c(head, readLines(src, warn = FALSE, encoding = "UTF-8")), out, useBytes = TRUE)
  if (render) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("render = TRUE needs 'rmarkdown'")
    rmarkdown::render(out, quiet = TRUE)
  }
  invisible(out)
}
