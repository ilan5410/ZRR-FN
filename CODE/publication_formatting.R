# Small formatting helpers shared by generated LaTeX publication artifacts.

latex_escape_cell <- function(x) {
  replacements <- c(
    "\\" = "\\textbackslash{}",
    "&" = "\\&",
    "%" = "\\%",
    "$" = "\\$",
    "#" = "\\#",
    "_" = "\\_",
    "{" = "\\{",
    "}" = "\\}",
    "~" = "\\textasciitilde{}",
    "^" = "\\textasciicircum{}"
  )

  escaped <- as.character(x)
  for (pattern in names(replacements)) {
    escaped <- gsub(pattern, replacements[[pattern]], escaped, fixed = TRUE)
  }
  escaped
}
