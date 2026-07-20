#' Collect and summarize claim tests
#'
#' @param ... `claim_test` objects, or nested lists containing them.
#'
#' @return A `claim_test_suite` object.
#' @examples
#' claims <- test_claims(
#'   expect_positive_effect(2),
#'   expect_interval_includes(
#'     data.frame(estimate = 2, conf.low = 1, conf.high = 3),
#'     value = 2
#'   )
#' )
#' claims
#' summary(claims)
#' as.data.frame(claims)
#' @export
test_claims <- function(...) {
  results <- flatten_claims(list(...))
  if (!length(results)) stop("Provide at least one claim test.", call. = FALSE)
  valid <- vapply(results, inherits, logical(1), what = "claim_test")
  if (!all(valid)) stop("Every input must be a `claim_test` object.", call. = FALSE)
  new_claim_suite(results)
}

new_claim_suite <- function(results) {
  passed <- vapply(results, `[[`, logical(1), "passed")
  structure(
    list(
      total = length(results),
      passed = sum(passed),
      failed = sum(!passed),
      pass_rate = mean(passed),
      results = results
    ),
    class = c("claim_test_suite", "claim_test_summary")
  )
}

flatten_claims <- function(x) {
  output <- list()
  visit <- function(item) {
    if (inherits(item, "claim_test")) {
      output[[length(output) + 1L]] <<- item
    } else if (is.list(item)) {
      for (child in item) visit(child)
    } else {
      output[[length(output) + 1L]] <<- item
    }
  }
  for (item in x) visit(item)
  output
}

#' @export
print.claim_test_suite <- function(x, ...) {
  cat(sprintf(
    "Claim tests: %d total | %d passed | %d failed | %.1f%% pass rate\n",
    x$total, x$passed, x$failed, 100 * x$pass_rate
  ))
  for (result in x$results) {
    cat("\n")
    print(result)
  }
  invisible(x)
}

# Backward-compatible class method for early development objects.
#' @export
print.claim_test_summary <- function(x, ...) print.claim_test_suite(x, ...)

#' @export
summary.claim_test_suite <- function(object, ...) object

#' @export
summary.claim_test_summary <- function(object, ...) object

#' @export
as.data.frame.claim_test_suite <- function(x, row.names = NULL,
                                           optional = FALSE, ...) {
  rows <- lapply(x$results, as.data.frame)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' @export
as.data.frame.claim_test_summary <- function(x, row.names = NULL,
                                             optional = FALSE, ...) {
  as.data.frame.claim_test_suite(x, row.names, optional, ...)
}

#' Format claims for reports
#'
#' `report_claims()` creates either a plain data frame or a dependency-free
#' Markdown table suitable for Quarto and R Markdown documents.
#'
#' @param x A `claim_test`, `claim_test_suite`, or list of claim tests.
#' @param format Output format.
#'
#' @return A data frame for `format = "data.frame"`; otherwise a character
#'   object of class `claim_report` whose print method writes Markdown.
#' @examples
#' claims <- test_claims(expect_positive_effect(2), expect_negative_effect(2))
#' report_claims(claims)
#' report_claims(claims, format = "data.frame")
#' @export
report_claims <- function(x, format = c("markdown", "data.frame")) {
  format <- match.arg(format)
  suite <- as_claim_suite(x)
  data <- as.data.frame(suite)
  if (format == "data.frame") return(data)

  display <- data.frame(
    Claim = data$message,
    Result = ifelse(data$passed, "Passed", "Failed"),
    Evidence = data$evidence,
    stringsAsFactors = FALSE
  )
  header <- paste(names(display), collapse = " | ")
  separator <- paste(rep("---", ncol(display)), collapse = " | ")
  rows <- apply(display, 1L, function(row) {
    paste(vapply(row, escape_markdown_cell, character(1)), collapse = " | ")
  })
  structure(paste(c(paste0("| ", header, " |"),
                    paste0("| ", separator, " |"),
                    paste0("| ", rows, " |")), collapse = "\n"),
            class = c("claim_report", "character"))
}

as_claim_suite <- function(x) {
  if (inherits(x, "claim_test_suite")) return(x)
  if (inherits(x, "claim_test")) return(new_claim_suite(list(x)))
  if (is.list(x)) return(test_claims(x))
  stop("`x` must be a claim test, claim suite, or list of claim tests.", call. = FALSE)
}

escape_markdown_cell <- function(x) {
  x <- gsub("|", "\\\\|", as.character(x), fixed = TRUE)
  gsub("[\r\n]+", " ", x)
}

#' @export
print.claim_report <- function(x, ...) {
  cat(as.character(x), "\n", sep = "")
  invisible(x)
}
