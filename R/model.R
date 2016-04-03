#' @export
split.formula <- function(x, ...)
{
  variables <- tail(as.character(attr(terms(x), "variables")), -1L)
  responseIndex <- attr(terms(x), "response")
  hasIntercept <- as.logical(attr(terms(x), "intercept"))

  re <- "\\+|-"
  exes <- tail(as.character(x), 1L)
  right <- exes
  operatorIndices <- gregexpr(re, exes, perl=TRUE)[[1L]]
  operatorLengths <- attr(operatorIndices, "match.length")
  operators <- NULL
  for (i in seq_along(operatorIndices))
    operators <- c(operators, substr(exes, operatorIndices[i], operatorIndices[i] + operatorLengths[i] - 1L))
  operators <- operators[operators != ""]
  exes <- trimws(strsplit(exes, re, perl=TRUE)[[1L]])
  exes <- exes[exes != ""]
  if (length(operators) < length(exes)) operators <- c("+", operators)

  left <- NULL; independents <- exes
  if (responseIndex != 0L) left <- variables[responseIndex];

  characters <- left %_% " ~ " %_% right

  rv = list(
    left_side = left,
    right_side = right,
    as_character = characters,
    independent = independents,
    operators = operators,
    variables = variables,
    all_vars = all.vars(x),
    intercept = hasIntercept,
    term_labels=attr(terms(x), "term.labels")
  )

  return (rv)
}

## usage:
# split(x <- a ~ b + 1 + c + .d + offset(e) + b %in% a)
