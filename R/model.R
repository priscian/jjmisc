#' @export
split.formula <- function(x, re_plus_minus=NULL, remove_extra_parens=TRUE, ...)
{
  variables <- tail(as.character(attr(terms(x), "variables")), -1L)
  responseIndex <- attr(terms(x), "response")
  hasIntercept <- as.logical(attr(terms(x), "intercept"))

  rePlusMinus <- ifelse(is.null(re_plus_minus), "\\s+(\\+|-)\\s+", re_plus_minus)
  exes <- tail(as.character(x), 1L)
  right <- exes
  operatorIndices <- gregexpr(rePlusMinus, exes, perl=TRUE)[[1L]]
  operatorLengths <- attr(operatorIndices, "match.length")
  operators <- NULL
  for (i in seq_along(operatorIndices))
    operators <- c(operators, substr(exes, operatorIndices[i], operatorIndices[i] + operatorLengths[i] - 1L))
  operators <- operators[operators != ""]
  exes <- trimws(strsplit(exes, rePlusMinus, perl=TRUE)[[1L]])
  exes <- exes[exes != ""]
  if (length(operators) < length(exes))
    operators <- c("+", operators)

  left <- NULL; independents <- exes
  if (responseIndex != 0L)
    left <- variables[responseIndex];

  ## 'update.formula()' sometimes parenthesizes backquoted variables; remove those parentheses.
  if (remove_extra_parens) {
    reRemoveParens <- "(?:^|\\s+)\\((`.+?`)\\)(?:\\s+|$)"
    independents <- gsub(reRemoveParens, "\\1", independents)
    left <- gsub(reRemoveParens, "\\1", left)
    right <- gsub(reRemoveParens, "\\1", right)
  }

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
    terms = terms(x)
  )

  return (rv)
}

## usage:
# split(x <- a ~ b + 1 + c + .d + offset(e) + b %in% a)
