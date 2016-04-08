## N.B. This is rough; might have to upgrade later depending on need.
#' @export
create_events <- function(characteristics, p, ..., default_condition="== 1", mChoice=TRUE, label=NULL)
{
  if (missing(characteristics))
    stop("Event-characteristics list variable must be supplied.")

  if (!inherits(p, "pointer") && !inherits(p, "environment"))
    stop("Function argument 'p' is not a pointer or an environment.")

  events <- list()

  i <- 1
  for (characteristic in characteristics) {
    ## Check for already-created event list
    if (inherits(characteristic, "eventlist")) {
      events <- c(events, characteristic)

      next
    }

    ## Check for already-created event.
    if (inherits(characteristic, "event")) {
      events[["event" %_% i]] <- characteristic
      i <- i + 1

      next
    }

    characteristic <- as.list(characteristic)

    ## Variable name.
    variable <- NULL
    if (!is.null(characteristic$variable))
      variable <- characteristic$variable
    else variable <- characteristic[[1]]

    ## Test condition.
    condition <- default_condition
    if (!is.null(characteristic$condition))
      condition <- characteristic$condition
    else if (length(characteristic) >= 2 && is.null(characteristic$description))
      condition <- characteristic[[2]]

    context <- ifelse (inherits(p, "pointer"), "..(p)[['", "p[['")
    x <- eval_js(context %_% variable %_% "']]")

    if (!is.null(x)) {
      description <- variable
      if (!is.null(characteristic$description))
        description <- characteristic$description
      else if (length(characteristic) >= 3)
        description <- characteristic[[3]]
      else if (!is.null(attr(x, "label")))
        description <- Hmisc::label(x)

      tx <- eval_js("x" %_% " " %_% condition)
      tx[is.na(tx)] <- F; tx[!tx] <- ""; tx[tx != ""] <- description
      class(tx) <- "event"

      events[[variable]] <- tx
    }
  }

  if (mChoice) {
    vectors <- paste(paste("events[[", 1:length(events), sep=''), "]]", sep='', collapse=", ")
    mc <- eval_js("Hmisc::mChoice(" %_% vectors %_% ", ..., label=label)")
    attr(mc, "events") <- events

    return (mc)
  }
  else {
    events <- c(events, list(...))
    class(events) <- "eventlist"

    return (events)
  }
}
