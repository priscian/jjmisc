#' @export
surv_brief = function(f, x, survival_type="both", subset=NULL, summary...=NULL, survfit_frame...=FALSE)
{
  p <- x
  if (!inherits(p, "pointer"))
    p <- ptr(x)

  if (is.null(summary...))
    summary... <- list(times=seq(0L, by=12L, length.out=11L))

  temp <- NULL
  for (i in survival_type) {
    if (i == "both") temp = c(temp, "pfs", "os")
    else temp <- c(temp, i)
  }
  survival_type <- temp

  formulas <- list() # Or: 'structure(vector(mode="list", length(survival_type)), names=survival_type)'
  for (i in survival_type) {
    if (inherits(f, "formula")) # Can't use 'ifelse()' here because of incompatible types.
      formulas[[i]] <- f
    else
      formulas[[i]] <- f[[i]]

    environment(formulas[[i]]) <- environment() # Need to change the formula's environment from 'R_GlobalEnv'.
  }

  subset_ <- list()
  if (!is.list(subset)) {
    for (i in survival_type)
      subset_[[i]] <- subset
  }
  else
    subset_ <- subset

  survfitFrameArgs <- list(
    skip = 2,
    header = TRUE,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  summaryArgs <- list()
  summaryArgs <- modifyList(summaryArgs, summary...)

  r <- list()

  for (i in survival_type) {
    f <- eval(substitute(update(formulas[[i]], v ~ .), list(v=as.symbol(i))))

    sf <- rms::npsurv(f, data=..(p), subset=subset_[[i]]) # 'survfit' objects don't work with 'survplot()' any more (8 Jan. 2015).

    makeSurvfitFrame <- FALSE
    if (is.list(survfit_frame...))
      makeSurvfitFrame <- TRUE
    else if (is.logical(survfit_frame...)) {
      if (survfit_frame...) {
        makeSurvfitFrame <- TRUE
        survfit_frame... <- list()
      }
    }

    sfdf <- NULL
    if (makeSurvfitFrame) {
      cosf <- capture.output(sf)

      tempArgs <- survfitFrameArgs
      tempArgs <- modifyList(survfitFrameArgs, survfit_frame...)

      ## Check for comment on missing values; if present, bump up skip count.
      tc <- textConnection(cosf)
      tempText <- readLines(tc); close(tc)
      if (any(grepl("due to missingness", tempText)))
        tempArgs$skip <- tempArgs$skip + 1L

      tc <- textConnection(cosf) # Can I do this without creating a new 'textConnection'? 'seek()' doesn't work here.
      tempArgs$file <- tc

      sfdf <- do.call("read.table", tempArgs)

      close(tc)
    }

    tempArgs <- summaryArgs
    tempArgs$object <- sf
    sm <- do.call("summary", tempArgs)

    sdiff <- NULL
    if (attr(..(p)[[i]], "type") == "right" && split(f)$independent_terms[1L] != 1)
      sdiff <- survdiff(f, data=..(p), subset=subset_[[i]])

    r[[i]] <- list(
      formula = f,
      survfit = sf,
      data.frame = sfdf,
      summary = sm,
      survdiff = sdiff
    )
  }

  class(r) <- "surv_brief"

  return (r)
}


#' @export
print_bshazard <- function (x, ...)
{
  ncov <- length(x$cov.value)
  if (is.null(x$cov.value)) {
    n.e <- sum(x$raw.data$n.event)
    pt <- sum(x$raw.data$person.time)
    r <- n.e/pt

    rv <- data.frame(n.events = as.numeric(n.e), person.time=as.numeric(pt), rate=as.numeric(r))
    attr(rv, "n") <- x$n

    return (rv)
  }
  if (!is.null(x$cov.value)) {
    agg <- aggregate(x$raw.data, by=list(x$raw.data[, 5:(5 + ncov - 1)]), FUN="sum")
    agg$rate <- agg$n.event/agg$person.time
    agg[, attr(x$cov.value, "names")] <- agg[, 1:(1 + ncov - 1)]

    rv <- data.frame(agg[, c(attr(x$cov.value, "names"), "n.event", "person.time", "rate")])
    attr(rv, "n") <- x$n

    return (rv)
  }
}


#' @export
plot_survival_by_strata <- function(km_fit, cox_fit=NULL, cox_term=1L, surv_fun=rms::survplot, legend_text=NULL, HR=NULL, pad=0.0, pvalue=NULL, xlab=NULL, ylab=NULL, caption="", plot...=list(), mtext...=list(), legend...=list(), caption_translate=FALSE, latex=FALSE, setpar=NULL, ...)
{
  temp <- names(km_fit$strata)

  GetGroups <- function(x) # This might fail for weird strata names, so I probably should allow the groups to be passed in as a parameter.
  {
    re <- "^.*?\\=(.*?)$"
    idMatches <- gregexpr(re, x, perl=TRUE)[[1]] # Note that 'regmatches()' doesn't seem to work for 'perl=TRUE'.
    idIndices <- attr(idMatches, "capture.start")
    idLengths <- attr(idMatches, "capture.length")
    rv <- NULL
    for (j in seq_along(idIndices))
      rv <- c(rv, substr(x, idIndices[j], idIndices[j] + idLengths[j] - 1L))

    return (rv)
  }

  groups <- NULL
  for (i in seq_along(temp))
    groups <- c(groups, GetGroups(temp[i]))

  if (is.null(xlab)) xlab <- "Time (months)"
  if (is.null(ylab)) ylab <- "Probability"

  plotArgs <- list(
    fit = km_fit,
    conf = "none",
    label.curves = FALSE,
    n.risk = TRUE,
    y.n.risk = -0.22,
    sep.n.risk = 0.030,
    cex.n.risk = 0.7,
    levels.only = TRUE,
    cols = gray.colors(length(groups), 0.0, 0.6),
    lty = seq_along(groups),
    lwd = 2,
    xlab = "",
    ylab = ylab,
    main = ""
  )
  plotArgs <- modifyList(plotArgs, plot...)

  if (is.null(legend_text))
    legend_text <- groups

  PlotXLabAndLegend <- function()
  {
    mtextArgs = list(
      text = xlab,
      side = 1,
      line = 2
    )
    mtextArgs <- modifyList(mtextArgs, mtext...)

    do.call("mtext", mtextArgs)

    legendArgs <- list(
      x = "bottomleft",
      xjust = 0,
      yjust = 0,
      inset = c(0.05),
      legend = legend_text,
      lty = seq_along(legend_text),
      lwd = 2,
      cex = 0.8,
      bty = "o",
      title = NULL,
      bg = NULL
    )
    legendArgs <- modifyList(legendArgs, legend...)

    if (!is.null(legend_text))
      do.call("legend", legendArgs)
  }

  PlotPValue <- function() # But think about whether a single p-value here might be misinterpreted as coming from a log-rank test.
  {
    formattedPValue <- get_formatted_p_value(pvalue, le="<", fmt="%1.3f")
    text_ <- "P " %_% ifelse(grepl("[~<>]", formattedPValue), "", "= ") %_% formattedPValue
    xy <- get_cp_coords("ne", margin_x_percent=10, margin_y_percent=0)

    text(x=xy, labels=text_, pos=c(1, 2), cex=1.2)
  }

  PlotHRs <- function()
  {
    # Add HRs to plot
    s <- km_fit
    maxTime <- minSurv <- NULL
    i <- 1
    xlim <- par("xaxp")[1:2]
    ylim <- par("yaxp")[1:2]
    for (j in s$strata) {
      stime <- s$time[i:(i + j - 1)]
      maxTimeIndex <- whichClosest(stime, xlim[2])
      maxTimej <- stime[maxTimeIndex]
      maxTime <- c(maxTime, maxTimej)
      ssurv <- s$surv[i:(i + j - 1)]
      minSurvj <- ssurv[maxTimeIndex]
      minSurv <- c(minSurv, minSurvj)
      i <- i + j
    }

    if (length(pad) < length(s$strata) * 2)
      pad <- rep_len(pad, length(s$strata) * 2)

    for (j in 0:length(groups)) {
      if (j == 0)
        hr <- "1.00"
      else
        hr <- sprintf(exp(coef(cox_fit))[j + cox_term - 1], fmt="%1.2f")
      if (!is.null(HR))
        hr <- HR[j + 1]

      text(maxTime[j + 1] + pad[j * 2 + 1], minSurv[j + 1] + pad[j * 2 + 2], "HR: " %_% hr, pos=4, cex=0.7)
    }
  }

  PlotExtras <- function() { title(plotArgs$main); PlotXLabAndLegend(); if (!is.null(cox_fit)) PlotHRs(); if (!is.null(pvalue)) PlotPValue(); }

  if (!latex) {
    if (!is.null(setpar)) eval_js(setpar, envir=parent.frame())
    do.call(surv_fun, plotArgs)
    do.call("PlotExtras", list())
  }
  else
    do.call("latexPlot", merge(list(lpFun=surv_fun, caption=caption, extra=PlotExtras, translateLatex=caption_translate, setpar=setpar), plotArgs))
}


## Degrees of freedom; probably only necessary for 'survdiff' objects.
#' @export
d_free  <- function(x, ...)
  UseMethod("d_free")


#' @export
d_free.survdiff <- function(x, ...) # Based on code from Terry Therneau's 'print.survdiff()'.
{
  if (!inherits(x, "survdiff"))
    stop("Object is not the result of survdiff.")

  degreesOfFreedom <- NULL

  if (length(x$n) == 1)
    degreesOfFreedom <- 1
  else {
    if (is.matrix(x$obs))
      etmp <- apply(x$exp, 1, sum)
    else
      etmp <- x$exp
    degreesOfFreedom <- (sum(1 * (etmp > 0))) - 1
  }

  return (degreesOfFreedom)
}
