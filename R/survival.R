#' @export
latex.coxph <- function(x, caption = NULL, label = NULL, digits = 3, frame_only = FALSE, ...)
{ # V. http://stackoverflow.com/questions/7780666/cox-regression-output-in-xtable-choosing-rows-columns-and-adding-a-confidence
  cox <- x

  ## Prepare the columns.
  beta <- coef(cox)
  se <- sqrt(diag(cox$var))
  p <- get_formatted_p_value(1 - pchisq((beta/se)^2, 1), le = "<")
  ci <- exp(confint.default(cox))

  ## Bind columns together and select desired rows.
  res <- dataframe(beta, se = exp(beta), ci, p)
  colnames(res) <- c("beta", "HR", colnames(ci), "\\(p\\)")

  ## Print results in a LaTeX-ready form.
  x <- xtable(res, caption = caption, label = label, digits = digits)

  if (!frame_only)
    print(x, booktabs = TRUE, sanitize.text.function = function(x) Hmisc::latexTranslate(x, pb = FALSE, greek = TRUE), math.style.negative = TRUE, table.placement = "H", caption.placement = "top", ...)
  else
    return(res)

  return (nop())
}


#' @export
latex.cph <- latex.coxph


#' @export
gcox <- function(f, p, survivalType = "both", subset = NULL, prefix = "c", model = TRUE, surv = TRUE, x = TRUE, y = TRUE, package = "both", ...)
{
  if (!is.list(f) && !inherits(f, "formula"))
    stop("Function argument is not a list or a formula.")

  if (!inherits(p, "pointer"))
    stop("Function argument is not a pointer.")

  temp <- NULL
  for (i in survivalType) {
    if (i == "both") temp <- c(temp, "pfs", "os")
    else temp <- c(temp, i)
  }
  survivalType <- temp

  useRms <- useSurvival <- FALSE
  temp <- substring(trim(package), 1, 3)
  if (temp == "bot")
    useRms <- useSurvival <- TRUE
  else if (temp == "rms")
    useRms <- TRUE
  else if (temp == "sur")
    useSurvival <- TRUE

  formulas <- list() # Or: 'structure(vector(mode = "list", length(survivalType)), names = survivalType)'
  for (i in survivalType) {
    if (inherits(f, "formula")) # Can't use 'ifelse()' here because of incompatible types.
      formulas[[i]] <- f
    else
      formulas[[i]] <- f[[i]]

    environment(formulas[[i]]) <- environment() # Need to change the formula's environment from 'R_GlobalEnv'.
  }

  .subset <- list()
  if (!is.list(subset)) {
    for (i in survivalType)
      .subset[[i]] <- subset
  }
  else
    .subset <- subset

  modelFits <- list()

  for (i in survivalType) {
    survivalTypeAbbreviation <- substring(i, 1, 1)
    survivalVariableNames <- paste(prefix, c("", "h"), survivalTypeAbbreviation, sep = "")

    f <- eval(substitute(update(formulas[[i]], v ~ .), list(v = as.symbol(i))))
    fh <- as.formula(gsub("strata\\(", "strat(", split(f)$as_character))
    #browser()
    if (useSurvival)
      modelFits[[survivalVariableNames[1]]] <- coxph(f, data = ..(p), subset = .subset[[i]], model = model, x = x, y = y, ...)
    if (useRms)
      modelFits[[survivalVariableNames[2]]] <- cph(fh, data = ..(p), subset = .subset[[i]], model = model, surv = surv, x = x, y = y, ...)
  }

  class(modelFits) <- "gcox"

  return (modelFits)
}
## usage:
# gc1 <- gcox(f2, ptr(d), subset = list(pfs = keepRowsLandmarkPfs, os = keepRowsLandmarkOs))


#' @export
print.gcox <- function(x, confint = FALSE, survivalType = "both", tache = "#####", reps = 10, ...)
{
  op <- options("prType"); options(prType = NULL)

  isCph <- logical(length(x))
  for (i in 1:length(x)) {
    if (inherits(x[[i]], "cph"))
      isCph[i] <- TRUE
  }

  temp <- NULL
  for (i in survivalType) {
    if (i == "both") temp <- c(temp, "pfs", "os")
    else temp <- c(temp, i)
  }
  survivalType <- temp
  survivalTypeAbbreviation <- substring(survivalType, 1, 1)

  if (all(isCph)) modelPrintNames <- names(x)
  else modelPrintNames <- names(x)[!isCph]

  for (i in modelPrintNames) {
    if (str_sub(i, -1) %in% survivalTypeAbbreviation) { # Uses "stringr" package
      catn(paste(rep(tache, reps), collapse = ""), suffix = "")
      catn(tache, suffix = "")
      catn(tache %_% " Model:", suffix = "")
      form <- x[[i]]$formula
      if (is.null(form)) form <- as.list(x[[i]]$model)$formula
      catn(tache %_% " " %_% split(form)$as_character, suffix = "")
      catn(tache, suffix = "\n")
      catn(paste(rep(tache, reps), collapse = ""), prefix = "")
      print(x[[i]])
    }
  }

  anovaPrintNames <- names(x)[isCph]
  for (i in anovaPrintNames) {
    if (str_sub(i, -1) %in% survivalTypeAbbreviation) {
      catn(paste(rep(tache, reps), collapse = ""), suffix = "")
      catn(tache, suffix = "")
      catn(tache %_% " Wald Tests:", suffix = "")
      catn(tache %_% " " %_% split(as.list(x[[i]]$model)$formula)$as_character, suffix = "")
      catn(tache, suffix = "\n")
      catn(paste(rep(tache, reps), collapse = ""), prefix = "")
      print(anova(x[[i]]))
    }
  }

  if (confint) {
    for (i in modelPrintNames) {
      if (str_sub(i, -1) %in% survivalTypeAbbreviation) {
        catn(paste(rep(tache, reps), collapse = ""), suffix = "")
        catn(tache, suffix = "")
        catn(tache %_% " Hazard Ratio & Confidence Intervals:", suffix = "")
        form <- x[[i]]$formula
        if (is.null(form)) form <- as.list(x[[i]]$model)$formula
        catn(tache %_% " " %_% split(form)$as_character, suffix = "")
        catn(tache, suffix = "\n")
        catn(paste(rep(tache, reps), collapse = ""), prefix = "")
        print(confint(x[[i]]))
      }
    }
  }

  options(op)

  nop()
}


## Some notes on 'latex.gcox()'.
#
# The object 'gc7$cp' is a "coxph" model fit with parameter values 'model = TRUE, x = TRUE, y = TRUE' included.
#
# 'gc7$cp$model' or 'model.frame(gc7$cp)' returns the model frame, i.e. the relevant data frame with all in-formula operations performed on variables. For RMS models, use 'eval(gc7$chp$model)' (which seems like a hack, but whatever).
#
# all.vars(terms(gc7$cp)) # Basic names of predictor variables used in model
# attr(terms(gc7$cp), "term.labels") # Labels of regressors in model fit
# colnames(model.matrix(gc7$cp)) # Column names for all regressors in design matrix
# gc7$cp$assign # Which columns from 'model.matrix(gc7$cp)' are used for each predictor; also, how many regressors used for each predictor
# attr(terms(gc7$cp), "variables") # 'call' object; can use 'length()' and 'list' subsetting to extract names of predictor variables, but not needed
# attr(terms(gc7$cp), "order") # Corresponds to "term.labels" above; value > 1 is an interaction term
# attr(anova(gc7$chp), "coef.names")
# attr(anova(gc7$chp), "which") # Regressors used in calculating Wald statistics of 'anova.rms()'

#' @export
latex.gcox <- function (x, survivalType = "both", prefix = "c", ...)
{
  temp <- NULL
  for (i in survivalType) {
    if (i == "both") temp <- c(temp, "pfs", "os")
    else temp <- c(temp, i)
  }
  survivalType <- temp

  for (i in survivalType) {
    survivalTypeAbbreviation <- substring(i, 1, 1)
    svn <- survivalVariableNames <- paste(prefix, c("", "h"), survivalTypeAbbreviation, sep = "")

    specials <- as.vector(unlist(attr(terms(x[[svn[1]]]), "specials")))
    factors <- attr(terms(x[[svn[1]]]), "factors")
    allPredictors <- apply(factors, 1, function(a) { names(which(a > 0)) })
    allRegressors <- sapply(allPredictors, function(a) { as.vector(unlist(x[[svn[1]]]$assign[a])) })
    allRegressors <- allRegressors[!sapply(allRegressors, is.null)] # Handles all non-interaction terms
    regressorOrder <- structure(attr(terms(x[[svn[1]]]), "order"), names = attr(terms(x[[svn[1]]]), "term.labels"))

    allPredictors <- allPredictors[sapply(allPredictors, length) > 0]
    allPredictorsRms <- sapply(allPredictors,
      function(a1) {
        sapply(a1,
          function(a2) {
            temp <- strsplit(a2, ":")[[1]]
            temp <- sapply(temp, function(a3) { all.vars(as.formula("~" %_% a3)) })
            paste(temp, collapse = " * ")
          }
        )
      }
    )

    temp <- sort(unique(regressorOrder))
    interactionLevels <- subset(temp, temp > 1) # Interactions have order > 1
    interactionFactors <- factors[, regressorOrder %in% interactionLevels]
    omnibusInteractionFactors <- list()
    for (j in colnames(interactionFactors)) {
      temp <- NULL
      for (k in colnames(interactionFactors)) {
        if (sum(bitAnd(interactionFactors[, j], interactionFactors[, k])) == sum(interactionFactors[, j]))
          temp <- c(temp, k)
      }
      if (length(temp) > 0)
        omnibusInteractionFactors[[j]] <- temp
    }
    wald <- suppressWarnings(anova(x[[svn[2]]]))
    waldWhich <- attr(wald, "which")

    for (j in names(omnibusInteractionFactors))
      allRegressors[[j]] <- as.vector(unlist(x[[svn[1]]]$assign[omnibusInteractionFactors[[j]]]))

    omnibusPValues <- sapply(allRegressors,
      function(a) {
        rv <- NULL
        for (j in 1:length(waldWhich)) {
          if (length(a) == length(waldWhich[[j]]) && a == waldWhich[[j]]) {
            rv <- wald[j, "P"]
            break
          }
        }

        return (rv)
      }
    )

    regressorVariablesRms <- sapply(names(allRegressors), function(a) { all.vars(as.formula("~" %_% a)) })
    regressorVariablesRmsNames <- sapply(regressorVariablesRms, paste, collapse = " * ") # Also see 'gc7$chp$Design$name'

    regressorVariables <- sapply(names(allRegressors), function(a) { strsplit(a, ":")[[1]] })

    regressorLabels <- sapply(names(regressorVariables),
      function(a) {
        rv <- NULL
        for (j in regressorVariables[[a]]) {
          modelLabel <- label(x[[svn[1]]]$model[[j]])
          rv <- c(rv, ifelse(is.null(modelLabel), j, modelLabel))
        }
        rv <- paste(rv, collapse = " * ")

        return (rv)
      }
    )
    regressorUnits <- sapply(names(regressorVariables),
      function(a) {
        rv <- NULL
        if (length(regressorVariables[[a]]) > 1)
          return (rv)
        for (j in regressorVariables[[a]]) {
          modelUnits <- units(x[[svn[1]]]$model[[j]])
          rv <- c(rv, ifelse(is.null(modelUnits), j, modelUnits))
        }

        return (rv)
      }
    )

    ## TODO: Use 'gc7$chp$Design$nonlinear'? Match booleans to 'gc7$chp$assign' via 'allPredictorsRms' and get the p-value from 'waldWhich' object?
    nonlinears <- x[[svn[2]]]$Design$nonlinear
    #nonlinears <- nonlinears[sapply(nonlinears, any)]

    nonlinearPredictors <- sapply(names(allPredictorsRms),
      function(a1) {
        p <- allPredictorsRms[[a1]]
        flags <- as.vector(sapply(p, function(a2) { nonlinears[[a2]] }))
        indices <- as.vector(sapply(names(p), function(a2) { x[[svn[2]]]$assign[[a2]] }))

        rv <- as.vector(unlist(indices)[unlist(flags)])
        if (is.invalid(rv)) return (NA)

        return (sort(rv))
      }
    )

    #browser()
  }

  # x[[svn[1]]]$y[, "status"] # Survival event; may need 'as.vector()'ing
  # x[[svn[1]]]$model$raceReduced # E.g.; model-variable values (no need to deal with 'subset's)
}


#' @export
surv_brief <- function(f, x, survival_type="both", subset=NULL, summary...=NULL, survfit_frame...=FALSE)
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
    do.call("latex_plot", merge(list(lpFun=surv_fun, caption=caption, extra=PlotExtras, translateLatex=caption_translate, setpar=setpar), plotArgs))
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
