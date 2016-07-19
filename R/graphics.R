###

#' @export
scale_color_brewer_recycle <- function (..., type="seq", palette=1L, direction=1L)
{
  discrete_scale(aesthetics="colour", scale_name=palette, palette=brewer_pal_recycle(type, palette, direction), ...)
}

#' @export
scale_colour_brewer_recycle <- scale_color_brewer_recycle


#' @export
brewer_pal_recycle <- function (type="seq", palette=1L, direction=1L)
{
  pal <- scales:::pal_name(palette, type)

  function(n) {
    suppressWarnings(cols <- RColorBrewer:::brewer.pal(n, pal))
    pal <- rep(cols, length.out=n)
    if (direction == -1L)
      pal <- rev(pal)

    return (pal)
  }
}

## N.B. This is assigned in "zzz.R". I might want to create an 'environment()' here for package variables instead.
#.latex_plot_counter <- 1L

#' @export
get_latex_plot_counter <- function()
{
  .latex_plot_counter
}

#' @export
increment_latex_plot_counter <- function()
{
  .latex_plot_counter <<- .latex_plot_counter + 1L
}

#' @export
reset_latex_plot_counter <- function()
{
  .latex_plot_counter <<- 1L
}


#' @export
latex_plot  <- function(x, ...)
  UseMethod("latex_plot")


#' @export
latex_plot.default <- function(x, ..., lp_fun=plot, devices=list(pdf=list(height=8.27, width=11.69), emf=list(ext="emf"), png=list(height=8.27, width=11.69)), cap="", short_caption=cap, path_base="image", graphics_path="./images/", latex, translate_latex=TRUE, create_plots=TRUE, float_placement="!ht", graphics_width="\\linewidth", graphics_height=NULL, set_par=NULL, print_after=FALSE, callback=NULL)
{
  caption_ <- cap

  if (missing(latex)) { # If not missing, make sure replacement uses the same "@@...@@" placeholders as below.
    latex <- "  %%
  \\begin{figure}[" %_% float_placement %_% "]
  \\begin{center}
    \\includegraphics[width=" %_% graphics_width %_% ifelse(is.null(graphics_height), "", ", height=" %_% graphics_height) %_% ", keepaspectratio]{@@FILEPATH@@}
  \\end{center}
  \\caption[@@SHORTCAPTION@@]{@@CAPTION@@\\label{fig:@@LABEL@@}}
  \\end{figure}"
  }

  figNum <- sprintf("%04d", get_latex_plot_counter())
  pathName <- graphics_path %_% path_base %_% "-" %_% figNum

  if (create_plots) {
    for (d in names(devices)) {
      argList = devices[[d]]
      ext = argList$ext
      if (is.null(ext)) ext <- d
      argList$ext = NULL
      filePath = paste(pathName, ext, sep='.')
      argList$file <- filePath

      do.call(d, argList)
      if (!is.null(set_par)) eval_js(set_par, envir=parent.frame())
      if (!is.null(lp_fun)) { if (!print_after) lp_fun(x, ...) else print(lp_fun(x, ...)) }
      if (!is.null(callback)) eval_js(callback, envir=parent.frame())
      dev.off()
    }
  }

  ## Create LaTeX output.
  temp <- latex
  temp <- sub("@@CAPTION@@", gsub("\\\\", "\\\\\\\\", ifelse(translate_latex, Hmisc::latexTranslate(caption_, greek=TRUE), caption_)), temp)
  temp <- sub("@@SHORTCAPTION@@", gsub("\\\\", "\\\\\\\\", ifelse(translate_latex, Hmisc::latexTranslate(short_caption, greek=TRUE), caption_)), temp)
  temp <- sub("@@FILEPATH@@", basename(file_path_sans_ext(pathName)), temp)
  temp <- sub("@@LABEL@@", basename(file_path_sans_ext(pathName)), temp)

  cat(temp, "\n\n", sep='')

  increment_latex_plot_counter()

  return (nop())
}


## Get cardinal-point coordinates from plot device.
#' @export
get_cardinal_point <- function(cardinal_position="center", margin_x_percent=0, margin_y_percent=margin_x_percent, as_list=TRUE)
{
  cp <- cardinal_position

  marginXWidth <- (par("usr")[2] - par("usr")[1]) * (margin_x_percent / 100)
  marginYHeight <- (par("usr")[4] - par("usr")[3]) * (margin_y_percent / 100)
  top <- par("usr")[4] - marginYHeight
  right <- par("usr")[2] - marginXWidth
  bottom <-  par("usr")[3] + marginYHeight
  left <- par("usr")[1] + marginXWidth
  centerX <- left + (right - left) / 2
  centerY <- bottom + (top - bottom) / 2

  coordinates <- switch(cp,
    n =,
    north = c(x=centerX, y=top),
    ne =,
    northeast = c(x=right, y=top),
    e =,
    east = c(x=right, y=centerY),
    se =,
    southeast = c(x=right, y=bottom),
    s =,
    south = c(x=centerX, y=bottom),
    sw =,
    southwest = c(x=left, y=bottom),
    w =,
    west = c(x=left, y=centerY),
    nw =,
    northwest = c(x=left, y=top),
    c =,
    center = c(x=centerX, y=centerY),
    c(x=centerX, y=centerY) # default option
  )

  if (as_list)
    coordinates <- as.list(coordinates)

  return (coordinates)
}


#' @export
get_cp_coords <- function(...)
{
  coordinates <- get_cardinal_point(...)

  return (xy.coords(coordinates$x, coordinates$y))
}

#' @export
cp_coords <- get_cp_coords
