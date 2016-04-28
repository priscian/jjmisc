#' @importFrom RCurl getURL
has_internet = function(hostname="www.google.com")
{
  try(is.character(RCurl::getURL(hostname))) == TRUE
}


#' @export
#' @importFrom BiocInstaller biocLite
bioc_lite <- function(..., local=FALSE)
{
  rm(biocLite, envir=.GlobalEnv)

  if (.has_internet()) {
    cat("Downloading 'biocLite()' from source....\n")
    source("https://www.bioconductor.org/biocLite.R")
  } else {
    cat("No Internet connection. Trying to load package \"BiocInstaller\".\n")
    library(BiocInstaller)
  }

  if (!local)
    biocLite(...)
  else {
    libPath <- Sys.getenv("R_LIBS_USER")
    biocLite(..., lib=libPath, lib.loc=libPath, instlib=libPath, INSTALL_opts=c("--no-clean-on-error"))
  }
}


#' @export
reload_all <- function(package_name, ..., export_all=FALSE, redocument=FALSE)
{
  require(devtools)

  currentwd <- getwd()
  switchArgs <- list(
    EXPR = package_name,
    "." # Default option.
  )
  switchArgs <- modifyList(switchArgs, getOption("reload_all_package_dirs"))
  packagewd <- do.call(switch, switchArgs)

  devtools::load_all(packagewd, export_all=export_all, ...)
  if (redocument)
    devtools::document(packagewd)

  return (nop())
}
## usage:
# reload_all("climeseries", redocument=TRUE)
# reload_all("jjmisc", redocument=TRUE)
