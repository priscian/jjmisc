#' @importFrom RCurl getURL
has_internet = function(hostname="www.google.com")
{
  try(is.character(RCurl::getURL(hostname))) == TRUE
}


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
