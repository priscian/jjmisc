## Encryption functions
## http://blog.revolutionanalytics.com/2015/12/securely-storing-your-secrets-in-r-code.html
## https://github.com/sdoyen/r_password_crypt

## Write encrypted data frame to file.
#' @export
#' @importFrom digest AES
write_aes <- function(d, file_path, key)
{
  zz <- textConnection("out", "w")
  write.csv(d, zz, row.names=FALSE)
  close(zz)

  out <- paste(out, collapse="\n")
  raw <- charToRaw(out)
  raw <- c(raw, as.raw(rep(0L, 16L - length(raw) %% 16L)))
  aes <- AES(key, mode="ECB")
  aes$encrypt(raw)

  writeBin(aes$encrypt(raw), file_path)
}


## Read encypted data frame from file.
#' @export
#' @importFrom digest AES
read_aes <- function(file_path, key)
{
  dat <- readBin(file_path,"raw", n=1000L)
  aes <- AES(key, mode="ECB")
  raw <- aes$decrypt(dat, raw=TRUE)
  txt <- rawToChar(raw[raw > 0L])

  read.csv(text=txt, stringsAsFactors=FALSE)
}


#' @export
save_key <- function(file_path="./key.RData")
{
  key <- as.raw(sample(1:16, 16))
  save(key, file=file_path)
}

## usage:
# source("data/crypt.R")
# save_key("data/key.RData")
# load("data/key.RData")
## Write credentials.
# credentials <- data.frame(login="user_name", password="pass_word", stringsAsFactors=FALSE)
# write_aes(d=credentials, file_path="data/credentials.txt", key=key)
# rm(credentials)
## Read credentials.
# credentials <- read_aes(file_path="data/credentials.txt", key=key)
# print(credentials)
