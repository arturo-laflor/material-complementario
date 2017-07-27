iconvdataframe<-function (df, ...)  # Esta función la encontré por ahí 
{
  df.names <- iconv(names(df), ...)
  df.rownames <- iconv(rownames(df), ...)
  names(df) <- df.names
  rownames(df) <- df.rownames
  df.list <- lapply(df, function(x) {
    if (class(x) == "factor") {
      x <- factor(iconv(as.character(x), ...))
    }
    else if (class(x) == "character") {
      x <- iconv(x, ...)
    }
    else {
      x
    }
  })
  df.new <- do.call("data.frame", df.list)
  return(df.new)
}