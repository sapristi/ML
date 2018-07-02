

df.make.empty <- function(nrow, colnames=c(), stringsAsFactors=TRUE) {
  ncol <- length(colnames)
  df <- data.frame(matrix(vector(), nrow, ncol,
                        dimnames=list(c(), colnames)),
                        stringsAsFactors=stringsAsFactors)
  return(df)
}

df.make.from_df <- function(df, colnames, stringsAsFactors=TRUE) {
    r.df <- df.make.empty(nrow(df), colnames, stringsAsFactors)
    for (colname in colnames) {r.df[,colname] <- df[,colname]}
    return(r.df)
}
