fetch_SQL = function(db_name, keywords = character(), sources = vector(),
                           from = NULL, to = NULL)
{
  require(RSQLite)
  
  if (!is.null(from)) from <- as.numeric(as.POSIXlt(from)) 
  if (!is.null(to))   to <- as.numeric(as.POSIXlt(to)) + 1000*60*60*3
  
  driver <- dbDriver("SQLite")
  con <- dbConnect(driver, db_name)

  query_string <-  "SELECT * FROM Article WHERE "
  
  if (length(keywords)>0)
    cond1 <- paste(paste("content LIKE '%",keywords,"%'",sep=""),collapse=" AND ")
  else cond1 <- ""
  
  if (length(sources)>0)
  {
    if(nchar(cond1)>1) cond1 <- paste(cond1," AND ",collapse=" ")
    cond2 <- paste(paste("published_in LIKE '%",sources,"%'",sep=""),collapse=" OR ")
  }
  else cond2 <- ""
  
  if (!is.null(from))
    {
    if (nchar(cond2)>1) cond2 <- paste(cond2," AND ",collapse=" ")
    if (nchar(cond1)>1&&!nchar(cond2)>1) cond1 <- paste(cond1," AND ",collapse=" ")
    cond3 <- paste("load_date >=", from)
    }
  else cond3 <- ""
  
  if (!is.null(to))
    {
    if (nchar(cond3)>1) cond3 <- paste(cond3," AND ",collapse=" ")
    else 
      {
      if (nchar(cond2)>1) cond2 <- paste(cond2," AND ",collapse=" ")
      if (nchar(cond1)>1&&!nchar(cond2)>1) cond1 <- paste(cond1," AND ",collapse=" ")  
      }
    cond4 <- paste("load_date <=", to) 
    }
  else cond4 <- ""
  
  print(cond3)
  print(cond4)
  
  query_string <- paste(query_string, cond1, cond2, cond3, cond4, sep="")
  print(query_string)
  res <- dbGetQuery(conn=con,query_string)
  Encoding(res$content) <- "UTF-8"
  Encoding(res$title) <- "UTF-8"
  res$load_date <- as.Date(unix2POSIXct(res$load_date))
  return(res)
}