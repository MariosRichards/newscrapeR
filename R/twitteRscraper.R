TwitterCitation <- setRefClass("TwitterCitation",
                                
          fields = list(twitteRobj = "list",
                        url = "character",
                        fixed_url = "character",
                        source_name = "character",
                        article_title = "character",
                        created_at = "POSIXct",
                        last_updated ="Date",
                        all_fetched = "logical",
                        tweetsDF = "data.frame"
                        ),
                              
          methods = list( initialize = function(source_name,url,tweetsDF = data.frame(), all_fetched = FALSE,
                                                 created_at = Sys.time(),article_title, last_updated, oldTwitterCitation = NULL)
                              {
                              if (is.null(oldTwitterCitation))
                                {
                                .self$source_name = source_name;
                                .self$url = url;
                                .self$tweetsDF = tweetsDF;
                                .self$created_at = created_at;
                                .self$article_title = article_title;
                                .self$last_updated = last_updated;
                                .self$all_fetched = all_fetched;                           
                                }
                                else
                                  {
                                    .self$source_name = oldTwitterCitation$source_name;
                                    .self$url = oldTwitterCitation$url;
                                    .self$tweetsDF = oldTwitterCitation$tweetsDF;
                                    .self$created_at = oldTwitterCitation$created_at;
                                    .self$article_title = oldTwitterCitation$article_title;
                                    .self$last_updated = oldTwitterCitation$last_updated;
                                    .self$fixed_url = oldTwitterCitation$fixed_url
                                    .self$all_fetched = oldTwitterCitation$all_fetched;
                                  }
                              
                              },
                          
                          fetch_url_via_YQL  = function(query, format="json",yql_address="http://query.yahooapis.com/v1/public/yql/")
                            {
                              final_url <- URLencode(paste(yql_address,"?q=",query,"&format=",format,"&diagnostics=true",sep=""))
                              final_url2 <- url(final_url)
                                                          
                              j <- 1
                              while (j < 2){
                                jsonObj <- try(readLines(con=final_url2, warn = FALSE))
                                if (class(jsonObj) == "try-error") {
                                  next
                                } else {
                                  j <- j + 1
                                }
                              }
                              
                              print(jsonObj)
                              on.exit(close(final_url2))
                              
                              require(rjson)
                              ret <- fromJSON(jsonObj)
                              return(ret)
                            },
                            
                          fix_url = function()
                            {
                            
                            query = paste('select * from html where url="',.self$url,'" and xpath ="/query/diagnostics"',sep="")
 
                            diagnosticObj <- fetch_url_via_YQL(query=query)
                            redirect <- diagnosticObj$query$diagnostics$redirect

                            if (is.null(redirect)) result <- diagnosticObj$query$diagnostics$url[["content"]]
                            else
                              {
                                if (is.null(redirect$content)) 
                                {
                                  last_redirect <- redirect[[length(redirect)]]
                                  result <- last_redirect[["content"]]
                                }
                                else  result <- redirect[["content"]]
                                
                              }
                            if (!is.null(result))
                            .self$fixed_url <- result
                            },
                           
                          grabTweets = function()
                          {
                            require(twitteR)
                            temp_url = .self$fix_url()
                            if (!is.null(temp_url))
                            {
                              .self$fixed_url <- temp_url
                              grabbed <- try(searchTwitter(.self$fixed_url, n = 800))                
                              already_fetched_ids <- vector() 
                              
                              for (i in 1:nrow(.self$tweetsDF))
                                {
                                already_fetched_ids[i] <- .self$tweetsDF[[i,"id"]] 
                                }
                              print(grabbed)
                              if (length(already_fetched_ids) == 0) 
                                if(!inherits(grabbed,"try-error")) 
                                  {
                                   if(length(grabbed)>0) .self$tweetsDF = twListToDF(grabbed)
                                   else .self$tweetsDF = data.frame();
                                   .self$last_updated = Sys.Date();
                                  }
                              else cat("Tweets couldn't be fetched for Citation object");
                            }
                          },
                          
                          show = function()
                            {
                            cat("A Citation Object. \n")
                            cat("Article URL:",.self$url,"\n")
                            cat("Number of Tweets:",nrow(.self$tweetsDF))
                              
                            }
                            
                       
                         )
                    
);

CitationContainer = setRefClass("CitationContainer",
                                 
    fields = list( citation_list = "list",
                   pending_articles = "list",
                   source_name = "character",
                   last_added ="Date",
                   newscrapeR = "ANY",
                   con = "ANY",
                   db_name = "character"
                  ),
                                 
    methods = list( 
      
                    create_initial_citation_list = function()
                        {
                        
                          return_list <- list()
                          no_articles <- length(.self$pending_articles)
                          for (i in 1:no_articles)
                          { 
                             article_url <- .self$pending_articles[[i]]$url;
                             article_title <- .self$pending_articles[[i]]$title
        
                             new_citation <- new("TwitterCitation", source_name = .self$pending_articles[[i]]$published_in, 
                                                 article_title = .self$pending_articles[[i]]$title, 
                                                 url = .self$pending_articles[[i]]$url, last_updated = .self$pending_articles[[i]]$load_date) 
                            
                            return_list[length(return_list)+1] <- new_citation;
                           }  
                         
                         .self$citation_list <- return_list;
                        },
                    
                    add_citations = function()
                      {
                      newscrapeRobj <- .self$newscrapeR
                      art_list <- articles(newscrapeRobj, sources = .self$source_name)
                      ReadArticlesURLs <- lapply(.self$citation_list, function(x) x$url)
                      
                      null_list <- list()
                      
                      for (i in 1:length(art_list))
                        {
                        if (!art_list[[i]]$url %in% ReadArticlesURLs) 
                          null_list <- append_list(null_list, art_list[[i]]) 
                        }
                      
                      .self$pending_articles <- null_list
                      return_list <- list()
                      no_articles <- length(.self$pending_articles)
                     
                      if (no_articles > 0)
                      {
                      for (i in 1:no_articles)
                        { 
                          article_url <- .self$pending_articles[[i]]$url;
                          article_title <- .self$pending_articles[[i]]$title
                          
                          new_citation <- new("TwitterCitation", source_name = .self$pending_articles[[i]]$published_in, 
                                              article_title = .self$pending_articles[[i]]$title, 
                                              url = .self$pending_articles[[i]]$url, last_updated = .self$pending_articles[[i]]$load_date) 
                          
                          return_list[length(return_list)+1] <- new_citation;
                        }  
                      .self$citation_list <- c(.self$citation_list, return_list);
                      .self$pending_articles <- list();
                      .self$last_added <- Sys.Date();
                      }
                      
                      },
                    
                    SQL_init = function()
                      {
                      
                      require(RSQLite)              
                      driver <- dbDriver("SQLite") 
                      .self$con <- dbConnect(driver, .self$db_name)
                      dbGetQuery(.self$con,"PRAGMA ENCODING = \"UTF-8\";")
                                       
                      if (!"TwitterCitation" %in% dbListTables(con))
                        {                        
                          create_str <- "CREATE TABLE TwitterCitation(Id INTEGER PRIMARY KEY,
                          article_title VARCHAR(512),
                          url VARCHAR(512), 
                          fixed_url ARCHAR(512), 
                          created_at INTEGER,
                          source_name CHAR(80),
                          last_updated INTEGER,
                          all_fetched INTEGER 
                          )"
                                                         
                          dbSendQuery(.self$con, create_str)
                          
                          create_str_tweet <- "CREATE TABLE Tweet(Id INTEGER PRIMARY KEY,
                                              text CHAR(150),
                                              favorited INTEGER,
                                              replyToSN VARCHAR(20),
                                              created INTEGER,
                                              truncated INTEGER,
                                              replyToSID  VARCHAR(20),
                                              twitterID VARCHAR(20),
                                              replyToUID VARCHAR(20),
                                              statusSource VARCHAR(128),
                                              screenName  VARCHAR(20),
                                              retweetCount INTEGER,
                                              retweeted INTEGER,
                                              longitude DOUBLE PRECISION,
                                              latitude  DOUBLE PRECISION,
                                              TwitterCitationId  INTEGER,
                                              FOREIGN KEY(TwitterCitationId) REFERENCES TwitterCitation(Id)
                          )"
                          
                          dbSendQuery(.self$con, create_str_tweet)
                        }
   
                      
                      },
                    
                    dump_all = function()
                      {
                      Citations <- .self$citation_list
                      
                      for (i in 1:length(Citations))
                        {
                        if (Citations[[i]]$all_fetched == TRUE)
                        .self$dump_citation(Citations[[i]]) 
                        }
                      },
                    
                    dump_citation = function(citationObj)
                      {
                      
                      require(RSQLite)
                      
                      if (!isIdCurrent(.self$con))
                        {
                        driver <- dbDriver("RSQLite")
                        .self$con <- dbConnect(driver, .self$db_name)
                        }
                          
                        enc <- function(x) 
                         {
                            x <- gsub("'", "''", x) 
                            x
                         }
                          
                      var_sql <- "INSERT INTO TwitterCitation(article_title,url,fixed_url, created_at,source_name,last_updated,all_fetched) VALUES('"
                      var_sql <- paste(var_sql, enc(citationObj$getArticle_title()),"',",sep="")
                      var_sql <- paste(var_sql, "'", enc(citationObj$getUrl()),"',",sep="")
                      var_sql <- paste(var_sql, "'", enc(citationObj$getFixed_url()),"',",sep="")
                      
                      created_at <- as.numeric(as.POSIXlt(citationObj$getCreated_at()))
                      var_sql <- paste(var_sql, as.integer(created_at), ",", sep="")   
                      
                      var_sql <- paste(var_sql,"'",enc(citationObj$getSource_name()),"',",sep="")
                      
                      last_updated <- as.numeric(as.POSIXlt(citationObj$getLast_updated()))
                      var_sql <- paste(var_sql, as.integer(last_updated), ",", sep="") 
                          
                      var_sql <- paste(var_sql, as.integer(citationObj$getAll_fetched()),")",sep="")
                          
                      res <- dbSendQuery(.self$con, var_sql)  
                      dbClearResult(res)
                      .self$dump_tweets(citationObj=citationObj)
                                                                 
                      },
                    
                    getCurrentIndex = function()
                      {
                        require(RSQLite)
                        
                        if (!isIdCurrent(.self$con))
                        {
                          driver <- dbDriver("RSQLite")
                          .self$con <- dbConnect(driver, .self$db_name)
                        }
                        
                        fetchID_SQL <- "SELECT * FROM TwitterCitation WHERE Id = (SELECT MAX(Id) FROM TwitterCitation)"
                        
                        res <- dbSendQuery(.self$con, fetchID_SQL)
                        res2 <- fetch(res)  
                        dbClearResult(res)
                        return(res2$Id)
                      },
                    
                    dump_tweets = function(citationObj)
                      {
                                     
                      Tweets <- citationObj$tweetsDF
                      
                      if (nrow(Tweets)>0)
                        {
                        CurrentIndex <- .self$getCurrentIndex()  
                        
                        enc <- function(x) 
                        {
                          x <- gsub("'", "''", x) 
                          x
                        }
                        
                        
                        integer_null <- function(obj)
                          {
                            if (is.null(obj)) ret <- -1
                            else ret <- obj
                            return(as.integer(ret))
                          }
                        
                        double_null <- function(obj)
                          {
                            if (is.null(obj) || is.na(obj)) ret <- -999
                            else ret <- obj
                            return(ret)
                          }
                        
                        var_sql <- "INSERT INTO Tweet(text,replyToSN,replyToSID, created,twitterID,replyToUID,statusSource,screenName,favorited,truncated,retweetCount,retweeted,longitude,latitude,TwitterCitationId) VALUES('"
                        var_sql <- paste(var_sql, enc(Tweets$text),"',",sep="")
                        var_sql <- paste(var_sql, "'", enc(Tweets$replyToSN),"',",sep="")
                        var_sql <- paste(var_sql, "'", enc(Tweets$replyToSID),"',",sep="")
                        
                        created <- as.numeric(as.POSIXlt(Tweets$created))
                        var_sql <- paste(var_sql, as.integer(created), ",", sep="")   
                                         
                        var_sql <- paste(var_sql,"'",enc(Tweets$twitterID),"',",sep="")   
                        var_sql <- paste(var_sql, "'", enc(Tweets$replyToUID),"',",sep="")
                        var_sql <- paste(var_sql, "'", enc(Tweets$statusSource),"',",sep="")                  
                        var_sql <- paste(var_sql, "'", enc(Tweets$screenName),"',",sep="")                    
  
                        var_sql <- paste(var_sql, integer_null(Tweets$favorited),",",sep="")                    
                        var_sql <- paste(var_sql, integer_null(Tweets$truncated),",",sep="")                    
                        var_sql <- paste(var_sql, integer_null(Tweets$retweetCount),",",sep="")
                        var_sql <- paste(var_sql, integer_null(Tweets$retweeted),",",sep="")
                   
                        var_sql <- paste(var_sql, double_null(Tweets$longitude),",",sep="")
                        var_sql <- paste(var_sql, double_null(Tweets$latitude),",",sep="")
                                                 
                        var_sql <- paste(var_sql, CurrentIndex,")",sep="")
                        
                        print(var_sql)
                        for (i in 1:length(var_sql))
                          {
                          res <- dbSendQuery(.self$con, var_sql[i])  
                          dbClearResult(res)
                          }
                        }
                      },
                                     
                    initialize = function(newscrapeRobj, oldContainer = NULL, source_name,
                                          db_name = "tweets.db")
                      {
                                        
                         if (is.null(oldContainer))
                          {
                            require(newscrapeR)
                            art_list <- articles(newscrapeRobj,sources=source_name);
                            
                            .self$db_name <- db_name
                            .self$SQL_init()
                            
                            .self$pending_articles <- art_list;
                            .self$create_initial_citation_list();
                            .self$last_added <- Sys.Date()
                            .self$source_name <- source_name
                            
                            # set the list of pending articles back to empty state
                            .self$pending_articles <- list();              
                            .self$newscrapeR <- newscrapeRobj
                           }
                         else
                         { 
                         .self$db_name <- db_name # change later to old object
                         .self$SQL_init()
                         
                         .self$pending_articles <- oldContainer$pending_articles;
                         tmp_citation_list <- oldContainer$citation_list
                         for (i in 1:length(tmp_citation_list))
                           {
                           tmp_citation_list[[i]] <- migrate.TwitterCitation(oldTwitterCitation = tmp_citation_list[[i]]) 
                           }
                         .self$citation_list <- tmp_citation_list;
                         .self$last_added <- oldContainer$last_added;
                         
                         if (is.null(newscrapeRobj))
                           {
                           .self$newscrapeR <- oldContainer$newscrapeR
                           }
                         else
                           {
                             .self$newscrapeR <- newscrapeRobj  
                           }
                         
                         .self$source_name <- oldContainer$source_name
                  
                         }
                      
                      },
                    
                    show = function()
                    {
                      cat("Container of Citation Tweets. \n")
                      
                      tweetno <- 0
                      for (i in 1:length(.self$citation_list))
                        {
                        tweetno <- tweetno + nrow(.self$citation_list[[i]]$tweetsDF);
                        }
                      
                      cat("Total Number of Tweets: ")
                      print(tweetno)
                    }
                          
                  )
                                                          
);


# -------------------------- Accessor Functions for TwitterCitation ------------------------#

TwitterCitation$accessors(c("article_title","url","fixed_url"))
TwitterCitation$accessors(c("created_at","source_name"))
TwitterCitation$accessors(c("last_updated","all_fetched"))

#################### Twitter User List ########################## 

UserContainer = setRefClass("UserContainer",
                            fields = list( user_list = "list",
                                           pending_users = "vector",
                                           included_users = "vector"
                              ),
                            methods = list (
                              getUserIDs = function(CitationContainer)
                                  {
                                  if (!inherits(CitationContainer, "CitationContainer")) 
                                  stop("CitationContainer must be of class 'CitationContainer'")
                                  else
                                    {
                                    cit_list <- CitationContainer$citation_list 
                                    user_names <- character()   
                                    for (i in 1:length(cit_list))
                                      {
                                      user_names <- c(user_names,cit_list[[i]]$tweetsDF$screenName)
                                      }
                                    
                                    # remove users which appear more than once and 
                                    # leave only one instance of the respective users
                                    user_names <- user_names[!duplicated(user_names)]
                                    }
                              
                                  .self$pending_users <- user_names
                                  },
                              
                              getPendingUsers = function()
                                {
                                pending <- .self$pending_users
                                pending_unresolved <- character()
                                
                                stopping = FALSE

                                    while (stopping == FALSE)
                                    {
                                      if (length(pending)>=50) 
                                        
                                          {
                                          res <- try(expr = lookupUsers(users = pending[1:50]),)
                                          if (class(res) != "try-error")
                                              {
                                            lapply(X=res, FUN = function(x)
                                                  {
                                                  screenName <- x$getScreenName()
                                                  
                                                  if (!screenName %in% .self$included_users) 
                                                      {
                                                        .self$user_list[[length(.self$user_list)+1]] <- x
                                                        .self$included_users <- c(.self$included_users, screenName)
                                                      }
                                                  })
                                            pending <- pending[-(1:50)]
                                              }
                                          else 
                                            {
                                            pending_unresolved <- c(pending_unresolved, pending[1:50]) 
                                            pending <- pending[-(1:50)]
                                            }
                                          
                                          }
                                          else
                                          {
                                            stopping <- TRUE;
                                          }
                                    }
                            
                                  }
                                
                              
                              
                              )                                                
                            ) 


UserContainer$accessors(c("user_list","pending_users","included_users"));


#################################################################

CitationContainer$accessors(c("citation_list","pending_articles"));

CitationContainer <- function(newscrapeRobj, source_name)
{
  if (!inherits(newscrapeRobj, "newscrapeR")) 
    stop("newscrapeRobj must be of class 'newscrapeR'")
  
  ret <- new("CitationContainer", newscrapeRobj = newscrapeRobj,
             source_name = source_name)
  ret
}

migrate.CitationContainer <- function(oldContainer, newscrapeRobj = NULL)
{
  if (!inherits(oldContainer, "CitationContainer")) 
    stop("oldContainer must be of class 'CitationContainer'")
  
  ret <- new("CitationContainer",oldContainer = oldContainer, newscrapeRobj = newscrapeRobj)   
  ret
}

migrate.TwitterCitation <- function(oldTwitterCitation)
{
  ret <- new("TwitterCitation",oldTwitterCitation = oldTwitterCitation)   
  ret
}

# -------------------------- S4 Methods for CitationContainer class -------------------------------------#

# show method for CitationContainer object
setMethod("show",signature="CitationContainer", function(object) 
  {
    object$show()
  } 
);

# show method for CitationContainer object
setMethod("summary","CitationContainer", function(object,...) 
  {
    object$summary()
  } 
);

# ---------------------------------- Download Function to Grab Tweets ---------------------------------------#


downloadTweets <-  function(object, n.max = 100) 
  # Twitter at the moment supports 150 unauthenticated calls per hour
{
  
  if (!inherits(object, "CitationContainer")) 
    stop("object must be of class 'CitationContainer'")
  
  j <- 1
  
  for (i in 1:length(object$citation_list))
  {
    
    fetched <- object$citation_list[[i]]$all_fetched
    check_timerange <- object$citation_list[[i]]$created_at > Sys.time() - 604800 &&
                       object$citation_list[[i]]$created_at < Sys.time() - 345600
    if(fetched == FALSE && check_timerange)
    {
      object$citation_list[[i]]$grabTweets()
      j <- j + 1 # increase counter of downloads by one.
      object$citation_list[[i]]$all_fetched <- TRUE
      object$dump_citation(object$citation_list[[i]]) # dump this citation to SQL database
      cat("Test No.:",j)
    }
    
    if (j > n.max) break()
    
  }
  
} 

getTweets<- function(object)
  {
  retlist <- list()
  
  for (i in 1:length(object$citation_list))
    {    
      retlist[[i]] <- object$citation_list[[i]]$tweetsDF
    }
  
  return(retlist)  
  }