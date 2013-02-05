TwitterCitation <- setRefClass("TwitterCitation",
                                
          fields = list(twitteRobj = "list",
                        url = "character",
                        fixed_url = "character",
                        source_name = "character",
                        article_title = "character",
                        created_at = "POSIXct",
                        last_updated ="Date",
                        all_fetched = "logical",
                        tweetsDF = "data.frame",
                        source_name = "character"
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
                                    .self$created_at = created_at;
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
                   last_added ="Date",
                   newscrapeR_name = "character"
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
                      
                      newscrapeRobj <- eval(parse(text=.self$newscrapeR_name), envir=.GlobalEnv)
                      art_list <- articles(newscrapeRobj, sources = .self$source_name)
                      null_list <- lapply(art_list, function(x) if(x$load_date >= .self$last_added) x)
                      .self$pending_articles <- as.list(unlist(null_list))
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
                                   
                    initialize = function(newscrapeRobj, oldContainer = NULL, newscrapeR_name, source_name)
                      {
                         if (is.null(oldContainer))
                          {
                            require(newscrapeR)
                            art_list <- articles(newscrapeRobj,sources=source_name);
                            
                            .self$pending_articles <- art_list;
                            .self$create_initial_citation_list();
                            .self$last_added <- Sys.Date()
                            .self$source_name <- source_name
                            
                            # set the list of pending articles back to empty state
                            .self$pending_articles <- list();
                            .self$newscrapeR_name <- newscrapeR_name;
                           }
                         else
                         { 
                           
                         .self$pending_articles <- oldContainer$pending_articles;
                         tmp_citation_list <- oldContainer$citation_list
                         for (i in 1:length(tmp_citation_list))
                           {
                           tmp_citation_list[[i]] <- migrate.TwitterCitation(oldTwitterCitation = tmp_citation_list[[i]]) 
                           }
                         .self$citation_list <- tmp_citation_list;
                         .self$last_added <- oldContainer$last_added;
                         .self$newscrapeR_name <- oldContainer$newscrapeR_name
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

CitationContainer$accessors(c("citation_list","pending_articles"));

CitationContainer <- function(newscrapeRobj, source_name)
{
  if (!inherits(newscrapeRobj, "newscrapeR")) 
    stop("newscrapeRobj must be of class 'newscrapeR'")
  
  newscrapeR_name <- deparse(substitute(newscrapeRobj))
  ret <- new("CitationContainer", newscrapeRobj = newscrapeRobj, newscrapeR_name = newscrapeR_name,
             source_name = source_name)
  ret
}

migrate.CitationContainer <- function(oldContainer)
{
  if (!inherits(oldContainer, "CitationContainer")) 
    stop("oldContainer must be of class 'CitationContainer'")
  
  ret <- new("CitationContainer",oldContainer = oldContainer, newscrapeRobj = NULL)   
  ret
}

migrate.TwitterCitation <- function(oldTwitterCitation)
{
  ret <- new("TwitterCitation",oldTwitterCitation = oldTwitterCitation)   
  ret
}

# -------------------------- S4 Methods for CitationContainer class -------------------------------------#

# show method for CitationContainer object
setMethod("show","CitationContainer", function(object) 
  {
    object$show()
  } 
);

# show method for CitationContainer object
setMethod("summary","CitationContainer", function(object) 
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
    if(fetched == FALSE) 
    {
      object$citation_list[[i]]$grabTweets()
      j <- j + 1 # increase counter of downloads by one.
      object$citation_list[[i]]$all_fetched <- TRUE
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