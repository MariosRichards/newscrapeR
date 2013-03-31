# turns on development mode
In_Development = FALSE;
MultiCore = TRUE;

# print function only active in development mode  
devprint = function(obj_text)
  {
    if (In_Development == TRUE) print(obj_text)
  }

# appends an object (param: obj) to list (param: list)
append_list <- function(list,obj)
  {
    list[[length(list)+1]]<-obj
    list
  }

# ----------------------------------------- Helper Functions -------------------------------------#

scraping_links_wrapper <- function(active_source)
  {
  require(R.utils)
  evalWithTimeout(active_source$scraping_links(), timeout = 1800, onTimeout="warning")
  # active_source$scraping_links()
  }


remove_empty_articles = function(article_list)
  {
    removable <- vector()
    article.length <- length(article_list)
    
    for (i in 1:article.length) 
    {
      if (article_list[[i]]$empty_content == TRUE | article_list[[i]]$content == "") removable <- c(removable,i)  
    }
    
     if ( length(removable) > 0) returned_list <- article_list[-removable]
          else returned_list <- article_list
            
     return(returned_list)
  }


remove_tags = function(article)
# this function takes as an input argument an article string (in XML format) and removes tags etc. 
# in order to obtain the article content in plain-text format
{
  
  uncleaned_article <- article;
  uncleaned_article <- gsub("<p>","",uncleaned_article);
  uncleaned_article <- gsub("</p>","",uncleaned_article);
  
  # deutsche sonderzeichen checken
  
  regexp_in <- "<"
  regexp_out <- ">"
  
  cond = TRUE
  while (cond)
  {
    start <- regexpr(text=uncleaned_article,pattern=regexp_in) 
    
    if (start == -1) cond = FALSE
    else
    {
      start <- regexpr(text=uncleaned_article,pattern=regexp_in) 
      
      end <- regexpr(text=uncleaned_article,pattern=regexp_out)
      sl <- nchar(uncleaned_article)
      
      uncleaned_article <- paste(substr(uncleaned_article, 1, start-1),substr(uncleaned_article,end+1,sl),sep="")
      uncleaned_article <- gsub("  "," ",uncleaned_article);
      
    }
    
  }
  
  return(uncleaned_article)
}

# ----------------------------------------------- Article --------------------------------------#


article <- setRefClass("Article",
                       fields = list(
                         title = "character",
                         author = "character",
                         abstract = "character",
                         content = "character",
                         url = "character",
                         load_date ="Date",
                         published_in = "character",
                         pub_date = "Date",
                         empty_content = "logical",
                         dumped = "logical"
                       ),
                       
                       methods = list(
                         initialize = function(title=as.character(NA), author=as.character(NA), 
                                               abstract=as.character(NA), content, url,load_date=Sys.Date(),
                                               pub_date=as.Date(NA),published_in,empty_content=FALSE,
                                               dumped=FALSE)
                         {
                           .self$title <- title;
                           .self$abstract <- abstract;
                           .self$author <- author;
                           .self$content <- content;
                           .self$url <- url;
                           .self$load_date <- load_date;
                           .self$pub_date <- pub_date;
                           .self$published_in <- published_in;
                           .self$empty_content <- empty_content;
                           .self$dumped <- dumped;
                           
                         },
                         
                         show = function()
                         {
                           
                           cat("---------------------------------------------------------- \n")
                           cat("Date: ",as.character(.self$load_date),", Source:",.self$published_in,"\n")
                           cat("Title: \n")
                           cat(as.character(.self$title))
                           cat("\n ---------------------------------------------------------- \n ")
                           cat(.self$content)
                           cat("\n ---------------------------------------------------------- \n ")
                         },
                         
                         summary = function()
                         {
                           
                           cat("------------------------------------------------------------- \n")
                           cat("Date: ",as.character(.self$load_date),"\n")
                           cat("Author:",.self$author,"\n")
                           cat("Source:",.self$published_in,"\n")
                           cat("URL:",.self$url,"\n")
                           cat("Title: \n")
                           cat(as.character(.self$title))
                           cat("\n ---------------------------------------------------------- \n ")
                           cat(.self$content)
                           cat("\n ---------------------------------------------------------- \n ")
                           
                         }
                                             
                         
                       )
                       
                       
)

# -------------------------- Accessor Functions for Article------------------------#

article$accessors(c("title","author"))
article$accessors(c("abstract","content"))
article$accessors("url")
article$accessors(c("load_date","pub_date"))
article$accessors("published_in")
                  
# ---------------------------------------- Source ---------------------------------#

NewsSource <- setRefClass("Source",
                    fields = list(
                      
                      name      = "character",
                      query     = "character",
                      article_query = "character",
                      aliases    = "vector",
                      url       = "character",
                      rss       = "logical",
                      updated_at = "POSIXct",
                      language = "character",
                      
                      cleansing = "logical",
                      
                      article_links = "list",
                      articles = "list",
                      article_xpath = "character",
                      pbar          = "ANY",
                      parent = "ANY"
                      
                    ),
                    
                    
                    
                    #--------------------
                    methods = list(
                      
                      show = function()
                      {
                        cat("----------- Source --------------\n")
                        cat("Name: ",.self$name,"\n");
                        cat("URL: ",.self$url,"\n")
                        cat("No. of Articles: ",length(.self$articles),"\n")
                        cat("Last updated at:", as.character(.self$updated_at),"\n")
                        cat("---------------------------------\n")
                      },
                      
                      summary = function()
                      {
                        cat("----------- Summary of Source --------------\n")
                        cat("Name: ",.self$name,"\n");
                        cat("Aliases: ",.self$aliases,"\n");
                        cat("URL: ",.self$url,"\n")
                        cat("No. of Articles: ",length(.self$articles),"\n")
                        cat("Last updated at:", as.character(.self$updated_at),"\n")
                        
                        if (.self$rss == TRUE) rssprint <- "Yes"
                        else rssprint <- "No"
                        
                        cat("RSS:",rssprint,"\n")
                        
                        cat("article_query:",.self$article_query,"\n")
                        cat("query:",.self$query,"\n")
                        cat("article_xpath:",.self$article_xpath,"\n")
                        cat("---------------------------------\n")
                        
                      },
                      
                                 
                      search = function(keyword)
                        {
                          retlist <- list()
                          regexp_search <- keyword
                          
                          for (i in seq_along(.self$articles))
                          {
                            search_res <- regexpr(text=.self$articles[[i]]$content,pattern=regexp_search,ignore.case=TRUE) 
                            if (search_res != -1)
                              retlist[[length(retlist)+1]] <- .self$articles[[i]]
                          }
                          
                          return(retlist)
                        },
                      
                      select_time_period = function(from, to)
                        {
                          retlist <- list()
  
                          from <- as.Date(x=from,format="%Y-%m-%d")
                          to <- as.Date(x=to,format="%Y-%m-%d")
                          
                          for (i in seq_along(.self$articles))
                          {               
                            if (.self$articles[[i]]$load_date >= from && .self$articles[[i]]$load_date <= to)
                              retlist[[length(retlist)+1]] <- .self$articles[[i]]
                          }
                          return(retlist) 
                        },
                      
                      select_by_keyword_and_time = function(keyword, from, to)
                        {
                        
                        retlist <- list()
                        regexp_search <- keyword
                        if(is.character(from)) from <- as.Date(x=from,format="%Y-%m-%d")
                        if(is.character(to))   to <- as.Date(x=to,format="%Y-%m-%d")
                        
                        for (i in seq_along(.self$articles))
                          {
                            
                            search_res <- regexpr(text=.self$articles[[i]]$content,
                                                  pattern=regexp_search,ignore.case=TRUE)
                            if (search_res != -1 && 
                                .self$articles[[i]]$load_date >= from && 
                                .self$articles[[i]]$load_date <= to)
                              retlist[[length(retlist)+1]] <- .self$articles[[i]]
                          }
                          
                        return(retlist)
                        
                        },
                      
                      initialize = function(query = as.character(NA), url = as.character(NA), 
                                            name = as.character(NA), aliases = vector(), article_xpath = "none", 
                                            cleansing = FALSE, rss = FALSE, old_Source = NULL
                                            )
                        {
                          
                          if(is.null(old_Source))
                            {
                              .self$name <- name;
                              .self$url <- url;
                              .self$query <- query;
                              .self$aliases <- aliases;
                              .self$article_xpath <- article_xpath;
                              .self$rss <- rss;
                              .self$cleansing <- cleansing;   
                              
                            }
                          else
                            {
                              .self$name <- old_Source$name;
                              .self$url <- old_Source$url;
                              .self$query <- old_Source$query;
                              .self$aliases <- old_Source$aliases;
                              .self$article_xpath <- old_Source$article_xpath;
                              .self$rss <- old_Source$rss;
                              .self$cleansing <- old_Source$cleansing;    
                              
                              .self$updated_at <- old_Source$updated_at;
                              .self$articles <- old_Source$articles;
                              .self$article_links <- old_Source$article_links;
                              
                            }
  
                        },
                      
                      
                      query_YQL = function(format="json",yql_address="http://query.yahooapis.com/v1/public/yql/")
                        
                      {
                        final_url <- URLencode(paste(yql_address,"?q=",.self$query,"&format=",format,sep=""))
                        final_url2 <- url(final_url)
                        devprint(final_url2)
                        
                        i <- 1
                        while (i < 2){
                          res <- try(res <- readLines(con=final_url2, warn=FALSE))
                          if (class(res) == "try-error") {
                            next
                          } else {
                            i <- i + 1
                          }
                        }

                        on.exit(close(final_url2))
                        
                        result = .self$json_to_r_link(res);
                        retList <- result$query$results[[1]]
                        
                        if (.self$rss == FALSE)
                          { 
                            .self$fix_href(retList)
                          }
                          else
                            { 
                              .self$store_rss_links(retList)
                            }
                        
                      },
                      
                      store_rss_links = function(list)
                      {
                        total <- length(list)
                        # create progress bar
                        if (total > 0)
                        {
                        .self$pbar <- txtProgressBar(min = 0, max = total, style = 3)  
                      
                        for (i in seq_along(list))
                          {
                            
                            a = .self$check_valid_url( list[[i]]$link);
                            
                            devprint(list[[i]]$link)
                            if ( length(a) == 0) list[[i]]$link = paste(.self$url, list[[i]]$link,sep="");
                            
                            
                            if (.self$check_double (list[[i]] ) == FALSE)
                            {
                              .self$append(list[[i]]);
                              .self$articles[[i]]<-.self$fetch_article(list[[i]]);
                            }
                            
                            # update progress bar
                            setTxtProgressBar( .self$pbar, i)
                                   
                          }
                      
                         devprint(.self$article_links)
                        }
                        else 
                          cat("\n No new articles from ",.self$name," available right now. Try again later.")
                      },
                      
                      # if there is a valid url
                      check_valid_url = function(item)
                      {
                        a =  grep("http://", item)
                        a
                      },
                      
                      
                      
                      fix_href = function(list)
                      {
                        #devprint(str(list));
                        
                        for (i in seq_along(list))
                        {
                          a = .self$check_valid_url( list[[i]][1]);
                          
                          if ( length(a) == 0) list[[i]][1] = paste(.self$url, list[[i]][1],sep="");
                          
                          
                          if (.self$check_double (list[[i]] ) == FALSE)
                          {
                            .self$append(list[[i]]);
                            .self$fetch_articles[[i]]<-.self$fetch_article(list[[i]]);
                          }
                        }
                      },
                      
                      
                      
                      json_to_r_link = function(json_obj)
                      {
                        require(rjson)
                        ret <- fromJSON(json_obj)
                        ret
                      },
                      
                      # appends link to list of article_links
                      append = function(item)
                      {
                        .self$article_links[[length(.self$article_links) + 1]] <- item;
                        .self$article_links[[length(.self$article_links)]]$already_fetched <- FALSE;
                        
                      },
                      
                      # checks whether the link is already stored
                      check_double = function(link)
                      {
                        ret = FALSE;
                        
                        for (i in seq_along(.self$article_links))
                        {
                          item = .self$article_links[[i]][1];
                          if (link[[1]] == item) ret = TRUE;
                        }
                        
                        ret;
                      },
                      
                      
                      # fetches the articles
                      fetch_articles = function()
                      {
                        new_articles_count <- 0
                        fetched_counter <- 0
                        
                        for (i in seq_along(.self$article_links))
                        {
                          if (.self$article_links[[i]]$already_fetched == FALSE) 
                            new_articles_count <- new_articles_count + 1 
                        }
                        
                        if (new_articles_count > 0)
                        {
                        cat("\nFetching Articles from",.self$name,"\n")
                        .self$pbar <- txtProgressBar(min = 0, max = new_articles_count + 1, style = 3)
                                                    
                        for (i in seq_along(.self$article_links))
                        {
                        if (.self$article_links[[i]]$already_fetched == FALSE) 
                            {
                            string1 <- 'select * from html where url="'
                            temp_url <- .self$get_url(.self$article_links[[i]])
                            string2 <- '" and xpath="'
                            xpath <- .self$article_xpath
                            
                            final_string <- paste(string1, temp_url, string2, xpath,'"', sep="")
                            
                            temp_article_string <- articles_YQL(final_string)
                            final_article_string <- .self$clean_article(temp_article_string)
                              
                            temp_title <- .self$article_links[[i]]$title
                            
                            new_article <- new("Article", content = final_article_string, url = temp_url, title = temp_title, published_in=.self$name)
                            
                            .self$articles[[i]] <- new_article
                            
                            # write to SQL database
                            .self$parent$dump_article(new_article)
                            .self$articles[[i]]$dumped <- TRUE;
                            
                            # update progress bar
                            fetched_counter <- fetched_counter + 1;                           
                            setTxtProgressBar( .self$pbar, fetched_counter);
                            
                            if (final_article_string == "") 
                              {
                              .self$articles[[i]]$empty_content <- TRUE;          
                              }
                            
                            .self$article_links[[i]]$already_fetched <- TRUE 
                            }                         
                    
                        }
                        
                        setTxtProgressBar( .self$pbar, fetched_counter + 1);
                        close(.self$pbar)
                        }
                        else cat("\n No new articles from ",.self$name," available right now. Try again later.")
                                  
                      },
                      
                      # fetches a single article
               
                      fetch_article = function(Link)
                      {
                     
                        string1 <- 'select * from html where url="'
                        temp_url <- .self$get_url(Link)
                        string2 <- '" and xpath="'
                        xpath <- .self$article_xpath
                        
                        final_string <- paste(string1, temp_url, string2, xpath,'"', sep="")
                        
                        temp_article_string <- articles_YQL(final_string)
                        final_article_string <- .self$clean_article(temp_article_string)                 
                        
                        temp_title <- Link$title
                        
                        new_article <- new("Article", content = final_article_string, url = temp_url, title = temp_title, published_in=.self$name)
                        
                        return(new_article)
                      },
                      
                      scraping_links = function()
                      {
                        
                        cat("In Progress: \n")                    
                        
                        cat("Now scraping links for",.self$name,"\n")
                        .self$query_YQL();
                        .self$fetch_articles();
                        .self$updated_at = Sys.time();
                        
                      },
                      
                      
                      articles_YQL = function(query, format="xml",yql_address="http://query.yahooapis.com/v1/public/yql/")
                        
                      {
                        
                        final_url <- URLencode(paste(yql_address,"?q=",query,"&format=",format,sep=""))
                        devprint(final_url)
                        final_url2 <- url(final_url)
                        
                        i <- 1
                        while (i < 2){
                          res <- try(res <- readLines(con=final_url2, warn = FALSE,encoding="UTF-8"))
                          if (class(res) == "try-error") {
                            next
                          } else {
                            res <- paste(res,collapse="")
                            i <- i + 1
                          }
                        }           
                                     
                        devprint(res)
                        on.exit(close(final_url2))
                        return(res)
                        
                      },
                      
                      clean_article = function(article)
                      {
                        cleaned_article <- remove_tags(article)
                        return(cleaned_article)
                      },
                      
                      get_url = function(link_obj)
                        
                      {
                        if (is.null(link_obj$href)==TRUE)
                        {
                          if (is.null(link_obj$link)==TRUE)
                          {
                            if (is.null(link_obj$url)==TRUE) {devprint("wir haben ein problem")}
                            else final <- link_obj$url
                          }
                          else final <- link_obj$link
                        }
                        else  final <- link_obj$href
                       
                        if (is.character(final)) return(final)
                        if (is.list(link_obj$link)) return(final[[1]])  
                        
                      }
                                   
                    )
                    
                    # -Ende Funktionsbereich
)

#-------------------------------- Accessor Functions for Source ---------------------------------------#

NewsSource$accessors(c("name","aliases"))
NewsSource$accessors(c("query","article_query"))
NewsSource$accessors(c("url","rss"))
NewsSource$accessors(c("language","article_xpath"))

#------------------------------------------------------------------------------------------------------#
scrapeR <- setRefClass("newscrapeR",
                     fields = list(
                       
                       name      = "character",
                       source_list = "list",
                       active_source_list ="list",
                       links       = "vector",
                       active_links = "vector",
                       ROAuth = "ANY",
                       con = "ANY",
                       drv = "ANY"
                       
                     ),
                     
                     methods = list(
                                                               
                       check_article = function(Link)
                       {
                         ret <- NULL;
                         
                         for (j in seq_along(.self$source_list))
                         {
                           n <- .self$source_list[[j]]$aliases
                           temp <- match(tolower(Link),tolower(n))
                           if (!is.na(temp)) ret <- .self$source_list[[j]]
                         }
                         ret
                       },
                       
                       # function which checks which of the requested sources are available and starts a query_YQL search.
                       check_list = function(links)
                       {
                         .self$links = links;
                         LinkList      = list();             
                         ErrorLinkList = list();
                         
                         
                         for (i in seq_along(.self$links))
                         {
                           devprint(.self$links[i])
                           ret <- .self$check_article(.self$links[i])
                            
                           if (!is.null(ret)) 
                             {
                             LinkList <- append(LinkList,ret)
                             .self$active_links <- append(.self$active_links,links[i])
                             }
                           else           ErrorLinkList <- append(ErrorLinkList,.self$links[i])
                         }
                         
                         .self$active_source_list = LinkList;
                         .self$error_message("MissingSources", ErrorLinkList);
            
                          if (length(LinkList)>0) lapply(LinkList, scraping_links_wrapper);
                         
                       },
                       
                       update_sources = function(sources = .self$active_source_list)
                       {
                         LinkList <- sources; 
                         cat("In Progress: \n")
                         for (i in seq_along(LinkList))
                         {
                           
                           cat("Now scraping links for",LinkList[[i]]$name,"\n")
                           evalWithTimeout(LinkList[[i]]$query_YQL(),timeout = 1800, onTimeout="warning");
                           # LinkList[[i]]$query_YQL()
                           LinkList[[i]]$fetch_articles();
                           LinkList[[i]]$updated_at = Sys.time();
                           
                         } 
                       },
                       
                       check_double_sources = function(user_sources)
                         {
                         
                         all_source_links <- vector()
                         for (i in seq_along(.self$active_source_list))
                         {
                         all_source_links <- c(all_source_links,.self$active_source_list[[i]]$aliases)
                         }
                         
                         checkvar <- which(tolower(user_sources) %in% tolower(all_source_links))
                         double_checked_sources <- vector()
                         
                         if (length(checkvar)>0) 
                           {
                           cat("The following sources are already included:",user_sources[checkvar],"\n")
                           double_checked_sources <- user_sources[-checkvar]
                           return(double_checked_sources)
                           }
                         else return(user_sources)
                        
                         },
                       
                       check_available_sources = function(user_sources)
                       {
                         
                         all_source_links <- vector()
                         for (i in seq_along(.self$source_list))
                         {
                           all_source_links <- c(all_source_links,.self$source_list[[i]]$aliases)
                         }                 
                         
                         checkvar <- which(!(tolower(user_sources) %in% tolower(all_source_links)))
                         if (length(checkvar)>0) 
                         {
                           cat("The following sources are not available:",user_sources[checkvar],"\n")
                           available_sources <- user_sources[-checkvar]
                           return(available_sources)  
                         }
                         else return(user_sources)  
                         
                       },
                    
                       error_message = function(type, list)
                       {
                         if (type == "MissingSources" && length(list)>0) 
                         {
                         retstr <- paste(list, collapse=", ")
                         cat(paste("The following sources do not exist: ", retstr), ".", sep="")
                         cat(" Type available_sources() to see a list of available sources. \n")
                         }
                       },
                                  
                       add_source = function(new_sources)
                         {
                         ret <- .self$check_double_sources(new_sources);
                         if (length(ret)>0)
                           {
                           ret <- .self$check_available_sources(ret);
                           LinkList <- list();
                           
                           for (i in seq_along(ret))
                             {
                             m <- .self$check_article(ret[i])
                             if (!is.null(m))
                               {
                                 LinkList <- append(LinkList,m)
                                 .self$active_source_list <- append(.self$active_source_list,m)
                                 .self$active_links <- append(.self$active_links,ret[i]) 
                               }
                             }
                           
                           if (length(LinkList)>0) lapply(LinkList, scraping_links_wrapper);
                           }
                         },
                       
                       remove_source = function(selected_sources)
                       {
                         
                         for (i in seq_along(.self$active_source_list))
                           {
                             if (tolower(selected_sources) %in% tolower(.self$active_source_list[[i]]$aliases))
                               {
                               temp_name <- .self$active_source_list[[i]]$name
                               .self$active_source_list[[i]] <- NULL
                               cat(temp_name,"has been successfully from the newscrapeR.")
                               }
                           }
                                                               
                       },
                       
                       dump_article = function(article)
                       { 
                         require(RSQLite)
                         enc <- function(x) 
                            {
                            require(tau)
                            x <- gsub("'", "''", x) 
                            x <- fixEncoding(x)
                            x
                            }
                         connection = .self$con;
                         var_sql <- "INSERT INTO Article(title, author, abstract, content, url, load_date, published_in, pub_date, empty_content) VALUES('"
                         
                         var_sql <- paste(var_sql,enc(article$getTitle()),"',",sep="")                 
                         var_sql <- paste(var_sql,"'",enc(article$getAuthor()),"',", sep="")
                         var_sql <- paste(var_sql,"'",enc(article$getAbstract()),"',", sep="")
                         var_sql <- paste(var_sql,"'",enc(article$getContent()),"',", sep="")
                         var_sql <- paste(var_sql,"'",article$getUrl(),"',", sep="")
                         
                         load_ts <- as.numeric(as.POSIXlt(article$getLoad_date()))
                         
                         var_sql <- paste(var_sql, load_ts, ",", sep="")
                         var_sql <- paste(var_sql,"'",enc(article$getPublished_in()),"',", sep="")
                         
                         pub_ts <- as.numeric(as.POSIXlt(article$getPub_date()))
                         if (is.na(pub_ts)) pub_ts <- 0 
                         
                         var_sql <- paste(var_sql, pub_ts, ",", sep="")
                         
                         var_sql <- paste(var_sql, as.integer(article$empty_content),")",sep="")
                         
                         print(var_sql)
                         dbSendQuery(connection, var_sql)
                         
                         
                       },
                       
                       complete_dump = function()
                         {
                         
                         for (i in seq_along(.self$active_source_list))
                           {
                            a <- active_source_list[[i]]
                            for (j in seq_along(a$articles))
                            {
                            a$articles[[j]]$dumped = FALSE;
                            if (a$articles[[j]]$dumped == FALSE) 
                                {
                                .self$dump_article(a$articles[[j]]) 
                                a$articles[[j]]$dumped = TRUE;
                                
                                }
                            }
                           }
                                    
                         },
                       
                       search_articles = function(keywords = character(), sources = vector(),
                                                  from, to)
                        {
                         
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
                         
                         query_string <- paste(query_string, cond1, cond2, sep="")
                         print(query_string)
                         res <- dbGetQuery(conn=.self$con,query_string)
                         Encoding(res$content) <- "UTF-8"
                         Encoding(res$title) <- "UTF-8"
                         return(res)
                        },
                         
                       initialize = function(links, old_newscrapeR = NULL, db_name)
                         {
                            if (is.null(db_name)) 
                              {
                                .self$name <- "newscrapeR"
                                db_name <- "newscrapeR.db"
                              } 
                            else 
                              {
                               .self$name <- db_name 
                               db_name <- paste(db_name,".db",sep="")
                              }
                            
                            require(RSQLite)              
                            .self$drv <- dbDriver("SQLite") 
                            .self$con <- dbConnect(.self$drv,db_name)
                            dbGetQuery(.self$con,"PRAGMA ENCODING = \"UTF-8\";")
                            
                            if (!"Article" %in% dbListTables(.self$con))
                              {                        
                              create_str <- "CREATE TABLE Article(Id INTEGER PRIMARY KEY,
                                              title VARCHAR(512),
                                              author CHAR(164),
                                              abstract TEXT,
                                              content TEXT,
                                              url VARCHAR(512),
                                              load_date INTEGER,
                                              published_in CHAR(80),
                                              pub_date INTEGER,
                                              empty_content INTEGER 
                                              )"
                              
                              dbSendQuery(.self$con, create_str)
                              
                              }
                              
                         
                           .self$links = links;
                           
                           # initialization of sources
                           
                           spiegel = new("Source", query="select * from rss where url='http://www.spiegel.de/schlagzeilen/index.rss'",
                                         name="Spiegel Online",aliases=c("Spiegel","Der Spiegel","Spiegel Online"),url="http://www.spiegel.de",rss=TRUE,
                                         article_xpath="//div[@id='spArticleSection']/p[not(script)]"
                                         );
                           
                           .self$source_list[1] = spiegel;
                           
                           
                           welt = new("Source", query="select * from rss where url='http://www.welt.de/politik/?service=Rss'",
                                      name="Die Welt",aliases=c("Die Welt","Welt"),rss=TRUE,url="http://www.welt.de",article_xpath="//div//p[@class='prefix_2 text artContent']|a");
                           
                           .self$source_list[2] = welt;
                           
                           
                           indep = new("Source",query="select * from rss where url='http://rss.feedsportal.com/c/266/f/3495/index.rss'",
                                       name="The Independent",aliases=c("Independent","The Independent"),url="http://www.independent.co.uk/",
                                       rss=TRUE,article_xpath="//div[@class='widget storyContent article widget-editable viziwyg-section-1024 inpage-widget-6138699 articleContent']//*")
                           
                           .self$source_list[3] = indep;
                           
                           ## robots blocks access?
                           #nytimes = new("Source",query="select * from rss where url='http://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml'",name="New York Times",aliases=c("New York Times","NY Times"),
                           #              url="http://www.nytimes.com/")
                           
                           
                           #.self$source_list[4] = nytimes;
                          
                           
                           telegraph = new("Source",query="select * from rss where url='http://www.telegraph.co.uk/news/rss'",
                                           name="The Telegraph",aliases=c("Telegraph","The Telegraph"),rss=TRUE,article_xpath="//div[@id='mainBodyArea']//p",
                                           url="http://www.telegraph.co.uk/")
                           
                           .self$source_list[4] = telegraph;
                           
                           
                          # latimes = new("Source",query="select * from rss where url='http://pipes.yahoo.com/pipes/pipe.run?_id=5dc8a87340794a992d374f7389268bc3&_render=rss'",name="Los Angeles Times",
                          #              aliases=c("Los Angeles Times","LA Times"),url="http://www.latimes.com/",rss=TRUE,article_xpath="//div[@id='story-body-text']/p|a")
                           
                          # .self$source_list[5] = latimes;
                           
                           natpost = new("Source",query="select * from rss where url='http://news.nationalpost.com/category/news/feed/'",
                                         name="National Post",aliases="National Post",url="http://www.nationalpost.com",rss=TRUE,
                                         article_xpath="//div[@class='npBlock npPostContent']//p")
                           
                           .self$source_list[5] = natpost;
                           
                           # Houston Chronicle
                           chron = new("Source",query="select * from rss where url='http://www.chron.com/rss/feed/News-270.php'",name="Houston Chronicle",
                                       aliases=c("Houston Chronicle","Houston Chron"),url="http://www.chron.com/",rss=TRUE,article_xpath="//div[@class='article-body clearfix']/p|a")
                           
                           .self$source_list[6] = chron;
                           
                           
                           # The Guardian
                           guardian <- new("Source",query="select * from rss where url='http://www.guardian.co.uk/theguardian/rss'",
                                           name="The Guardian",aliases=c("The Guardian","Guardian"),url="http://www.guardian.co.uk/",rss=TRUE,article_xpath="//div[@id='article-body-blocks']/p|a")
                           
                           .self$source_list[7] = guardian;
                           
                           # Chicago Tribune
                           chicago_tribune <- new("Source",query="select * from rss where url='http://feeds.chicagotribune.com/chicagotribune/news/'",
                                                  name="Chicago Tribune",aliases=c("Chicago Tribune"),url="http://www.chicagotribune.com/",rss=TRUE,article_xpath="//div[@id='story-body-text']/p|a")
                           
                           .self$source_list[8] = chicago_tribune;
                           
                           
                          # nypost <- new("Source",query="select * from rss where url='http://www.nypost.com/rss/all_section.xml'",
                          #               name="New York Post",aliases=c("NY Post","New York Post"),url="http://www.nypost.com/",rss=TRUE,article_xpath="//div[@id='story-body']/p|a")
                           
                          # .self$source_list[9] = nypost;
                           
                          # watimes <- new("Source",query="select * from rss where url='http://www.washingtontimes.com/atom/headlines/news/'",
                          #                name="The Washington Times",aliases=c("Washington Times","The Washington Times"),
                          #                url="http://www.washingtontimes.com",rss=TRUE,
                          #                article_xpath="//div[@class='story left mb']//p")
                          # 
                          #  .self$source_list[9] = watimes;
                           
                           baltimoresun <- new("Source",query="select * from rss where url='http://feeds.feedburner.com/baltimoresun/news/rss2'",
                                               name="Baltimore Sun",aliases="Baltimore Sun",url="http://www.baltimoresun.com/",
                                               rss=TRUE,article_xpath="//div[@id='story-body-text']//p")
                           
                           .self$source_list[9] = baltimoresun;
                           
                           # does not work either
                           # philly <- new("Source",query="select * from rss where url='http://www.philly.com/philly_news.rss'",
                           #              name="Philadelphia Inquirer",aliases=c("Philadelphia Inquirer","Philly"),url="http://www.philly.com",rss=TRUE,article_xpath="//div[@class='body-content']/p|a")
                           #
                           #
                           #.self$source_list[10] = philly;
                           
                          #seattle <- new("Source",query="select * from rss where url='http://seattletimes.com/rss/home.xml'",
                          #                name="The Seattle Times",aliases=c("The Seattle Times","Seattle Times"),rss=TRUE,url="http://seattletimes.com",
                          #                article_xpath="//div[@id='leftcolumn']//p")
                          # 
                          # .self$source_list[10] = seattle;
                            
                           detroit <- new("Source",query="select * from rss where url='http://www.detroitnews.com/feeds/rss.xml'",
                                          name="The Detroit News",aliases=c("Detroit News","The Detroit News"),rss=TRUE,
                                          article_xpath="//div[@class='gel-content']//p",url="http://www.detroitnews.com")
                           
                           .self$source_list[10] = detroit;
                           
                           ny_dailynews <- new("Source",query="select * from rss where url='http://feeds.nydailynews.com/nydnrss'",
                                               name="New York Daily News",aliases=c("NY Daily News","New York Daily News"),url="http://www.nydailynews.com/",rss=TRUE,article_xpath="//div[@class='story-body p402_premium']/p|a")
                           
                           .self$source_list[11] = ny_dailynews;
                           
                           # ----------------- Denver Post ------------------
                           denverpost <- new("Source",query="select * from rss where url='http://feeds.denverpost.com/dp-news-breaking'",
                                             name="Denver Post",aliases=c("Denver Post","DenverPost"),url="http://www.denverpost.com",rss=TRUE,article_xpath="//div[@class='articleBody']/p|a")
                           
                           .self$source_list[12] = denverpost;
                           
                           # ---------- Frankfurter Allgemeine Zeitung ------
                           faz = new("Source", query ="select * from rss where url='http://www.faz.net/aktuell/?rssview=1'",
                                     name="Frankfurter Allgemeine Zeitung",aliases=c("FAZ","Frankfurter Allgemeine Zeitung","Frankfurter Allgemeine"),
                                     url="http://www.faz.net",rss=TRUE,article_xpath="//div[@class='FAZArtikelText']/p");
                           
                           .self$source_list <- append_list(.self$source_list,faz)
                           
                           
                           miami <- new("Source",query="select * from rss where url='http://www.miamiherald.com/news/index.xml'",
                                        name="Miami Herald",url="http://www.miamiherald.com",aliases=c("Miami Herald"),rss=TRUE,
                                        article_xpath="//div[@id='storyBodyContent']/p|a")
                           
                           .self$source_list <- append_list(.self$source_list,miami)
                          
                            
                            for (i in 1:length(.self$source_list))
                                {
                                .self$source_list[[i]]$parent = .self
                                }
                            
                           # here is the start of the reading procedure
                           if (is.null(old_newscrapeR))
                             .self$check_list(.self$links)
                           else 
                             {
                             .self$links <-  old_newscrapeR$links
                             .self$active_links <- old_newscrapeR$active_links
                             
                             new_source_list <- list()
                             for (i in 1:length(old_newscrapeR$active_source_list))
                               {
                               new_source_list[[i]]  <- NewsSource$new(old_Source = 
                                                                      old_newscrapeR$active_source_list[[i]]) 
                               new_source_list[[i]]$parent = .self;
                               }
                             .self$active_source_list <- new_source_list
                             }
                         },
                       
                       show = function()
                         
                         {   
                         
                           cat("Total # of Articles:",length(articles(.self)),"\n")
                           cat("Included Sources: \n")
                           for (i in seq_along(.self$active_source_list))
                             {
                               source_name <- .self$active_source_list[[i]]$name;
                               no_articles <-length(articles(.self,sources=source_name));
                               cat(.self$active_source_list[[i]]$name,": ",no_articles," articles \n",sep="")
                             } 
                           
                         },
                       
                       summary = function()
                         
                         {
                           cat("A newscrapeR Object \n \n")
                           cat("Total # of Articles:",length(articles(.self)))
                           cat("Included Sources: \n")
                           
                           for (i in seq_along(.self$active_source_list))
                           {
                             
                             source_name <- .self$active_source_list[[i]]$name;
                             no_articles <- length(articles(.self,sources=source_name));
                             cat(.self$active_source_list[[i]]$name,": \t",no_articles," articles",sep="");
                             
                             update <- as.character(.self$active_source_list[[i]]$updated_at);
                             
                             cat(" (last time updated on: ",update,") \n",sep="");
                             
                           }; 
                           
                         }
             
                     )
                     
)

# -------------------------------------------- newscrapeR Public Section -------------------------------------------------------

# creates a new newscrapeR object
# Arguments:
# sources - character vector of included sources

newscrapeR <- function(sources = vector(), db_name = NULL)
  {
    ret <-new("newscrapeR",links = sources, db_name = db_name)
    ret
  }


migrate.newscrapeR <- function(newscrapeRobj, db_name = NULL)
  {
  ret <- scrapeR$new(old_newscrapeR = newscrapeRobj, links = vector(),  db_name = db_name)   
  ret
  }

migrate.Source <- function(SourceObj)
{
  ret <- NewsSource$new(old_Source = SourceObj)   
  ret
}


registerROAuth <- function(newscrapeRobj,OAuth) 
{ 
  
  if (!inherits(OAuth, "OAuth")|| !inherits(newscrapeRobj,"newscrapeR"))
    stop("Please supply the function with objects of the required classes 'OAuth' and 'newscrapeR'")
  if (!oauth$getHandshakeComplete()) 
    stop("Handshake is not yet completed")  
  
  if(require(ROAuth,quietly=TRUE))
    {
      
     
     
    }
    else
      {
        cat('trying to install package "ROAuth"')
        install.packages("ROAuth") || stop('could not install package "ROAuth"')
      }   
      
};

# ------------------------ Function to convert an Article List  --------------------#

ArtListToDF <- function (List) 
# exports to DataFrame
# Arguments:
# List:  a list of objects with class "Article"   
  {
    do.call("rbind", lapply(List, as.data.frame))
  };

setMethod("as.data.frame","Article", 
          function(x,row.names = NULL, optional = FALSE) 
          {
            
            df <- data.frame(title = x$title, author = as.factor(x$author), abstract = x$abstract,
                             content = x$content, url = x$url, load_date = x$load_date,
                             published_in=as.factor(x$published_in), stringsAsFactors = FALSE);
            
            return(df)
            
          } 
          
);


ArtListToCorpus <- function (List,language="en")
# exports Article List to Corpus format from package "tm" for further processing
# Arguments:
# List:  a list of objects with class "Article" 
# language: object of class character specifying the language of the text
  {
      
    if(require(tm,quietly=TRUE))
      {
        df <- ArtListToDF(List)
        Contents <- df$content 
        x<-VectorSource(Contents)
        mycorpus <- Corpus(x,readerControl=list(reader=readPlain(),language=language))
        return(mycorpus)
      }
    else
      {
      cat('trying to install package "tm"')
      install.packages("tm")
      if(require(tm,quietly=TRUE))
        {
          df <- ArtListToDF(List)
          Contents <- df$content 
          x<-VectorSource(Contents)
          mycorpus <- Corpus(x,readerControl=list(reader=readPlain(),language=language))
          return(mycorpus)
        } else stop('could not install package "tm"')
       }    
  };

# -------------------------- S4 Methods for Article class -------------------------------------#

setMethod("summary","Article", 
          function(object)
          {  
          object$summary()
          }
);


# -------------------------- S4 Methods for Source class -------------------------------------#

setMethod("summary","Source", 
           function(object)
           {  
            object$summary()
           }
);


# -------------------------- S4 Methods for newscrapeR class -------------------------------------#

# show method for newscrapeR object
setMethod("show","newscrapeR", function(object) 
  {
    object$show()
  } 
);

# show method for newscrapeR object
setMethod("summary","newscrapeR", function(object) 
  {
    object$summary()
  } 
);

#----------------------------------- download ------------------------------------------------#
# function to update newscrapeR object by fetching current articles from the respective sources

setGeneric("download",
           def= function(object,...)
           {
             standardGeneric("download")
           }
);

setMethod("download","newscrapeR", function(object, sources = NULL, exclude = NULL) 
            
             {
              final_sources <- list()
              if (is.null(sources)) 
                {
                if (is.null(exclude)) final_sources = object$active_source_list
                else
                  {
                  for (i in seq_along(object$active_source_list))
                    {
                      if (!object$active_source_list[[i]]$name %in% exclude) # does not deal with aliases yet
                        final_sources <- append_list(final_sources,object$active_source_list[[i]])
                    }
                  }
                }
              
              else 
                {
                for (i in seq_along(object$active_source_list))
                  {
                  if (object$active_source_list[[i]]$name %in% sources) # does not deal with aliases yet
                  final_sources <- append_list(final_sources,object$active_source_list[[i]])
                  }
                }  
              if (length(final_sources)==0) 
              cat("The newscrapeR object does not contain any of the supplied sources. To see a list of all currently included sources, call sources(newscrapeR object).")
              else object$update_sources(final_sources)
             } 
          
          );

# ---------------------------------------------------------------------------------------------#


#----------------------------------- add_sources ----------------------------------------------#
# function to add one or more new sources to an existing newscrapeR object
# Arguments:
# object - object of class "newscrapeR" to which new sources should be added
# sources - a character vector containing the additional sources

setGeneric("add.sources",
           def= function(object,sources)
           {
             standardGeneric("add.sources")
           }
);

setMethod("add.sources","newscrapeR", 
          function(object,sources) 
            {
              object$add_source(sources)
            } 
);

#----------------------------------- remove_sources ----------------------------------------------#
# function to remove one or more new sources from an existing newscrapeR object
# Arguments:
# object - object of class "newscrapeR" from which sources should be deleted
# sources - a character vector containing the sources which should be removed

setGeneric("remove.sources",
           def= function(object,sources)
           {
             standardGeneric("remove.sources")
           }
);

setMethod("remove.sources","newscrapeR", 
          function(object,sources) 
          {
            object$remove_source(sources)
          } 
);
# ---------------------------------------------------------------------------------------------#

setGeneric("available.sources",
           def= function(object,...)
           {
             standardGeneric("available.sources")
           }
);

setMethod("available.sources","newscrapeR", function(object)
  {
    cat("Available newscrapeR Sources: \n")
    cat("---------------------------- \n")
    for (i in seq_along(object$source_list))
      { 
        cat(object$source_list[[i]]$name,", ",sep="")
        cat("URL:",object$source_list[[i]]$url,"\n")
      }
  }
);

# ---------------------------------------------------------------------------------------------#

setGeneric("sources",
           def= function(object,...)
           {
             standardGeneric("sources")
           }
);

setMethod("sources","newscrapeR", function(object)
  {
    cat("Included newscrapeR Sources: \n")
    for (i in seq_along(object$active_source_list))
      print(object$active_source_list[[i]])
    cat("\n")
  }
);


# ----------------------------------------------------------------------------------------- #
# method to extract articles from newscrapeR object 
# ----------------------------------------------------------------------------------------- #

setGeneric("articles",
           def= function(object,sources=vector(),keyword=character(),from=NULL,to=NULL)
           {
             standardGeneric("articles")
           }
);

setMethod("articles","newscrapeR", function(object, sources = vector(),
                                            keyword = character(),from=NULL,to=NULL)
{
 
 if (!is.character(keyword))
    {
    keyword <- as.character(keyword)
    }
 
  if (length(sources)==0)
  # select from all sources
  {
    ret <- list()
    for (i in seq_along(object$active_source_list))
    {    
        
        if (length(keyword)==0)
           {
            if (is.null(from) && is.null(to)) ret <- c(ret,object$active_source_list[[i]]$articles)
            else
            ret <- object$active_source_list[[i]]$select_time_period(from=from,to=to)
           }
        else
        {  
          if (is.null(from) && is.null(to))
          {
            fetched <- object$active_source_list[[i]]$search(keyword)
          }
          else
            {
            fetched <- object$active_source_list[[i]]$select_by_keyword_and_time(
                                                      keyword = keyword,from = from,to = to)
            }
        ret <- c(ret,fetched)  
        }
      
    }
    if (length(ret)==0) cat("Unfortunately, the number of articles which match the specified requirements is zero. \n")
    
    ret <- remove_empty_articles(ret)
  
    return(ret)
  }
  else
  # fetch from selected sources
  {
    ret <- list()
    for (i in seq_along(object$active_source_list))
    {
            
        if (length(keyword)==0)
          { 
          if (object$active_source_list[[i]]$name %in% sources)
            {
              if (is.null(from) && is.null(to)) ret <- c(ret,object$active_source_list[[i]]$articles) 
              else ret <- c(ret,object$active_source_list[[i]]$select_time_period(from=from,to=to))
            }
          }
        else
        {
          if (object$active_source_list[[i]]$name %in% sources)
          {
        
            if (is.null(from) && is.null(to))
              {
                fetched <- object$active_source_list[[i]]$search(keyword)
              }
            else
              {
                fetched <- object$active_source_list[[i]]$select_by_keyword_and_time(
                  keyword = keyword,from = from,to = to)
              }
            ret <- c(ret,fetched)    
            
          }
        }
      
    }
    
    ret <- remove_empty_articles(ret)
    if (length(ret)==0) cat("Unfortunately, the number of articles which match the specified requirements is zero. \n")
    return(ret)
  }
}
);
