PDFSource <- setRefClass("PDFSource",
                         fields = list(
                           
                           name      = "character",
                           query     = "character",
                           url       = "character",
                           updated_at = "POSIXct",
                           language = "character",                           
                           pdf_links = "list",
                           rss = "logical",
                           pbar = "ANY",
                           replace_command = "character"
                           
                           ),
                         
                         methods = list(
                           
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
                             
                             result = json_to_r_link(res);
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
                                 
                                 a = check_valid_url(list[[i]]$link);
                                 
                                 devprint(list[[i]]$link)
                                 if ( length(a) == 0) list[[i]]$link = paste(.self$url, list[[i]]$link,sep="");
                                 
                                 
                                 if (.self$check_double (list[[i]] ) == FALSE)
                                 {
                                   .self$append_link(list[[i]]);
                                  # .self$fetch_pdf(list[[i]]);
                                 }
                                 
                                 # update progress bar
                                 setTxtProgressBar( .self$pbar, i)
                                 
                               }
                               
                               devprint(.selfa$pdf_links)
                             }
                             else 
                               cat("\n No new PDFs from ",.self$name," available right now. Try again later.")
                           },
                           
                           # checks whether the link is already stored
                           check_double = function(link)
                           {
                             ret = FALSE;
                             
                             for (i in seq_along(.self$pdf_links))
                             {
                               item = .self$pdf_links[[i]][1];
                               if (link[[1]] == item) ret = TRUE;
                             }
                             
                             ret;
                           },
                           
                           append_link = function(item)
                           {
                             .self$pdf_links[[length(.self$pdf_links) + 1]] <- item;
                             .self$pdf_links[[length(.self$pdf_links)]]$already_fetched <- FALSE;
                             
                           },
                           
                           
                           fetch_pdf = function(Link)
                           {
                           pdf.title <- substring(gsub(" ","_",x=Link$title),first=1,last=30)
                           dest_file <- paste(pdf.title,".pdf",sep="")
                           temp_url <- .self$get_url(Link)
                           downloadFile(url=temp_url,filename=dest_file)
                           
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
                             
                           },
                           
                           fetch_pdfs = function()
                           {
                             new_files_count <- 0
                             fetched_counter <- 0

                             
                             for (i in seq_along(.self$pdf_links))
                             {
                               if (.self$pdf_links[[i]]$already_fetched == FALSE) 
                                 new_files_count <- new_files_count + 1 
                             }
                             
                             if (new_files_count > 0)
                             {
                               cat("\nFetching PDFs from",.self$name,"\n")
                               .self$pbar <- txtProgressBar(min = 0, max = new_files_count + 1, style = 3)
                               
                               for (i in seq_along(.self$pdf_links))
                               {
                                 if (.self$pdf_links[[i]]$already_fetched == FALSE) 
                                 {

                                   temp_url <- .self$get_url(.self$pdf_links[[i]])
                                   
                                   temp_url <- .self$replace_url(temp_url)
                                   
                                   temp_title <- .self$pdf_links[[i]]$title
                                   temp_title <- paste0(temp_title,".pdf")
                                   downloadFile(url=temp_url, filename=temp_title)
                                   
                                   .self$pdf_links[[i]]$already_fetched <- TRUE 
                                   
                                   # update progress bar
                                   fetched_counter <- fetched_counter + 1;                           
                                   setTxtProgressBar( .self$pbar, fetched_counter);                                   
                                                        
                                 }                         
                                 
                               }
                               
                               setTxtProgressBar( .self$pbar, fetched_counter + 1);
                               close(.self$pbar)
                             }
                             else cat("\n No new PDFs from ",.self$name," available right now. Try again later.")
                                         
                           },
                           
                           replace_url = function(link)
                           {
                           ret <- eval(parse(text=.self$replace_command))
                           ret
                           },
                                                                 
                           initialize = function(query = as.character(NA), url = as.character(NA), 
                                                 name = as.character(NA), rss = FALSE, old_Source = NULL,
                                                 replace_command = "link"
                           )
                           {
                             
                             if(is.null(old_Source))
                             {
                               .self$name <- name;
                               .self$url <- url;
                               .self$query <- query;
                               .self$rss <- rss;
                               .self$replace_command <- replace_command

                             }
                             else
                             {
                               .self$name <- old_Source$name;
                               .self$url <- old_Source$url;
                               .self$query <- old_Source$query;
                               .self$rss <- old_Source$rss;  
                               
                               .self$updated_at <- old_Source$updated_at;
                               .self$pdf_links <- old_Source$pdf_links;
                               
                             }
                             
                           }
                           
                           )
                         ) 

 arxiv_stats = new("PDFSource", query="select * from rss where url='http://export.arxiv.org/rss/stat'",
                  name="arXiv Statistics",url="http://arxiv.org/",rss=TRUE,
                  replace_command='paste0(gsub(pattern="abs",replacement="pdf",link),"v1.pdf")')

setMethod("download",signature="PDFSource", function(object,...)
  {
  object$query_YQL()
  object$fetch_pdfs()
  object$updated_at = Sys.time();
  }
)