\name{getTweets}
\alias{getTweets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Twitter feeds from CitationContainer
}
\description{
This function will allow you to extract Twitter feeds from a CitationContainer object. 
}
\usage{
getTweets(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
CitationContainer object which stores the respective data 
}
}
\details{
The CitationContainer class provides an easy way to store Twitter feeds which quote articles in a  \code{\link{newscrapeR}} object. It uses the twitteR package by Jeff Gentry.
}
\value{
A list of \code{\link{data.frame}} objects, one for for each \code{\link{Article}}. The data frames contain the associated Twitter feeds.
}
\references{
%% ~pureferences to the literature/web site here ~
}
\author{
Philipp Burckhardt
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object) 
{
    retlist <- list()
    for (i in 1:length(object$citation_list)) {
        retlist[[i]] <- object$citation_list[[i]]$tweetsDF
    }
    return(retlist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
