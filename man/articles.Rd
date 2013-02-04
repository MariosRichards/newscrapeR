\name{articles}
\alias{articles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to view and extract articles
}
\description{
These functions will allow you to extract articles from a newscrapeR object
}
\usage{
articles(object, sources = vector(), keyword = character(), from = NULL, to = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
newscrapeR object which stores the respective data 
}
  \item{sources}{
a character vector specifying the sources from which the articles should be extracted 
}
  \item{keyword}{
an object of type "character". if supplied, only articles containing the keyword are returned. 
}
 \item{from}{
if supplied, only articles with a publication date later than the one supplied wll be returned. It should be supplied as a character string with the format YYYY-MM-DD.
}
 \item{to}{
if supplied, only articles published before the specified date wll be returned. The value should be supplied as a character string with the format YYYY-MM-DD.
}
}
\details{
The search for a certain keyword is not case-sensitive.
}
\value{
A list of \code{\link{Article}} objects.
}

\author{
Philipp Burckhardt
}

\seealso{
\code{\link{newscrapeR}}
}
\examples{
myscraper <- newscrapeR(sources="Guardian")
articles(myscraper)
articles(myscraper,keyword="Cameron")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
