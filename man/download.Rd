\name{download}
\alias{download}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download current articles 
}
\description{
Called upon an newscrapeR object, this function initializes a download of all the current articles from the sources inside the newscrapeR object.
}
\usage{
download(object, sources)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
newscrapeR object for which the articles should be downloaded
}
  \item{sources}{
a character vector specifying the sources of the newscrapeR object for which a download should be executed
}
}
\details{
If nothing is passed to the argument \code{sources}, new articles are downloaded for all the active sources of the newscrapeR object.
}

\author{
Philipp Burckhardt
}

\seealso{
\code{\link{newscrapeR}}
}
\examples{

myscraper <- newscrapeR( c("Houston Chronicle", "The Guardian") )

# initializes download of new articles of the Houston Chronicle:
download(myscraper, sources = "Houston Chronicle") 

# downloads new articles for all sources:
download(myscraper) 
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
