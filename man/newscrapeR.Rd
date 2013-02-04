\name{newscrapeR}
\alias{newscrapeR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a newscrapeR object
}
\description{
This function creates a newscrapeR object, which constitutes the basic object for all subsequent calculations. In it, all fetched articles are stored. 
}
\usage{
newscrapeR(sources)
}

\arguments{
  \item{sources}{
A character vector of all news sources which the object should keep track of. 
}
}
\details{
If no sources are passed to the function, an empty newscrapeR object is created. Further sources can be added using the function \code{\link{add_sources}}. A list of all available inputs can be obtained by calling \code{\link{available_sources}}.
}
\value{
A \code{\link{newscrapeR}} object.
}
\author{
Philipp Burckhardt
}

\examples{
my.scraper <- newscrapeR("Houston Chronicle")
my.scraper2 <- newscrapeR(sources=c("Houston Chronicle","Denver Post"))
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
