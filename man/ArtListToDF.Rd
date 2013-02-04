\name{ArtListToDF}
\alias{ArtListToDF}

\title{
Converts a List of Articles to a data.frame
}
\description{
This functions takes a list of objects of class \code{\link{Article}} and returns the contents of their fields as a data.frame 
}
\usage{
ArtListToDF(List)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{List}{
A List of objects of class \code{\link{Article}}.
}
}

\value{
A \code{\link{data.frame}} in which the rows represent the individual article objects of the list and the columns contain the fields of the class. 
}

\author{
Philipp Burckhardt
}

\seealso{
\code{\link{Article}}
}
\examples{
my.scraper <- newscrapeR("Houston Chronicle")
article.list <- articles(my.scraper)
my.df <- ArtListToDF(article.list)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
