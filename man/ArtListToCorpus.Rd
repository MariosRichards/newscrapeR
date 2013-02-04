\name{ArtListToCorpus}
\alias{ArtListToCorpus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a List of "Articles" to a "Corpus"
}
\description{
This function takes as an input a list of objects of class "Article" and turns them into a "Corpus" object, making them ready to be analyzed via the means provided by the package "tm"
}
\usage{
ArtListToCorpus(List, language = "en")
}

\arguments{
  \item{List}{
A list of objects of class \code{\link{Article}}. 
}
  \item{language}{
A character string outlining the language of the documents
}
}

\value{
An object of class VCorpus containing the contents of all passed articles.
}

\author{
Philipp Burckhardt
}

\seealso{
\code{\link[tm]{VCorpus}}
}
\examples{
my.scraper <- newscrapeR("Houston Chronicle")
article.list <- articles(my.scraper)
my.corpus <- ArtListToCorpus(article.list)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
