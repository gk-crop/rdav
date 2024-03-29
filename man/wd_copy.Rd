% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/webdav.R
\name{wd_copy}
\alias{wd_copy}
\title{Copies a file or directory on the WebDAV server}
\usage{
wd_copy(req, source, target, overwrite = TRUE)
}
\arguments{
\item{req}{request handle obtained from \code{\link{wd_connect}}}

\item{source}{path of the source on the server}

\item{target}{path of the target on the server}

\item{overwrite}{overwrites files when TRUE (default)}
}
\value{
TRUE on success, FALSE on failure (invisibly)
}
\description{
Copies a file or directory on the WebDAV server
}
\examples{
\dontrun{

wd_copy(r, "testfile.R", "testfile_old.R")

}
}
