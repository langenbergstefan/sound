\name{saveSample}
\alias{saveSample}
\title{ Save a Sample Object as a WAV File }
\description{
  Save a Sample object to disk as a wav file.
}
\usage{saveSample(s, filename, overwrite=FALSE)
}
\arguments{
  \item{s}{ a Sample object. }
  \item{filename}{ a string giving the path and the name of the destination file. }
  \item{overwrite}{ logical. If \code{FALSE} and \code{filename} already exists, an error will be reported. Otherwise the old file will be deleted. }
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{loadSample}} }

\examples{\dontrun{
s <- Sine(440,1)
saveSample(s,"sine.wav")
}}
\keyword{ file }
