\name{print.Sample}
\alias{print.Sample}
\title{ Print a Sample Object }
\description{
  Display the basic information about a Sample object or a wav file.
}
\usage{\method{print}{Sample}(x,\dots)
}
\arguments{
  \item{x}{ a Sample object, or a string giving the name of a wav file. }
  \item{\dots}{ further parameters, not used at the moment. }
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{plot.Sample}} for plotting the waveform of a sample. }

\examples{\dontrun{
s <- Sine(440,1)
print(s)
}}

\keyword{ print }
