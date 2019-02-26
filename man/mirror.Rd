\name{mirror}
\alias{mirror}
\title{ Mirror a Stereo Sample }
\description{
  Interchange the left and the right channel of a stereo Sample object or a stereo wav file.
}
\usage{mirror(s)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
}
\details{
If \code{s} is a mono sample, it will be returned as it is.
}
\value{
  a Sample object, with the left and the right channel of \code{s} interchanged.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{panorama}} for a command with more parameters, \code{\link{left}} and \code{\link{right}} for access to single channels of a sample. }

\examples{\dontrun{
s <- stereo(Sine(440,1),Sine(220,1))
play(s)  # higher tone is on the left
play(mirror(s))  # now higher tone is on the right
}}
\keyword{ manip }
