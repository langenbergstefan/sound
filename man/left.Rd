\name{left}
\alias{left}
\alias{right}
\title{ Extract one Channel from a Stereo Sample }
\description{
  Extract either the left or the right channel of a stereo Sample object or a stereo wav file.
}
\usage{left(s)
right(s)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
}
\details{
If \code{s} is a mono sample, it will be returned as it is.
}
\value{
  a Sample object containing the left or the right channel of \code{s}.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{stereo}} for creating a stereo Sample object from two mono samples. }

\examples{\dontrun{
sLeft <- Sine(440,1)
sRight <- Sine(220,1)
s <- stereo(sLeft,sRight)
play(s)
play(left(s))  # only the left channel
play(right(s)) # only the right channel
}}
\keyword{ manip }
