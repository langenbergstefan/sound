\name{stereo}
\alias{stereo}
\title{ Create a Stereo Sample Object from Two Mono Samples }
\description{
  Create a stereo Sample object, given the two channels as Sample objects or wav files.
}
\usage{stereo(sLeft, sRight, pan=50)
}
\arguments{
  \item{sLeft}{ a Sample object or a string giving the name of a wav file. Used for the left channel. }
  \item{sRight}{ a Sample object or a string giving the name of a wav file. Used for the right channel. }
  \item{pan}{ a number between -50 and 50 describing the distance between the two sound sources.
}
}
\details{
If \code{abs(pan)<50}, mixtures of the two sources are used for the left and the right channel so that they appear closer to the center. For \code{pan=0}, both sounds are at the center. If \code{pan<0}, left and right channel are interchanged afterwards.

If the samples have different sample parameters (bits, rate and channels), the command code{\link{fitSampleParameters}} is called to adjust them before the two samples are combined.
}

\value{
  a stereo Sample object.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{left}}, \code{\link{right}}, \code{\link{as.Sample}}, \code{\link{panorama}} }

\examples{\dontrun{
sLeft <- Sine(440,1)
sRight <- Sine(220,1)
s <- stereo(sLeft,sRight)
play(s)
}}
\keyword{ manip }