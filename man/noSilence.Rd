\name{noSilence}
\alias{noSilence}
\title{ Cut Off Silence from a Sample Object }
\description{
  Cut off silence or low noise at the beginning and/or at the end of a Sample object or a wav file.
}
\usage{noSilence(s, level=0, start=TRUE, end=TRUE)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{level}{ non-negative numeric. Absolute values in the waveform matrix smaller than or equal to this value are regarded as silence. }
  \item{start}{ logical. If \code{TRUE}, silence at the beginning of the sample will be cut off. }
  \item{end}{ logical. If \code{TRUE}, silence at the end of the sample will be cut off. }
}
\details{
For stereo samples, it is checked if the values of both channels are silence before the silence is cut off.
}
\value{
  a Sample object without those parts at the start and at the end of the original sample that are below the specified noise level.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{cutSample}} }

\examples{\dontrun{
s <- Sine(440,5)
sound(s) <- sound(s)*matrix(seq(1,0,length=5*44100),nrow=1)
sampleLength(s)
play(s) # fade out
s <- noSilence(s,level=.05)
sampleLength(s)  # s is shorter now
play(s)    # although you don't hear that the end is missing
}}
\keyword{ manip }
