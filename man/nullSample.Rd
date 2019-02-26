\name{nullSample}
\alias{nullSample}
\title{ The NULL Sample Object }
\description{
  Create a Sample object whose waveform has length 1 and value 0. Often useful to initialize loops.
}
\usage{nullSample(rate=44100, bits=16, channels=1)
}
\arguments{
  \item{rate}{ the sampling rate, between 1000 and 48000. }
  \item{bits}{ the sample quality (number of bits per sample), 8 or 16. }
  \item{channels}{ 1 for mono, or 2 for stereo. }
}
\value{
  a Sample object.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\note{ Future versions may use a special NULLSample flag instead of using a sample of length 1. }

\seealso{ \code{\link{Silence}}}

\examples{\dontrun{
scale <- 2^(seq(0,1,length=13))[c(1,3,5,6,8,10,12,13)]
base <- 440
s <- nullSample()
for (f in scale)
  s <- appendSample(s,Sine(f*base,1))
play(s)
}}
\keyword{ sysdata }