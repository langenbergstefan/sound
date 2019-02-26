\name{Sine}
\alias{Sine}
\alias{Sawtooth}
\alias{Square}
\alias{Silence}
\alias{Noise}
\title{ Create Sample Objects for the Basic waveforms }
\description{
  Create a Sample object with a sine, sawtooth, or square waveform, silence, or noise.
}
\usage{Sine(freq, dur, rate=44100, bits=16, channels=1)
Sawtooth(freq, dur, rate=44100, bits=16, channels=1, reverse=FALSE)
Square(freq, dur, rate=44100, bits=16, channels=1, upPerc=50)
Silence(dur, rate=8000, bits=8, channels=1)
Noise(dur, rate=44100, bits=16, channels=1)
}
\arguments{
  \item{freq}{ the frequency (a double). }
  \item{dur}{ the duration in seconds (a double). }
  \item{rate}{ the sampling rate, an integer between 1000 and 48000. }
  \item{bits}{ the sampling quality in bits per sample, 8 or 16. }
  \item{channels}{ 1 for mono, or 2 for stereo. }
  \item{reverse}{ logical. If \code{TRUE}, the waveform will be mirrored vertically. }
  \item{upPerc}{ a number between 0 and 100 giving the percentage of the waveform with value +1. }
}
\details{
 If \code{channels=2}, left and right side of the sample will be the same for \code{Sine}, \code{Sawtooth} and \code{Square}. For \code{Noise}, both sides will be generated separately, using \code{runif}.
}
\value{
  a Sample object.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{as.Sample}}, \code{\link{loadSample}}, \code{\link{nullSample}} }

\examples{\dontrun{
s1 <- Sine(440,1)
play(s1)

s2 <- Sawtooth(440,1)
play(s2)

play(Noise(1))
}}
\keyword{ sysdata }
