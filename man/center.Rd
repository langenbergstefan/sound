\name{center}
\alias{center}
\title{ Center a Sample Object. }
\description{
This function adds a constant to a Sample object's waveform, so that its mean gets zero. This makes sense especially for sonification purposes, when (in general non-centered) data is transformed into sound.
}
\usage{center(s)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
}
\details{
For a stereo Sample object, both channels are treated separately.
}
\value{
  a Sample object with zero as the mean of each channel's waveform.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{normalize}} }

\examples{\dontrun{
x <- seq(0,50*pi,length=10000)
waveform <- (sin(x))^2 + .6*cos(x/2)^2
s <- as.Sample(waveform,44100,16)
plot(s) # nice idea, but wrong range for a sample
play(s) # sounds ugly, too
s <- center(s)
plot(s)  # now zero is the mean
play(s)  # sounds good, but too quiet
s <- normalize(s)
plot(s)  # this looks like a perfect sample!
play(s)  # e voila!
}}
\keyword{ manip }