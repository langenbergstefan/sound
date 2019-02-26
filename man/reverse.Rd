\name{reverse}
\alias{reverse}
\title{ Play a Sample Object Backwards }
\description{
  Returns the Sample object (or wav file) played backwards.
}
\usage{reverse(s)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
}
\value{
  a Sample object with the same parameters but with the sound played backwards.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\examples{\dontrun{
waveform <- 2*((seq(0,80,length=88200)\%\%1^2)-.5)
s <- as.Sample(waveform,44100,16)
play(s)
play(reverse(s)) # now played backwards
}}
\keyword{ manip }
