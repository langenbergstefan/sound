\name{normalize}
\alias{normalize}
\title{ Rescale the Range of a Sample to [-1,1] }
\description{
  Multiply the waveform of a Sample object or a wav file with a positive constant so that the maximum absolut value becomes 1, or any other specified constant.

Use this command before saving or playing a Sample object to avoid cracks in the sound caused by parts in the waveform that exceed the range [-1,1]. 
}
\usage{normalize(s, level=1)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{level}{ a number between 0 and 1 specifying the desired maximum absolute value of the waveform. }
}
\value{
  a Sample object with the specified maximum absolut value of the waveform.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{Ops.Sample}}, \code{\link{center}} }

\examples{\dontrun{
s <- .6*Sine(440,1)
plot(s)
plot(normalize(s)) # now it uses the full range
play(s)
play(normalize(s)) # this one is louder
}}
\keyword{ manip }
