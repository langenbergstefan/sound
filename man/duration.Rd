\name{duration}
\alias{duration}
\alias{duration<-}
\alias{setDuration}
\title{ Duration of a Sample Object }
\description{
  Get or set the duration (in seconds) of a Sample object or a wav file.
}
\usage{duration(s)
duration(s) <- value
setDuration(s,value)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{value}{ a double giving the duration in seconds. }
}
\details{
The replacement form can be used to reset the duration of the Sample object (here, filenames are not accepted).

If a Sample object is shortened, extra values are discarded. When a Sample object is lengthened, it is padded out to its new length with zeros (silence).
}
\value{
  For \code{duration}, the duration of the sample in seconds.

  For \code{setDuration}, a Sample object with the new duration. 
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{sampleLength}} }

\examples{\dontrun{
s <- Sine(440,3)
duration(s)  # 3
duration(s) <-.5  # sample is now .5 sec long
play(setDuration(s,1)) # plays a .5 sec sine wave and then .5 sec silence
}}
\keyword{ attribute }
