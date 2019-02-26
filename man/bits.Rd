\name{bits}
\alias{bits}
\alias{bits<-}
\alias{setBits}
\title{ Bits per Sample }
\description{
  Get or set the \code{bits} parameter (the sampling quality) of a Sample object or a wav file.
}
\usage{bits(s)
bits(s) <- value
setBits(s,value)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{value}{ the number of bits per sample: 8, 16 or 24. }
}
\details{
The replacement form can be used to reset the sampling quality of a Sample object, that is the number of bits per sample (8 or 16). Here, filenames are not accepted.
}
\value{
  For \code{bits}, the bits parameter (number of bits per sample) of the Sample object (8, 16 or 24).

  For \code{setBits}, a Sample object with the new \code{bits} parameter. 
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}
\note{ Changing the sampling quality of a Sample object does not affect its actual data but only its \code{$bits} flag. The sampling quality is only used when a Sample object is played or saved to disk. Internally, \R always uses doubles for the waveform.

An 8 bit sample needs only half the disk space compared to a 16 bit sample, but it has a lower sound quality.

Note also that 24 bit samples cannot be played by every wav file player.
}

\seealso{ \code{\link{fitSampleParameters}} }

\examples{\dontrun{
s <- Sine(20000,1,rate=44100,bits=16)
play(s)
print(s)
bits(s) <- 8
play(s)  # now worse quality
print(s) # but less disk space
play(setBits(s,16)) # now better quality again, since waveform data was not changed.
}}
\keyword{ attribute }
