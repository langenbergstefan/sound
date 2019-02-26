\name{sampleLength}
\alias{sampleLength}
\alias{sampleLength<-}
\alias{sampleLength<-.Sample}
\alias{setSampleLength}
\title{ Length of a Sample Object }
\description{
  Get or set the length (number of columns in the waveform matrix) of a Sample object or a wav file.
}
\usage{sampleLength(s)
sampleLength(s) <- value
setSampleLength(s,value)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{value}{ an integer giving the sample length (number of columns in the waveform matrix). }
}
\details{
The replacement form can be used to reset the sample length (here, filenames are not accepted).

If a Sample object is shortened, extra values are discarded. When a Sample object is lengthened, it is padded out to its new length with zeros (silence).
}
\value{
  For \code{sampleLength}, the number of columns in the waveform matrix of the sample.

  For \code{setSampleLength}, a Sample object with the new length.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{duration}} }

\examples{\dontrun{
s <- Sine(440,3,rate=44100,bits=16,channels=2)
sampleLength(s)  # 132300 samples ( = 3 sec * 44100 samples/sec )
sampleLength(s) <- 22050  # sample is now .5 sec long
play(setSampleLength(s,44100)) # plays a .5 sec sine wave and then .5 sec silence
}}
\keyword{ attribute }
