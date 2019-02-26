\name{channels}
\alias{channels}
\alias{channels<-}
\alias{setChannels}
\title{ Number of Channels of a Sample Object }
\description{
  Get or set the number of channels of a Sample object or a wav file.
}
\usage{channels(s)
channels(s) <- value
setChannels(s,value)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{value}{ 1 for mono, or 2 for stereo. }
}
\details{
The replacement form can be used to reset the number of channels of a Sample object (here, filenames are not accepted).

If a mono sample is transformed into a stereo sample, each channel of the stereo sample equals the waveform of the mono sample. If a stereo Sample is transformed to a mono sample, \code{(left(s)+right(s))/2} is returned.
}
\value{
  For \code{channels}, the number of channels of the sample (1 for mono, 2 for stereo).

  For \code{setChannels}, a Sample object with the new \code{channels} parameter. 
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{fitSampleParameters}} }

\examples{\dontrun{
s <- stereo(Sine(440,1),Sine(220,1))
channels(s)  # 2
play(s)
channels(s) <- 1  # now a mono sample
play(s)
}}
\keyword{ attribute } 
