\name{play}
\alias{play}
\alias{play.Sample}
\alias{play.default}
\title{Play a Sample Object or a WAV File}
\description{
  Play a Sample object or a wav file, using the wav file play command returned by \code{\link{WavPlayer}}, or any other specified command.
}
\usage{play(s, stay=FALSE, command=WavPlayer())
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{stay}{ logical. If TRUE, the Windows Media Player won't be closed after playing the sample. }
  \item{command}{ a character string giving the system command to be used for playing the sample. }
}
\details{
If \code{s} is a Sample object, it will be saved into a temporary folder before it is played. The temporary file will only be deleted afterwards if \code{stay=FALSE}.
}

\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\examples{\dontrun{
s <- Sine(440,1)
play(s)
}}
\keyword{ IO }
