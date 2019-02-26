\name{loadSample}
\alias{loadSample}
\title{ Load a WAV File from Disk }
\description{
  Load a wav file from disk and create a Sample object.
}
\usage{loadSample(filename, filecheck=TRUE)
}
\arguments{
  \item{filename}{ a string giving the path and the name of the wav file. }
  \item{filecheck}{ logical. If FALSE, no check for existance and read permission of the file will be performed. }
}
\details{
All kinds of wav files are supported: mono / stereo, 8 / 16 bits per sample, 1000 to 48000 samples/second.
}
\value{
  the Sample object that is equivalent to the wav file.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\note{ \code{filename} can also be a Sample object. In this case, the same object will be returned immediately. This can be useful when writing functions that accept both Sample objects and the names of a wav file as an argument. See \code{\link{is.Sample}} for an example. }

\seealso{  \code{\link{saveSample}}, \code{\link{as.Sample}} }

\examples{\dontrun{
s <- loadSample("soundfile.wav")
play(s)
}}

\keyword{ file }