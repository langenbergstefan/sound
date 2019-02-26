\name{Sample}
\alias{Sample}
\alias{as.Sample}
\alias{is.Sample}
\title{ Sample Objects }
\description{
  \code{as.Sample} creates a Sample object from a given numeric matrix.

  \code{is.Sample} tests if its argument is a Sample object or the name of a wav file.
}
\usage{as.Sample(sound, rate, bits)
is.Sample(s, argname="'s' ")
}
\arguments{
  \item{sound}{ a \code{channels(s)} x \code{sampleLength(s)} matrix or a vector of doubles describing the waveform(s) of the sample. }
  \item{rate}{ the sampling rate (number of samples per second). }
  \item{bits}{ the sampling quality (the number of bits per sample), 8 or 16. }
  \item{s}{ an R object to be tested.}
  \item{argname}{ a string giving the name of the object that is tested. It is used for creating an error message. }
}

\details{ The rows of the matrix represent the channels of the sample: If \code{sound} is a vector or a matrix with only one row,
\code{as.Sample} will return a mono sample; if \code{sound} is a matrix with two rows, \code{as.Sample} returns a stereo sample, where the left and the right channel are represented by the first and the second row, respectively.

\code{sound} can contain any real number, but when the Sample object is played or saved to disk, [-1,1] is regarded as the range of the sample, and any values outside this interval will cause cracks in the sound.

A Sample object's waveform can exceed this interval during calculations. It is the task of the programmer to take care of the range of the waveform before saving or playing the sample, for example by using the \code{\link{normalize}} command.

Internally, the sound is saved as a matrix with doubles, independent of the \code{bits} parameter that is only used when the Sample object is played or saved to disk.

The \code{is.Sample} command is used by several other routines that allow both Sample objects and filenames as arguments.
}

\value{
  For \code{as.Sample} a Sample object, that is a list with the components \code{$sound}, \code{$rate} and \code{$bits}.

  \code{is.Sample} returns a list with the entries
  \item{test }{a logical indicating whether or not \code{s} is a Sample object or the name of a valid wav file.}
  \item{error }{a string with one of the messages "Filename must have the extension .wav.", "File not found.", "No read permission for this file.", or "Argument "+ argname + "must be a Sample object or the name of a wav file." If \code{test=TRUE}, this list entry doesn't exist.}
}

\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{stereo}} for creating a stereo Sample object from two mono Sample objects, \code{\link{loadSample}} for loading a wav file and turning it into a Sample object, \code{\link{saveSample}} for saving a Sample object as a wav file, \code{\link{sound}}, \code{\link{bits}}, \code{\link{rate}}, \code{\link{channels}}, \code{\link{sampleLength}} and \code{\link{duration}} for access to the basic parameters of a Sample object.
}

\examples{\dontrun{
waveLeft <- 2*((seq(0,80,length=88200)\%\%1^2)-.5)
s <- as.Sample(waveLeft,44100,16)
play(s) # a mono sample

waveRight <- waveLeft[88200:1]
s <- as.Sample(rbind(waveLeft,waveRight),44100,16)
play(s) # a stereo Sample

# How to use is.Sample to allow both a Sample object and a filename
# as an argument:
x <- anyargument
sampletest <- is.Sample(x, argname="'x' ")
if (!sampletest$test) stop(sampletest$error) #no valid argument
x <- loadSample(x,filecheck=FALSE)
# If x is Sample object, loadSample will return it immediately.
# If x is a string, the Sample object will be loaded from disk.
# No check for existence of the file will be performed since this
# was already tested in is.Sample.
#
# Now x is a Sample object, continue with code.
}}

\keyword{ classes }
