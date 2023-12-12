\name{sound}
\alias{sound}
\alias{sound<-}
\title{ The Waveform Matrix of a Sample Object }
\description{
  Get or set the waveform matrix of a Sample object or a wav file.
}
\usage{sound(s)
sound(s) <- value
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{value}{ a \code{channels(s)} x \code{sampleLength(s)} matrix of doubles. }
}
\details{
The replacement form can be used to reset the waveform of a sample object. Here, filenames are not accepted for \code{s}.

The matrix can have one (for mono samples) or two rows (for stereo samples), where in the latter case the first row corresponds to the left and the second row to the right channel.

It contains the waveform(s) of the Sample object as sequence(s) of numbers between -1 and 1. \code{waveform} can contain arbitrary real numbers, but when the Sample object is played or saved to disk, [-1,1] is regarded as the native range of the sample, and any values outside this interval will cause cracks in the sound.

The waveform of a Sample object might exceed this interval during calculations. It is the task of the programmer to take care about the range of the waveform before saving or playing the sample, for example by using the \code{\link{normalize}} function.
}
\value{
  the waveform matrix of the sample.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}


\seealso{\code{\link{as.Sample}}}

\examples{\dontrun{
s <- Sine(440,1,channels=2)  # stereo sine wave
sound(s)[2,] <- sound(s)[2,]*seq(1,0,length=sampleLength(s))
play(s)  # right channel fades to zero
}}
\keyword{ attribute }
