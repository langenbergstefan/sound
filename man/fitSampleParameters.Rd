\name{fitSampleParameters}
\alias{fitSampleParameters}
\title{ Adjust Parameters of Two Sample Objects. }
\description{
  Adjust the parameters sampling rate, channels and bits/sample of two Sample objects.
}
\usage{fitSampleParameters(s1, s2)
}
\arguments{
  \item{s1,s2}{ a Sample object, or strings giving the name of a wav file. }
}
\details{ The commands \code{\link{rate}}, \code{\link{channels}} and \code{\link{bits}} are used to transform copies of \code{s1} and \code{s2} to samples with the same parameters rate, channels and bits. Always the parameter with the better quality is chosen for the returned samples, that is the higher sampling rate, the larger number of channels and the larger number of bits per sample.
}
\value{
  a list containing the two transformed Samples as components.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\note{ This routine is called before certain commands such as \code{\link{sum.Sample}} or \code{\link{appendSample}} are applied to Sample objects with different parameters. }

\seealso{ \code{\link{rate}}, \code{\link{channels}}, \code{\link{bits}} }

\examples{\dontrun{
s1 <- Sine(440,1,rate=22050,channels=1,bits=16)
s2 <- Sawtooth(440,1,rate=44100,channels=2,bits=8)
play(s1)
play(s2)
l  <- fitSampleParameters(s1,s2)
t1 <- l[[1]]
t2 <- l[[2]]
print(t1)
print(t2)  # both samples have the same parameters now
play(t1)
play(t2)   # none of the samples sounds different now,
           # since only parameters with higher quality were chosen
}}
\keyword{ manip }
