\name{appendSample}
\alias{appendSample}
\title{ Append Sample Objects }
\description{
  Append two or more Sample objects or wav files.
}
\usage{appendSample(s1, s2, ...)
}
\arguments{
  \item{s1, s2, \dots}{ Sample objects, or the names of wav files. }
}
\details{
If the samples have different sample parameters (bits, rate and channels), the command \code{\link{fitSampleParameters}} is called to adjust them before the samples are appended.
}
\value{
  a Sample object with the samples played one after the other.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{cutSampleEnds}} to avoid cracks between two appended samples,

\code{\link{sum.Sample}} for playing several samples at a time. }

\examples{\dontrun{
s1 <- Sine(440,1)
s2 <- Sine(550,1)
s3 <- Sine(660,1)
s4 <- Sine(880,1)
play(appendSample(s1,s2,s3,s4))
}}
\keyword{ manip }
