\name{cutSampleEnds}
\alias{cutSampleEnds}
\title{ Prepare Sample Object for appendSample }
\description{
  Prepare a Sample object or a wav file for usage of \code{\link{appendSample}} to avoid cracks between two appended samples.
}
\usage{cutSampleEnds(s)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
}
\details{
  At the beginning of the sample, all values in the waveform until the first transition from negative to positive values are dropped, at the end everything after the last transition from negative to positive values is dropped.

Currently, only channel 1 is used to determine which parts to drop. Hence in stereo samples there can still be some cracks in the right channel.
}

\value{
  a Sample object.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{cutSample}}, \code{\link{appendSample}} }

\examples{\dontrun{
s1 <- Sine(440,.01)
s2 <- Sine(550,.01)
s3 <- Sine(660,.01)
s4 <- Sine(880,.01)
l <- list(s1,s2,s3,s4)
#  first without cutSampleEnds:
s <- nullSample()
for (i in 1:99) {
  s <- appendSample(s,l[[i\%\%4+1]])
}
play(s) # ugly cracks
# now with cutSampleEnds:
s <- nullSample()
for (i in 1:99) {
  s <- appendSample(s,cutSampleEnds(l[[i\%\%4+1]]))
}
play(s) # no cracks,

# This is how it works:
# The waveform is not smooth between s1 and s2:
plot(appendSample(s1,s2))
# This is because s1 just ends somewhere at y=0.6:
plot(s1)
# Let's cut off the last positive part of it:
plot(cutSampleEnds(s1))
# A similar cuttoff would be made at the beginning
# of the sample (if it was necessary).
# Now the two samples fit perfectly (the cut is at x=400):
plot(appendSample(cutSampleEnds(s1),cutSampleEnds(s2)))
}}

\keyword{ manip }
