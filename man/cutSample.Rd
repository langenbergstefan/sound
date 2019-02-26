\name{cutSample}
\alias{cutSample}
\alias{[.Sample}
\title{ Cut Sample Objects }
\description{
  Cut a part out of a Sample object.
}
\usage{
cutSample(s, start, end)
\S3method{[}{Sample}(s,i)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{start}{ the start position in seconds. }
  \item{end}{ the end position in seconds. }
  \item{i}{ a vector of integers giving the numbers of the columns in the waveform matrix to be used. }
}
\details{
Only the intersection of [\code{start},\code{end}] with [0,\code{duration(s)}] is returned. Similarly, in the second form the intersection of \code{v} with \code{1:sampleLength(s)} is returned.
}
\value{
  the specified part of the given sample as a new Sample object.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{sound}} for direct access to the waveform matrix,

\code{\link{cutSampleEnds}} and \code{\link{noSilence}} for special cutoff techniques. }

\examples{\dontrun{
s <- appendSample(Sine(330,1),Sine(440,1))
play(cutSample(s,.8,1.8))
play(s[(44100*.8):(44100*1.8)])  # the same
}}
\keyword{ manip }
