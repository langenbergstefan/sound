\name{Ops.Sample}
\alias{Ops.Sample}
\alias{sum.Sample}
\alias{prod.Sample}
\alias{+}
\alias{-}
\alias{*}
\alias{/}
\title{ Basic Operations for Sample Objects }
\description{
These functions apply the basic operations pointwise to the waveforms of the samples.
}
\usage{e1 + e2
\method{Ops}{Sample}(e1,e2)
e1 - e2
\method{sum}{Sample}(e1, e2, ...)
\method{prod}{Sample}(e1, e2, ...)
}
\arguments{
  \item{e1, e2, \dots}{ For \code{+},\code{-},\code{*},\code{/}, Sample objects. For \code{prod.Sample} and \code{sum.Sample}, Sample objects or strings giving the names of wav files. }
}
\details{
The sum of two Sample objects corresponds to the sound when both samples are played at a time. The product of two samples causes an effect called ringmodulation, but it can also be used to add some vibrato to a sound (see the examples).

If the samples have different sample parameters (bits, rate and channels), the system uses the command \code{\link{fitSampleParameters}} to adjust them before the waveforms are combined.

Be careful to make sure that the resulting waveform does not exceed the interval [-1,1] when it is played or saved to disk, otherwise you will lose information and hear cracks in the sound. To avoid this, you can use \code{const * s} or the \code{\link{normalize}} command.

In \code{prod} and \code{sum} also the names of wavefiles can be used. Other forms like \code{e1+e2} do not accept filenames as arguments. If the first argument \code{e1} is a filename, the explicit forms \code{sum.Sample} and \code{prod.Sample} must be used.
}

\value{
  a Sample object. 
}

\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{normalize}}, \code{\link{center}} }

\examples{\dontrun{
const <- 2.0
e1 <- Sine(440,1)
e2 <- Sine(220,1)
play((e1+e2)/2) # both samples at a time
play(Sine(440,1)*Sine(5,1)) # vibrato

e1 * e2
const * e1
e1 * const
e2 / const

}}
\keyword{ manip }