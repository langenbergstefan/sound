\name{plot.Sample}
\alias{plot.Sample}
\title{ Plot a Sample Object }
\description{
  Plot the waveform of a Sample object or a wav file.
}
\usage{\method{plot}{Sample}(x,xlab="sample #",ylab=NULL,\dots)
}
\arguments{
  \item{x}{ a Sample object, or a string giving the name of a wav file. If \code{x} is a string, the explicit form \code{plot.Sample} must be used. }
  \item{xlab}{ the character string giving the label for the x-axis.}
  \item{ylab}{ For mono Sample objects as usual. For stereo Sample objects, \code{ylab} can be a vector of two strings to distinguish the y-labels for the left and the right channel. If \code{ylab=NULL}, the presets are used, that is \code{"waveform"} for mono samples and \code{c("left","right")} for stereo samples.}
  \item{\dots}{ further graphical parameters. }
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\note{ Use \code{plot(s[interval])} to plot parts of \code{s} only (see examples).

If the range of the graph exceeds [-1,1], you can use the \code{\link{normalize}} command before plotting to get a better view of the waveform. (Then you should also call this function to avoid cracks in the sound before you save or play it the next time.) }

\seealso{ \code{\link{print.Sample}} }

\examples{\dontrun{s <- Sine(440,1) + .4*Sine(1000,1)
plot(s[1:1000])
play(s)
s <- normalize(s)
plot(s[1:1000])  # now the range of the waveform is in [-1,1]
play(s)  # no cracks!}
}
\keyword{ hplot }
