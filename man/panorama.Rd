\name{panorama}
\alias{panorama}
\title{ Narrow the Panorama of a Stereo Sample }
\description{
  Narrow the panorama of a stereo Sample object or of a stereo wav file.
}
\usage{panorama(s, pan)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{pan}{ a number between -50 and 50 giving the width of the panorama. }
}
\details{
If \code{abs(pan)<50}, mixtures of the two channels of \code{s} are used for the left and the right channel of the returned Sample object, so that they appear closer to the center. For \code{pan=0}, both sounds are completely in the center.

If \code{pan<0}, the left and the right channel are interchanged afterwards.
}
\value{
  a Sample object with the transformed panorama.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{mirror}} for \code{pan=-50}, \code{\link{left}} and \code{\link{right}} for access to single channels of the sample. }

\examples{\dontrun{
s <- stereo(Sine(440,1),Sine(330,1))
play(s)
play(panorama(s,30))  # now right and left tones are closer to the center
play(panorama(s,10))  # now even closer
play(panorama(s,0))   # now both at the center, the same as setChannels(s,1)
play(panorama(s,-30)) # again wider, but both sides switched
play(panorama(s,-50)) # the same as mirror(s)
}}
\keyword{ manip }
