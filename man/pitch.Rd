\name{pitch}
\alias{pitch}
\title{ Pitch a Sample Object }
\description{
  Change the pitch of a Sample object or a wav file.
}
\usage{pitch(s, semitones)
}
\arguments{
  \item{s}{ a Sample object, or a string giving the name of a wav file. }
  \item{semitones}{ a double giving the number of semitones to be pitched. }
}
\details{
Pitching +12 semitones (+1 octave) means to double the frequencies. Negative values of \code{semitones} are used to lower the frequencies.

Note that this transformation changes the actual data of the sample. Since pitching a sample is equivalent to playing it at a different speed, the length of the Sample object will also change.
}

\value{
  a Sample object.
}

\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\note{ Future versions of this command may use a different algorithm to increase the quality of the returned sample. }

\examples{\dontrun{
s <- Sine(440,1)
# Now play it 12 semitones = 1 octave deeper,
# that is half the frequencies and twice the length,
# or played at half speed.
play(pitch(s,-12)) # is the same as...
play(Sine(220,2))
}}
\keyword{ manip }
