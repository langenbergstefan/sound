\name{sound-package}
\alias{sound-package}
\docType{package}
\title{
\packageTitle{sound}
}
\description{
\packageDescription{sound}
}
\details{
The basic object of this package is a variable of the new class \link{Sample}
with properties for
\itemize{
   \item the sampling rate.
   \item the number of bits per sample.
   \item the waveform matrix itself, which is a matrix with one or two rows
   (for mono or stereo sounds), each row representing one channel.
   The rows are sequences of values in [-1, 1] that discretize the waveform of the sound.
}
Sample objects can be created with the command \link{as.Sample}
\preformatted{sample <- as.Sample(sound,rate,bits)}
where \link{sound} is the waveform matrix of the sample.
Alternatively, one can use \link{loadSample}
\preformatted{sample <- loadSample(filename)}
to load a wav-file on the hard disk and convert it into a Sample object. Similarly, 
one can save a Sample object as a wav-file, using \link{saveSample}
\preformatted{saveSample(sample,filename)}
}
\author{
\packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}
\references{
M. Heymann and M. Hansen, A new set of sound commands for R; Sonification of the HMC
algorithm, in ASA Proceedings, Statistical Computing Section, pp. 1439-1443, 2002.
\url{http://www.matthiasheymann.de/Download/Sonification.pdf}
}
\keyword{package}
\keyword{file}
\seealso{
   package \CRANpkg{tuneR}.
}
\examples{
   sample1 <- Sine(440, 10) # create a sin waveform sample with 440 Hz
   sample2 <- Sine(1000, 10) # create a sin waveform sample with 1000 Hz
   sample1 + sample2 # adding two waveforms
   appendSample(sample1, sample2) # merging waveforms
}
