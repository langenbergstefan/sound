\name{WavPlayer}
\alias{WavPlayer}
\alias{findWavPlayer}
\alias{setWavPlayer}
\title{ Set or Get the System Command for Playing WAV Files }
\description{
  \code{findWavPlayer} returns the most common system commands on your OS for playing wav files.

  \code{WavPlayer} returns the command that is currently used by \code{\link{play}}.

  \code{setWavPlayer} is used to define the command to be used by \code{\link{play}}.
}
\usage{findWavPlayer()
WavPlayer()
setWavPlayer(command=NULL)
}
\arguments{
  \item{command}{ a vector of character strings giving the command to be used as \code{"command wavfile.wav"}. If it contains more than one string, the commands are tested one after the other, and the first one that works properly will be used for future calls of the \code{\link{play}} command. If \code{command=NULL},the command \code{findWavPlayer()} is used to determine the standard commands for your system. }
}
\details{ The \code{\link{play}} command makes a system call of the form \code{"}command\code{ }wavfile\code{.wav"}, where 'command' is the string returned by \code{WavPlayer()}.

The default commands are \code{'mplay32 /play'} (calling the Windows media player) for Win32-systems and \code{'aplay'} and \code{'mplayer'} for Linux systems. Other commands will be added in future versions of this package.

Unfortunately for Windows 7/8/10 no command line tool for playing wav-files is included into the os. However, a suitable tool like \code{'wv_player.exe'} can be installed. To download it, go to \url{http://www.webxpace.com/software/}.

But any other program that provides a system call of the above form to play wav files is also fine. Please report additional play commands to the maintainer so that they can be recognized automatically in future versions of this package.

When \code{setWavPlayer} is called, it tries to play an empty wav file, using the new command(s). If it fails, no changes are made.
}
\value{
  \code{WavPlayer} returns the wav play command that is currently used, or \code{NULL}, if none is selected yet.

  \code{findWavPlayer} returns the default commands for your system, or \code{NULL}, if no command is known for your system.
}
\author{
Author: \packageAuthor{sound}

Maintainer: \packageMaintainer{sound}
}

\seealso{ \code{\link{play}} for playing Sample objects or wav files. }

\examples{\dontrun{
setWavPlayer("playwave")
# tries to set the command "playwave wavfile.wav" as the 
# preference for playing wav files with the play command.
# If successful,
WavPlayer()
# returns the string "playwave" afterwards.
}}
\keyword{ sysdata }