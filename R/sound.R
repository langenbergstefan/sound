setWavPlayer <- function(command=NULL){
  flag <- command
  if (is.null(flag)) command <- findWavPlayer()
  if (is.null(command)) {
    warning(paste("To play sounds you need to select a wav file player first.\n",
                  "For more information type '?setWavPlayer'.\n", sep=""))
  }
  else {
    if (!is.character(command)) stop("Argument 'command' must be a vector of character strings.")
    for (trycommand in command){
      op <- options(warn=2)
      if (trycommand=="mplay32 /play") {
        status <- try(
          system2(paste(trycommand," /close ", sep=""),
          	paste(path.package(package = "sound", quiet=TRUE),"/testsample.wav",sep=""),
          	stdout=NULL)
        )
      }
      else {
        status <- try(
          system2(trycommand, paste(path.package(package = "sound", quiet=TRUE),"/testsample.wav",sep=""),
          stdout=NULL)
      )
      }
      options(op)
      if (!inherits(status,"try-error")) {
        options(wavplayer=trycommand)
        return(paste("WAV file player '",trycommand,"' linked successfully.",sep=""))
      }
    }
    singplur <- sum(length(command)>1)+1
    if (is.null(flag)) {
      warning(paste("======================================================================\n",
                    "Couldn't run the default wav file play commands for your system.\n",
                    "Make sure they can be used as 'command soundfile.wav' and\n",
                    "then type 'setWavPlayer()'.\n",
                    "For more information - also about selecting an individual wav file player -\n",
                    "type '?setWavPlayer'.\n",
                    "======================================================================\n", sep="") )
    }
    else {
      warning(paste("============================================================================\n",
                    "Couldn't run ",c("the given command","any of the given commands")[singplur],".\n",
                    "Make sure ",c("it","they")[singplur]," can be used as 'command soundfile.wav' and then ",
                    "try it again.\nFor more information type '?setWavPlayer'.\n",
                    "============================================================================\n", sep="") )
    }
  }
  invisible(NULL)
}

WavPlayer <- function() {
  return(options()$wavplayer)
}

findWavPlayer <- function(){
  command <- switch(R.Version()$os,
	         "linux-gnu"    = c("aplay","mplayer"),
 	         "Win32"        = "mplay32 /play",
 	         "darwin16.7.0" = "afplay",
		    	"darwin17.2.0" = "afplay",
            default        = NULL)
  if (is.null(command)) warning("No standard wav player known for your system.\n")
  return(command)
}

#=========================================================================
#                             create objects
#=========================================================================

as.Sample <- function(sound,rate=44100,bits=16){
  if (mode(sound)!="numeric")
    stop("Argument 'sound' must be a numeric vectors.")
  if (mode(rate)!="numeric" || rate<1000 || rate>192000)
    stop("Parameter 'rate' must be an number between 1000 and 48000.")
  if (mode(bits)!="numeric" || bits!=8 && bits!=16 && bits!=24)
    stop("Parameter 'bits' must be 8, 16 or 24.")
  if (is.null(dim(sound)))
    sound <- matrix(sound,nrow=1)
  if (dim(sound)[1]>2){
    warning("Argument 'sound' has more than two rows. Only the first two are used.")
    sound <- sound[1:2,]
  }
  Sample <- list(sound=sound,rate=rate,bits=bits)
  class(Sample)<-"Sample"
  return(Sample)
}

#=========================================================================
#                        check for Sample object
#=========================================================================

is.Sample <- function(s,argname="'s' "){
  if (!is.null(class(s)) && is(s, "Sample")) return(list(test=TRUE))
  if (mode(s)!="character") return(list(test=FALSE,
      error=paste("Argument ",argname,"must be a Sample object or the name of a wav file.",sep="")))
  if (substr(s,nchar(s)-3,nchar(s))!=".wav") return(list(test=FALSE,error="Filename must have the extension .wav."))
  if (file.access(s)==-1) return(list(test=FALSE,error="File not found."))
  if (file.access(s,4)==-1) return(list(test=FALSE,error="No read permission for this file."))
  else return(list(test=TRUE))
}

#=========================================================================
#                           load / save samples
#=========================================================================

loadSample <- function(filename,filecheck=TRUE){
  if (!is.null(class(filename)) && is(filename, "Sample")) return(filename)
  if (mode(filename)!="character")
    stop("Argument 'filename' must be a character string.")
  if (substr(filename,nchar(filename)-3,nchar(filename))!=".wav")
    stop("Filename must have the extension .wav.")
  if (filecheck){
    if (file.access(filename)==-1)
      stop("File not found.")
    if (file.access(filename,4)==-1)
      stop("No read permission for this file.")
  }

  fileR <- file(filename,"rb")

  if(readChar(fileR, nchars=4) != 'RIFF')
    stop("File is not RIFF format.")
  readBin(fileR, "integer", n = 4, size = 1)
  if(readChar(fileR, nchars=4) != 'WAVE')
    stop("File is not WAVE format.")

              readBin(fileR,"integer",n=10,size=1)
  channels <- readBin(fileR,"integer",     size=2, endian='little')
  rate     <- readBin(fileR,"integer",     size=4, endian='little')
              readBin(fileR,"integer",n= 6,size=1)
  bits     <- readBin(fileR,"integer",     size=2, endian='little')
              readBin(fileR,"integer",n= 4,size=1)
  Length   <- readBin(fileR,"integer",     size=4, endian='little')
  if (bits==8)
      data <- readBin(fileR,"integer",n=Length  ,size=1,signed=FALSE, endian='little')/128-1
  if (bits==16)
      data <- readBin(fileR,"integer",n=Length/2,size=2,signed=TRUE , endian='little')/32768
  if (bits==24) {
      data <- readBin(fileR,"integer",n=Length,size=1,signed=FALSE , endian='little')
      indices <- 3*1:(Length/3)
      data <- data[indices-2]/8388608 + data[indices-1]/32768 + data[indices]/128
      data <- data-2*(data>=1)
  }

  close(fileR)

  if (channels==2)
    dim(data) <- c(channels,length(data)/channels)

  return(as.Sample(data,rate,bits))
}

saveSample <- function(s,filename,overwrite=FALSE){
  if (is.null(class(s)) || !is(s, "Sample"))
    stop("Argument 's' must be of class 'Sample'.")
  if (mode(filename)!="character")
    stop("Argument 'filename' must be a character string.")
  if (substr(filename,nchar(filename)-3,nchar(filename))!=".wav")
    stop("Filename must have the extension .wav.")
  if (file.access(filename)==0) {
    if (overwrite==FALSE) {cat(filename,"\n"); stop("File exists.")}
    if (file.remove(filename)==FALSE)
      stop("File exists and is protected against deletion.")
  }

  if (channels(s)==1) {data<-sound(s)[1,]}
    else  {data <- array(sound(s),dim=c(1,2*sampleLength(s)))}

  dataLength <- length(data)*bits(s)/8

  if (bits(s)==8)
      data <- data*127+128
  if (bits(s)==16)
      data <- data*32767
  if (bits(s)==24) {
      data <- 128*(data + 2*(data<0))
      hiByte <- floor(data)
      data <- 256*(data-hiByte)
      midByte <- floor(data)
      lowByte <- floor((data-midByte)*256)
      data <- matrix(matrix(c(lowByte,midByte,hiByte),3,length(data),byrow=TRUE),1,3*length(data))
  }

  fileW <- file(filename,"wb")
  writeChar("RIFF",fileW,eos=NULL)                                            # "RIFF"
  close(fileW)

  fileA <- file(filename,"ab")
  writeBin(as.integer(36+dataLength),fileA,endian='little')                   # number of following bytes
  writeChar("WAVEfmt ",fileA,eos=NULL)                                        # "WAVE"; "fmt "
  writeBin(as.integer(16),fileA, endian='little')                             # always 16
  writeBin(as.integer(1),fileA,size=2, endian='little')                       # always 1
  writeBin(as.integer(channels(s)),fileA,size=2, endian='little')             # 1=mono / 2=stereo
  writeBin(as.integer(rate(s)),fileA, endian='little')                        # sample rate
  writeBin(as.integer(rate(s)*channels(s)*bits(s)/8),fileA, endian='little')  # bytes/second
  writeBin(as.integer(channels(s)*bits(s)/8),fileA,size=2, endian='little')   # bytes/sample
  writeBin(as.integer(bits(s)),fileA,size=2, endian='little')                 # bits/sample

  writeChar("data",fileA,eos=NULL)                                            # "data"
  writeBin(as.integer(dataLength),fileA, endian='little')                     # length of data in bytes
  if (bits(s)==8 || bits(s)==16)                                              # data
      writeBin(as.integer(data),fileA,size=bits(s)/8, endian='little')
  if (bits(s)==24)
      writeBin(as.integer(data),fileA,size=1, endian='little')
  close(fileA)
}

#=========================================================================
#                             play sample
#=========================================================================

play <- function(s,stay=FALSE,command=WavPlayer()){
  UseMethod("play")
}

play.default <- function(s,stay=FALSE,command=WavPlayer()){
  if (is.null(command)) {
    stop(paste("No wav file player selected.\n",
               "To play sounds you need to select a wav file player first.\n",
               "For more information type '?setWavPlayer'.", sep=""))
  }
  sampletest <- is.Sample(s,argname="")
  if (!sampletest$test) stop(sampletest$error)
  if (stay==FALSE && command=="mplay32 /play") command <- paste(command,"/close")
  system2(command,s)
  invisible(NULL)
}

play.Sample <- function(s,stay=FALSE,command=WavPlayer()){
  if (is.null(command)) setWavPlayer()
  command <- WavPlayer()
  if (is.null(command)) {
    stop(paste("No wav file player selected.\n",
               "To play sounds you need to select a wav file player first.\n",
               "For more information type '?setWavPlayer'.", sep=""))
  }
  filename <- paste(tempfile("tempSound"),".wav",sep="")
  saveSample(s,filename)
  play(filename,stay=stay,command=command)
  if (stay==FALSE) {file.remove(filename)}
  invisible(NULL)
}

#=========================================================================
#                         display sample data
#=========================================================================

print.Sample <- function(x,...){
  sampletest <- is.Sample(x,argname="'x' ")
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(x,filecheck=FALSE)
  cat("type      : ",c("mono","stereo")[channels(s)],"\n",
      "rate      : ",rate(s)," samples / second\n",
      "quality   : ",bits(s)," bits / sample\n",
      "length    : ",sampleLength(s)," sample",ifelse(sampleLength(s)==1,"\n","s\n"),
      "R memory  : ",sampleLength(s)*channels(s)*4," bytes\n",
      "HD memory : ",sampleLength(s)*channels(s)*bits(s)/8+44," bytes\n",
      "duration  : ",round(duration(s),3)," second",ifelse(round(duration(s),3)==1,"\n","s\n"),sep="")
  invisible(NULL)
}

plot.Sample <- function(x,xlab="sample #",ylab=NULL,...){
  sampletest <- is.Sample(x,argname="'x' ")
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(x,filecheck=FALSE)
  if (channels(s)==1) {
    if (is.null(ylab)) ylab <- "waveform"
    plot(sound(s)[1,],type="l",col="red" ,ylim=c(-1,1),xlim=sampleLength(s)*c(.04,1.04)/1.08,
         xlab=xlab,ylab=ylab,axes=FALSE,...)
    axis(1)
    axis(2,at=c(-1,0,1),labels=as.character(c(-1,0,1)))
    abline(h=0)
    abline(h=c(-1,1),lty="dashed")
    lines(par()$usr[1:2],y=c(rep(par()$usr[3],2)),xpd=TRUE)
  }
  else {
    op1 <- par(mfrow=c(2,1))
    op2 <- par(mar=c(2,4,4,2)+.1)
    if (is.null(ylab)) ylab <- c("left","right")   
    plot(sound(s)[1,],type="l",col="red",ylim=c(-1,1),xlim=sampleLength(s)*c(.04,1.04)/1.08,
         xlab="",ylab=ylab[min(2,length(ylab))],axes=FALSE,...)
    axis(1)
    axis(2,at=c(-1,0,1),labels=as.character(c(-1,0,1)))
    abline(h=0)
    abline(h=c(-1,1),lty="dashed")
    lines(par()$usr[1:2],y=c(rep(par()$usr[3],2)),xpd=TRUE)
    par(op2)

    op3 <- par(mar=c(5,4,1,2)+.1)
    plotwithoutmain <- function(main,...){
      plot(sound(s)[2,],type="l",col="blue",ylim=c(-1,1),xlim=sampleLength(s)*c(.04,1.04)/1.08,
           xlab=xlab,ylab=ylab[1],axes=FALSE,main="",...)
    }
    plotwithoutmain(...)
    axis(1)
    axis(2,at=c(-1,0,1),labels=as.character(c(-1,0,1)))
    abline(h=c(0,par()$usr[1]))
    abline(h=c(-1,1),lty="dashed")
    lines(par()$usr[1:2],y=c(rep(par()$usr[3],2)),xpd=TRUE)
    par(op3)
    par(op1)
  }
  invisible(NULL)
}

#=========================================================================
#                            cut sample
#=========================================================================

"[.Sample" <- function(s,i){
  if (mode(i)!="numeric")
    stop("Index must be numeric.")
  ch <- channels(s)
  wave <- sound(s)[1:ch,i[!is.na(sound(s)[1,i])]]
  dim(wave) <- c(ch,length(wave)/ch)
  sound(s) <- wave
  return(s)
}

cutSample <- function(s,start,end){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  if (mode(start)!="numeric" || mode(end)!="numeric" || start<0 || end<0)
    stop("Parameters 'start' and 'end' must be a numeric >=0.")
  end <- min(end,duration(s))
  if (start>end)
    stop("Argument 'start' is larger than argument 'end'.")
  if (start>duration(s)){
    warning("Argument start is larger than duration of sample. Empty sample returned.")
    return(nullSample(rate(s),bits(s),channels(s)))
  }
  return(s[1+as.integer(start*rate(s)):as.integer(end*rate(s))])
}

#=========================================================================
#                        read / set basic properties
#=========================================================================

sound    <- function(s) {
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  return(loadSample(s,filecheck=FALSE)$sound)
}

bits     <- function(s) {
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  return(loadSample(s,filecheck=FALSE)$bits)
}

rate     <- function(s) {
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  return(loadSample(s,filecheck=FALSE)$rate)
}

channels <- function(s) {
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  return(dim(loadSample(s,filecheck=FALSE)$sound)[1])
}

sampleLength <- function(s) {
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  return(length(sound(loadSample(s,filecheck=FALSE))[1,]))
}

duration <- function(s) {
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  if (is.null(class(s))) s <- loadSample(s,filecheck=FALSE)
  return(sampleLength(s)/rate(s))
}

"sound<-" <- function(s,value){
  s$sound <- value
  return(s)
}

"bits<-" <- function(s,value){
  if (is.null(class(s)) || !is(s, "Sample"))
    stop("Argument 's' must be of class 'Sample'.")
  if (mode(value)!="numeric" || (value!=8 && value!=16))
    stop("Number of bits must be 8 or 16.")
  else s$bits <- value
  return(s)
}

"rate<-" <- function(s,value){
  if (is.null(class(s)) || !is(s , "Sample"))
    stop("Argument 's' must be of class 'Sample'.")
  if (mode(value)!="numeric" || value<1000 || value>48000)
    stop("Rate must be an number between 1000 and 48000.")
  if (rate(s)==value) return(s)
  ch <- channels(s)
  sound(s) <- sound(s)[,as.integer(seq(1,sampleLength(s)+.9999,by=rate(s)/value))]
  dim(sound(s)) <- c(ch,length(sound(s))/ch)
  s$rate <- value
  return(s)
}

"channels<-" <- function(s,value){
  if (is.null(class(s)) || !is(s, "Sample"))
    stop("Argument 's' must be of class 'Sample'.")
  if (mode(value)!="numeric" || !(value==1 || value==2))
    stop("Number of channels must be 1 or 2.")
  if (channels(s)==value) return(s)
  else {
    if (value==1){
      sound(s) <- (sound(s)[1,] + sound(s)[2,])/2
      dim(sound(s)) <- c(1,length(sound(s)))
    }
    else {
      sound(s) <- rbind(sound(s),sound(s))
    }
    return(s)
  }
}

"sampleLength<-" <- function(s,value){
  UseMethod("sampleLength<-")
}

"sampleLength<-.Sample" <- function(s,value){
  if (is.null(class(s)) || !is(s,"Sample"))
    stop("Argument 's' must be of class 'Sample'.")
  if (mode(value)!="numeric" || value<1)
    stop("Sample length must be a positive integer.")
  if (sampleLength(s)>value){
      ch <- channels(s)
      sound(s) <- sound(s)[,1:value]
      dim(sound(s)) <- c(ch,value)
  }
  else sound(s) <- cbind(sound(s),matrix(0,channels(s),value-sampleLength(s)))
  return(s)
}

"duration<-" <- function(s,value){
  if (is.null(class(s)) || !is(s,"Sample"))
    stop("Argument 's' must be of class 'Sample'.")
  if (mode(value)!="numeric" || value<=0)
    stop("Duration must be a positive number.")
  sampleLength(s) <- as.integer(value*rate(s))
  return(s)
}

setBits <- function(s,value){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  if (mode(value)!="numeric" || (value!=8 && value!=16))
    stop("Number of bits must be 8 or 16.")
  if (is.null(class(s))) s <- loadSample(s,filecheck=FALSE)
  bits(s) <- value
  return(s)
}

setRate <- function(s,value){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  if (mode(value)!="numeric" || value<1000 || value>48000)
    stop("Rate must be a number between 1000 and 48000.")
  if (is.null(class(s))) s <- loadSample(s,filecheck=FALSE)
  rate(s) <- value
  return(s)
}

setChannels <- function(s,value){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  if (mode(value)!="numeric" || !(value==1 || value==2))
    stop("Number of channels must be 1 or 2.")
  if (is.null(class(s))) s <- loadSample(s,filecheck=FALSE)
  channels(s) <- value
  return(s)
}

setSampleLength <- function(s,value){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  if (mode(value)!="numeric" || value<1)
    stop("Sample length must be a positive integer.")
  if (is.null(class(s))) s <- loadSample(s,filecheck=FALSE)
  sampleLength(s) <- value
  return(s)
}

setDuration <- function(s,value){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  if (mode(value)!="numeric" || value<=0)
    stop("Duration must be a positive number.")
  if (is.null(class(s))) s <- loadSample(s,filecheck=FALSE)
  duration(s) <- value
  return(s)
}

fitSampleParameters <- function(s1,s2){
  sampletest <- is.Sample(s1,argname="s1 ")
  if (!sampletest$test) stop(sampletest$error)
  sampletest <- is.Sample(s2,argname="s2 ")
  if (!sampletest$test) stop(sampletest$error)
  if (is.null(class(s1))) s1 <- loadSample(s1,filecheck=FALSE)
  if (is.null(class(s2))) s2 <- loadSample(s2,filecheck=FALSE)
  rate(s1) <- max(rate(s1),rate(s2))
  rate(s2) <- max(rate(s1),rate(s2))
  channels(s1) <- max(channels(s1),channels(s2))
  channels(s2) <- max(channels(s1),channels(s2))
  bits(s1) <- max(bits(s1),bits(s2))
  bits(s2) <- max(bits(s1),bits(s2))
  return(list(s1,s2))
}

#=========================================================================
#                    binary operations: +, *, /, append
#=========================================================================

Ops.Sample <- function(e1,e2){
  if (.Generic=="+"){
    if (is.null(class(e1)) || is.null(class(e2)) || !is(e1,"Sample") || !is(e2, "Sample"))
      stop("Arguments must be of class 'Sample'.")
    s <- fitSampleParameters(e1,e2)
    e1 <- s[[1]]
    e2 <- s[[2]]

    m <- max(sampleLength(e1),sampleLength(e2))
    e1 <- setSampleLength(e1,m)
    e2 <- setSampleLength(e2,m)

    sound(e1) <- sound(e1) + sound(e2)

    return(e1)
  }

  if (.Generic=="-"){
    if (missing(e2)){
      sound(e1) <- -sound(e1)
      return(e1)
    }
    else return(e1+(-e2))
  }

  if (.Generic=="*"){
    if (mode(e2)=="numeric") return(e2*e1)
    else if (mode(e1)=="numeric"){
      sound(e2) <- e1*sound(e2)
      return(e2)
    }
    else if (!is.null(class(e1)) && !is.null(class(e2)) && is(e1, "Sample") && is(e2, "Sample")){
      s <- fitSampleParameters(e1,e2)
      e1 <- s[[1]]
      e2 <- s[[2]]

      m <- max(sampleLength(e1),sampleLength(e2))
      sampleLength(e1) <- m
      sampleLength(e2) <- m

      sound(e1) <- sound(e1) * sound(e2)

      return(e1)
    }
    else stop("Arguments must be numeric or of class 'Sample'.")
  }

  if (.Generic=="/"){
    if(mode(e2)!="numeric") stop("Second argument must be numeric.")
    else return(1/e2*e1)
  }
}

sum.Sample <- function(e1,e2,...){
  sampletest <- is.Sample(e1,argname="")
  if (!sampletest$test) stop(sampletest$error)
  if(missing(e2)) {
    return(loadSample(e1,filecheck=FALSE))
  }
  return(loadSample(e1,filecheck=FALSE)+sum.Sample(e2,...))
}

prod.Sample <- function(e1,e2,...){
  sampletest <- is.Sample(e1,argname="")
  if (!sampletest$test) stop(sampletest$error)
  if(missing(e2)) {
    return(loadSample(e1,filecheck=FALSE))
  }
  return(loadSample(e1,filecheck=FALSE)*prod.Sample(e2,...))
}

appendSample <- function(s1,s2,...){
  if (is.null(s1) && !missing(s2)) return(appendSample(s2,...))
  sampletest <- is.Sample(s1,argname="")
  if (!sampletest$test) stop(sampletest$error)
  if (missing(s2)){
    return(loadSample(s1,filecheck=FALSE))
  }
  s2 <- appendSample(s2,...)
  s  <- fitSampleParameters(loadSample(s1,filecheck=FALSE),s2)
  sound(s[[1]]) <- cbind(sound(s[[1]]),sound(s[[2]]))
  return(s[[1]])
}

#=========================================================================
#                          panorama operations
#=========================================================================

stereo <- function(sLeft,sRight,pan=50){
  sampletest <- is.Sample(sLeft,argname="sLeft ")
  if (!sampletest$test) stop(sampletest$error)
  sampletest <- is.Sample(sRight,argname="sRight ")
  if (!sampletest$test) stop(sampletest$error)
  if (mode(pan)!="numeric" || abs(pan)>50)
    stop("Parameter 'pan' must be numeric between -50 and 50.")
  sLeft  <- setChannels(sLeft ,1)
  sRight <- setChannels(sRight,1)
  s      <- fitSampleParameters(sLeft,sRight)
  sLeft  <- s[[1]]
  sRight <- s[[2]]
  m <- max(sampleLength(sLeft),sampleLength(sRight))
  sampleLength(sLeft)  <-  m
  sampleLength(sRight) <-  m
  sound(sLeft) <- rbind(sound(sLeft),sound(sRight))
  sLeft <- panorama(sLeft,pan=pan)
  return(sLeft)
}

panorama <- function(s,pan) {
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  if (mode(pan)!="numeric" || abs(pan)>50)
    stop("Parameter 'pan' must be numeric between -50 and 50.")
  s <- loadSample(s,filecheck=FALSE)
  if(channels(s)==1 || pan==50) return(s)
  if(pan==-50) return(mirror(s))
  sl <- sound(s)[1,]
  sound(s)[1,]  <- (50+pan)*sl           + (50-pan)*sound(s)[2,]
  sound(s)[2,]  <- (50+pan)*sound(s)[2,] + (50-pan)*sl
  return(s/100)
}

mirror <- function(s){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  if (channels(s)==2) sound(s) <- sound(s)[2:1,]
  return(s)
}

left <- function(s){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  if (channels(s)==1) return(s)
  sound(s) <- matrix(sound(s)[1,],nrow=1)
  return(s)
}

right <- function(s){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  if (channels(s)==1) return(s)
  sound(s) <- matrix(sound(s)[2,],nrow=1)
  return(s)
}

#=========================================================================
#                          transform sample
#=========================================================================

reverse <- function(s){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  for (i in 1:channels(s)){
    sound(s)[i,] <- sound(s)[i,sampleLength(s):1]
  }
  return(s)
}

pitch <- function(s,semitones){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  if (mode(semitones)!="numeric") stop("Parameter 'semitones' must be numeric.")
  return(s[as.integer(seq(1,sampleLength(s),by=2^(semitones/12)))])
}

cutSampleEnds <- function(s){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  i <- 1
  while (!(sound(s)[1,i]<=0 && sound(s)[1,i+1]>0)) i <- i+1
  j <- sampleLength(s)
  while (!(sound(s)[1,j]>=0 && sound(s)[1,j-1]<0)) j <- j-1
  return(s[(i+1):(j-1)])
}

normalize <- function(s,level=1){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  if (mode(level)!="numeric" || level>1 || level<0) stop("Parameter 'level' must be a number between 0 and 1.")
  return(s/max(abs(sound(s)))*level)
}

center <- function(s){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  for (i in 1:channels(s))
    sound(s)[i,] <- sound(s)[i,]-mean(sound(s)[i,])
  class(s)<-"Sample"
  return(s)
}

noSilence <- function(s,level=0,start=TRUE,end=TRUE){
  sampletest <- is.Sample(s)
  if (!sampletest$test) stop(sampletest$error)
  s <- loadSample(s,filecheck=FALSE)
  if (mode(level)!="numeric" || level<0) stop("Parameter 'level' must be a numeric >=0.")
  i <- 1
  j <- sampleLength(s)
  if (start) {while (max(abs(sound(s)[,i]))<=level && i<j) i <- i+1}
  if (end)   {while (max(abs(sound(s)[,j]))<=level && j>i) j <- j-1}
  if (j==i){
    warning("Empty sample returned.")
    return(nullSample(rate(s),bits(s),channels(s)))
}
  return(s[i:j])
}

#=========================================================================
#                           basic wave forms
#=========================================================================

Sine <- function(freq,dur,rate=44100,bits=16,channels=1){
  if (mode(freq)!="numeric")
    stop("Parameter 'freq' must be numeric.")
  if (mode(dur)!="numeric" || dur<=0)
    stop("Parameter 'dur' must be positive numeric.")
  Sample <- as.Sample(sin(seq(0,freq*dur*2*pi,length=rate*dur)),rate,bits)
  Sample <- setChannels(Sample,channels)
  return(Sample)
}

Sawtooth <- function(freq,dur,rate=44100,bits=16,channels=1,reverse=FALSE){
  if (mode(freq)!="numeric")
    stop("Parameter 'freq' must be numeric.")
  if (mode(dur)!="numeric" || dur<=0)
    stop("Parameter 'dur' must be positive numeric.")
  sound <- (seq(0,2*freq*dur,length=rate*dur)%%2-1)
  if (reverse==TRUE) sound <- sound[length(sound):1]
  Sample <- as.Sample(sound,rate,bits)
  Sample <- setChannels(Sample,channels)
  return(Sample)
}

Square <- function(freq,dur,rate=44100,bits=16,channels=1,upPerc=50){
  if (mode(freq)!="numeric")
    stop("Parameter 'freq' must be numeric.")
  if (mode(dur)!="numeric" || dur<=0)
    stop("Parameter 'dur' must be positive numeric.")
  if (mode(upPerc)!="numeric" || upPerc<0 || upPerc >100)
    stop("Parameter 'upPerc' must be between 0 and 100.")
  sound <- sign((seq(0,freq*dur,length=rate*dur)%%1-(1-upPerc/100)))
  Sample <- as.Sample(sound,rate,bits)
  Sample <- setChannels(Sample,channels)
  return(Sample)  
}

Noise <- function(dur,rate=44100,bits=16,channels=1){
  if (mode(dur)!="numeric" || dur<=0)
    stop("Parameter 'dur' must be positive numeric.")
  if (channels==1) s <- as.Sample(runif(rate*dur,min=-1,max=1),rate,bits)
  else s <- as.Sample(rbind(runif(rate*dur,min=-1,max=1),runif(rate*dur,min=-1,max=1)),rate,bits)
  return(s)
}

Silence <- function(dur,rate=8000,bits=8,channels=1){
  if (mode(dur)!="numeric" || dur<=0)
    stop("Parameter 'dur' must be positive numeric.")
  else return(as.Sample(matrix(0,channels,dur*rate),rate,bits))
}

nullSample <- function(rate=44100,bits=16,channels=1){
  return(as.Sample(matrix(0,channels,1),rate,bits))
}
