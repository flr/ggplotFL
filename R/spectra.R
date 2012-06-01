#Spectral analysis function

spectra=function(x,scale="missing"){
  
      if (!missing(scale)) x =apply(x,c(1,3:6),scale)
     
      fn=function(x){
        nfft <- (2^ceiling(log2(length(x))))
        x[(length(x)+1): nfft] <- 0
        fftx <- fft(x)
    
        # It's symmetrical so throw away second half. Only first 1 + nfft points are unique
        NoUnPo <- ceiling((nfft+1)/2) # Number of unique points
        fftx <- fftx[1:NoUnPo]
        # First element is DC component, last is the Nyquist component
        
        # Take magnitude of fft of x and scale the fft so that it is not a function of length
        mx <- abs(fftx) / length(x)
        # Take square of magnitude
        mx <- mx^2
    
        # As we dropped the first half of fft so multiply by 2 to keep same energy
        # DC component (first element) and Nyquist component (last element if even
        # number points (should be)) should not be multiplied by 2
        mx[2:(length(mx)-1)] <- mx[2:(length(mx)-1)] * 2
    
        return(mx)}

    res=aperm(apply(x,c(1,3:6),fn),c(2,1,3:6))
    names(dimnames(res))[2]="year"
    dimnames(res)[["year"]]=seq(dim(res)[2])
    res=FLQuant(res)

    return(res)}


#Spectral analysis function

spectra. <- function(x,fs=1,norm = FALSE, pl = TRUE,omit=-(1:5))
{
#    browser()
    # Pad x with zeroes to make it's length a power of 2, i.e. length should be 2^something
    # This makes the fft faster
    oldx <- x # keep for later
    if(norm == TRUE) x <- x - mean(x)
    nfft <- (2^ceiling(log2(length(x))))
    x[(length(x)+1): nfft] <- 0
    fftx <- fft(x)
    # It's symmetrical so throw away second half. Only first 1 + nfft points are unique
    NoUnPo <- ceiling((nfft+1)/2) # Number of unique points
    fftx <- fftx[1:NoUnPo]
    # First element is DC component, last is the Nyquist component
    
    # Take magnitude of fft of x and scale the fft so that it is not a function of length
    mx <- abs(fftx) / length(x)
    # Take square of magnitude
    mx <- mx^2

    # As we dropped the first half of fft so multiply by 2 to keep same energy
    # DC component (first element) and Nyquist component (last element if even
    # number points (should be)) should not be multiplied by 2
    mx[2:(length(mx)-1)] <- mx[2:(length(mx)-1)] * 2

    # Plotting stuff
    f <- seq(from =0, to = NoUnPo-1) * fs/nfft # frequency axis for plot
    if (pl == TRUE)
    {
        par(mfrow=c(2,1))
        t <- seq(from = 0, to = (length(oldx)-1) / fs, by = 1/fs)
        plot(x=t,y=oldx,type="l",main="Time series", xlab="Time (s)", ylab = "x")
        plot(x=f[omit],y=mx[omit],type="l", main="Power spectrum", xlab="Frequency (Hz)", ylab = "Power")
    }
    invisible(list(mx = mx, f = f))}

