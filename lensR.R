getNetworkDimensions <- function(f) {
    x <- readLines(f, n=3)
    xi <- as.integer(strsplit(x[1],split =' ')[[1]])
    iupdate  <- xi[1]
    iexample <- xi[2]
    xi <- as.integer(strsplit(x[2],split =' ')[[1]])
    nticks <- xi[1]
    nticks_list <- nticks
    ngroups <- xi[2]
    nunits <- numeric(ngroups)
    istargetgroup <- logical(ngroups)
#        xi <- as.integer(strsplit(x[3],split =' ')[[1]])
#        itick_ <- xi[1]
#        ievent <- xi[2]
    for (i in 1:ngroups) {
        x <- readLines(f, n=1)
        xi <- as.integer(strsplit(x,split =' ')[[1]])
        nunits[i] <- xi[1]
        istargetgroup[i] <- as.logical(xi[2])
        skip <- readLines(f, n=nunits[i])
    }
    # An example will have an activation value for every unit and every
    # tick. At the start of each group, there is an extra line marking the
    # start of that group and its size. At the start of each tick there is
    # an index and event code to differentiate one set of activation values
    # from the next. So to get to the next example, we need to advance one
    # line for each tick at each unit, plus a line for each group-header
    # within each tick, plus a line for each tick header.
    #
    # To complicate things at this step, we have already read the whole
    # first tick from the first example having scaned all groups to get the
    # number of units in each. So we should be sitting on the header for the
    # second tick. So this time, we need to subtract 1 tick from everything.
    nticks <- nticks - 1
    # If there is only 1 tick on the first example, this will have set the
    # nticks to zero.
    nunitsTotal <- sum(nunits) # sum total of units over all groups (written to file).
    exampleSize <- nunitsTotal * nticks
    N <- (exampleSize + (ngroups * nticks) + nticks)
    # The command executed within the while statement should both:
    #   1. Check that there are more lines to consume
    #   2. Consume them, so that when we read lines within the block we have
    #      already reached the header for the next example.
    nexamples <- 1
    nupdates <- 1
    iupdate_o <- iupdate
    iupdate_p <- iupdate
    iexample_p <- iexample
    while (!is.na(N) && length(x <- readLines(f, n = N)) == N) {
        if (length(x <- readLines(f, n = 2)) > 0) {
            xi <- as.integer(strsplit(x[1],split = ' ')[[1]])
            iupdate  <- xi[1]
            iexample <- xi[2]
            xi <- as.integer(strsplit(x[2],split = ' ')[[1]])
            nticks <- xi[1]
            nticks_list <- c(nticks_list, nticks)
            ngroups <- xi[2]
            if (iupdate > iupdate_p) {
                nupdates <- nupdates + 1
                iupdate_p <- iupdate
            }
            else if (iupdate == iupdate_o && iexample > iexample_p) {
                nexamples <- nexamples + 1
                iexample_p <- iexample
            }
            # Technically, the step size to jump to the next example may need to
            # be updated if for some reason the number of ticks varied by
            # example.
            exampleSize <- nunitsTotal * nticks
            N <- (exampleSize + (ngroups * nticks) + nticks)
        }
    }
    dd <- list(
        nupdates = nupdates,
        nexamples = nexamples,
        nticks = nticks_list,
        ngroups = ngroups,
        nunits = nunits
    )
    return(dd)
}
initNetworkDataframe <- function(networkDims) {
    N <- networkDims$nupdates * sum(networkDims$nticks) * sum(networkDims$nunits)
    d <- data.frame(update = integer(N), example = integer(N), tick = integer(N), group = integer(N), unit = integer(N), activation = numeric(N), target = numeric(N), istarget = logical(N))
}
loadActivations <- function(path) {
    f <- file(path, 'r')
    on.exit(close(f))
    d <- getNetworkDimensions(f)
    close(f)
    D <- initNetworkDataframe(d)
    # The closing and reopening is done to rewind the file... documentation for
    # seek says not to use it on Windows, so this is all I can think of.
    f <- file(path, 'r')
    on.exit(close(f))
    cur <- 0
    for (iu in 1:d$nupdates) {
        for (ie in 1:d$nexamples) {
            x <- readLines(f, n = 2)
            for (it in 1:d$nticks[ie]) {
                x <- readLines(f, n = 1)
                for (ig in 1:d$ngroups) {
                    x <- readLines(f, n = 1)
                    xi <- as.numeric(strsplit(x,split = ' ')[[1]])
                    istarget <- as.logical(xi[2])
                    nu <- d$nunits[ig]
                    x <- readLines(f, n = nu)
                    xi <- strsplit(x,split = ' ')
                    X <- sapply(xi, as.numeric)
                    a <- cur + 1
                    b <- cur + nu
                    D$update[a:b] <- iu
                    D$example[a:b] <- ie
                    D$tick[a:b] <- it
                    D$group[a:b] <- ig
                    D$unit[a:b] <- 1:nu
                    if (istarget){
                        D$activation[a:b] <- X[1,]
                        D$target[a:b] <- X[2,]
                    } else {
                        D$activation[a:b] <- X
                        D$target[a:b] <- NaN
                    }
                    D$istarget[a:b] <- istarget
                    cur <- b
                }
            }
        }
    }
    return(D)
}
