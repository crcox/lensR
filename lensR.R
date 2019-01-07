LENS_ENDIAN <- 'big'

getNetworkDimensions.text <- function(f) {
    stepsize <- function(nunits_per_group, nticks) {
        ngroups <- length(nunits_per_group)
        exampleSize <- sum(nunits_per_group) * nticks
        return( exampleSize + (ngroups * nticks) + nticks )
    }
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
    N <- stepsize(nunits, nticks - 1)

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

            N <- stepsize(nunits, nticks)
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
getNetworkDimensions.binary<- function(f) {
    example_info <- readBin(f, integer(), n=4, size=4, endian=LENS_ENDIAN)
    names(example_info) <- c('total-updates','example-number','ticks-on-example','num-groups')

    num_units <- numeric(length = example_info['num-groups'])
    has_targets <- numeric(length = example_info['num-groups'])
    nticks_per_example <- numeric(100000) # pre-allocating for an arbitrary number of examples
    nticks_per_example[example_info[['example-number']]+1] <- example_info['ticks-on-example']
    names(nticks_per_example)[example_info['example-number']+1] <- example_info[['example-number']]

    tick_info <- readBin(f, integer(), n=2, size=4, endian=LENS_ENDIAN)
    names(tick_info) <- c('tick-number','event-number')

    for (i in 1:example_info['num-groups']) {
        num_units[i] <- readBin(f, integer(), n = 1, size=4, endian=LENS_ENDIAN)
        has_targets[i] <- readBin(f, logical(), n = 1, size = 1, endian=LENS_ENDIAN)

        # Skip ahead
        if (has_targets[i])
            readBin(f, numeric(), n=num_units[i]*2, size = 4, endian=LENS_ENDIAN)
        else
            readBin(f, numeric(), n=num_units[i], size = 4, endian=LENS_ENDIAN)

    }
    nunits <- num_units
    ngroups <- example_info[['num-groups']]

    nexamples_per_update <- numeric(100000) # pre-allocating for an arbitrary number of updates
    nexamples_per_update[1] <- 1 # already passed the first example
    nupdates <- 1 # in the middle of the first update
    example_info_previous <- example_info
    while ( TRUE ) {
        example_info <- readBin(f, integer(), n = 4, size=4, endian=LENS_ENDIAN)
        if (length(example_info) < 4) break
        names(example_info) <- c('total-updates','example-number','ticks-on-example','num-groups')
        nticks_per_example[example_info[['example-number']]+1] <- example_info['ticks-on-example']
        names(nticks_per_example)[example_info['example-number']+1] <- example_info[['example-number']]
        for (it in 1:example_info['ticks-on-example']) {
            tick_info <- readBin(f, integer(), n = 2, size=4, endian=LENS_ENDIAN)
            if (length(tick_info) < 2) break
            names(tick_info) <- c('tick-number','event-number')
            for (ig in 1:example_info['num-groups']) {
                num_units <- readBin(f, integer(), n = 1, size=4, endian=LENS_ENDIAN)
                if (length(num_units) < 1) break
                has_targets <- readBin(f, logical(), n = 1, size = 1, endian=LENS_ENDIAN)
                if (length(has_targets) < 1) break

                # If the group has targets, double the number of units associated with it.
                # Then attempt to consume that many 4 byte chunks from the file.
                if (has_targets)
                    num_units <- num_units * 2
                n <- length(readBin(f, numeric(), n = num_units, size=4, endian=LENS_ENDIAN))
                if (n < num_units) break
            }

            # If we make it to this point without breaking the while loop, as a
            # result of reading fewer than the expected number of values from
            # the file at any prior step, then increment the number of
            # (complete) updates and examples in the file.
            if (example_info['total-updates'] > example_info_previous['total-updates']) {
                nupdates <- nupdates + 1
                nexamples_per_update[nupdates] <- 0
            }

            if (example_info['example-number'] > example_info_previous['example-number']) {
                nexamples_per_update[nupdates] <- nexamples_per_update[nupdates] + 1
            }

            example_info_previous <- example_info
        }
    }
    dd <- list(
        nupdates = nupdates,
        nexamples = nexamples_per_update[nexamples_per_update > 0],
        nticks = nticks_per_example[nticks_per_example > 0],
        ngroups = ngroups,
        nunits = nunits
    )
    return(dd)
}
getNetworkDimensions <- function(path, format = c('text','binary')) {
    if (format == 'text') {
        f <- file(path, 'r')
        on.exit(close(f))
        return(getNetworkDimensions.text(f))
    } else {
        f <- file(path, 'rb')
        on.exit(close(f))
        return(getNetworkDimensions.binary(f))
    }
}

initNetworkDataframe <- function(networkDims) {
    require('data.table')
    N <- networkDims$nupdates * sum(networkDims$nticks) * sum(networkDims$nunits)
    d <- data.table::data.table(update = integer(N), example = integer(N), tick = integer(N), group = integer(N), unit = integer(N), activation = numeric(N), target = numeric(N), istarget = logical(N))
}

loadActivations.text <- function(f, d, D) {
    require('data.table')
    cur <- 0
    for (iu in 1:d$nupdates) {
        for (ie in 1:d$nexamples[iu]) {
            example_info <- as.numeric(do.call(c, strsplit(readLines(f, n = 2), split = ' ')))
            names(example_info) <- c('total-updates','example-number','ticks-on-example','num-groups')
            for (it in 1:d$nticks[ie]) {
                tick_info <- as.numeric(do.call(c, strsplit(readLines(f, n = 1), split = ' ')))
                names(tick_info) <- c('tick-number','event-number')
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
                    set(D, a:b, 'update', example_info[['total-updates']])
                    set(D, a:b, 'example', example_info[['example-number']])
                    set(D, a:b, 'tick', tick_info[['tick-number']])
                    set(D, a:b, 'group', ig)
                    set(D, a:b, 'unit', 1:nu)
                    if (istarget){
                        set(D, a:b, 'activation', X[1,])
                        set(D, a:b, 'target', X[2,])
                    } else {
                        set(D, a:b, 'activation', X)
                        set(D, a:b, 'target', NaN)
                    }
                    set(D, a:b, 'istarget', istarget)
                    cur <- b
                }
            }
        }
    }
    return(D)
}
loadActivations.binary <- function(f, d, D) {
    require('data.table')
    cur <- 0
    for (iu in 1:d$nupdates) {
        for (ie in 1:d$nexamples[iu]) {
            example_info <- readBin(f, integer(), n = 4, size=4, endian=LENS_ENDIAN)
            names(example_info) <- c('total-updates','example-number','ticks-on-example','num-groups')
            for (it in 1:example_info['ticks-on-example']) {
                tick_info <- readBin(f, integer(), n = 2, size=4, endian=LENS_ENDIAN)
                names(tick_info) <- c('tick-number','event-number')
                for (ig in 1:example_info['num-groups']) {
                    num_units <- readBin(f, integer(), n = 1, size=4, endian=LENS_ENDIAN)
                    has_targets <- readBin(f, logical(), n = 1, size=1, endian=LENS_ENDIAN)
                    a <- cur + 1
                    b <- cur + num_units
                    set(D, a:b, 'update', example_info[['total-updates']])
                    set(D, a:b, 'example', example_info[['example-number']])
                    set(D, a:b, 'tick', tick_info[['tick-number']])
                    set(D, a:b, 'group', ig)
                    set(D, a:b, 'unit', 1:num_units)
                    if (has_targets) {
                        M <- matrix( readBin(f, numeric(), n = num_units * 2, size=4, endian=LENS_ENDIAN), nrow = 2 )
                        set(D, a:b, 'activation', M[1,])
                        set(D, a:b, 'target', M[2,])
                    } else {
                        set(D, a:b, 'activation', readBin(f, numeric(), n = num_units, size=4, endian=LENS_ENDIAN))
                        set(D, a:b, 'target', NaN)
                    }
                    set(D, a:b, 'istarget', has_targets)
                    cur <- b
                }
            }
        }
    }
    return(D)
}
loadActivations <- function(path, d, format=c('text','binary')) {
    if ( missing(d) )
        d <- getNetworkDimensions(path, format=format)

    D <- initNetworkDataframe(d)

    if (format == 'text') {
        f <- file(path, 'r')
        on.exit(close(f))
        return(loadActivations.text(f, d, D))
    } else {
        f <- file(path, 'rb')
        on.exit(close(f))
        return(loadActivations.binary(f, d, D))
    }
}

loadWeights <- function(path, groups, bias = TRUE, format = 'binary') {
    f <- file(path, 'rb')
    on.exit(close(f))
    magic_cookie <- 1431655766
    x <- readBin(f, integer(), n = 1, size = 4, endian = LENS_ENDIAN)
    if ( magic_cookie != x ) stop("%s is not a binary LENS weight file.", path)
    net_info <- readBin(f, integer(), n = 3, size = 4, endian = LENS_ENDIAN)
    names(net_info) <- c('total-number-of-links', 'num-values', 'total-updates')

    W <- list()
    group_names <- names(groups)
    group_numbers <- 1:length(groups)
    if (is.null(group_names))
        group_names <- group_numbers
    z <- is.na(group_names)
    group_names[z] <- group_numbers[z]

    for (ig in 2:length(groups)) {
        label <- paste(group_names[ig-1],'->',group_names[ig])
        n_receiving_units <- groups[ig]
        n_sending_units <- groups[ig - 1] + bias
        w <- matrix(0, nrow = n_sending_units, ncol = n_receiving_units)
        for (receiving_unit in 1:n_receiving_units) {
            w[, receiving_unit] <- readBin(f, numeric(), n = n_sending_units, size = 4, endian = LENS_ENDIAN)
        }
        W[[label]] <- w
    }
    return(W)
}