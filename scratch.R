
    x <- readLines(f, n=4)
    xi <- as.integer(strsplit(x[1],split =' ')[[1]])
    iupdate  < - xi[1]
    iexample < - xi[2]
    xi <- as.integer(strsplit(x[2],split =' ')[[1]])
    nticks <- xi[1]
    ngroups <- xi[2]
    nunits <- rep(0, ngroups)
    istargetgroup <- rep(F, ngroups)
#        xi <- as.integer(strsplit(x[3],split =' ')[[1]])
#        itick_ <- xi[1]
#        ievent <- xi[2]
    xi <- as.integer(strsplit(x[4],split =' ')[[1]])


readOneUpdate <- function(f) {
    data.frame()
    x <- readLines(f, n=2)
    xi <- as.integer(strsplit(x[1],split =' ')[[1]])
    nupdates < - xi[1]
    iexample < - xi[2]
    xi <- as.integer(strsplit(x[2],split =' ')[[1]])
    nticks <- xi[1]
    ngroups <- xi[2]
    for (itick in 1:nticks) {
        x <- readLines(f, n=1)
        xi <- as.integer(strsplit(x,split =' ')[[1]])
        itick_ <- xi[1]
        ievent <- xi[2]
        for (igroup in 1:ngroups) {
            x <- readLines(f, n=1)
            xi <- as.integer(strsplit(x,split =' ')[[1]])
            nunits <- xi[1]
            istargetgroup <- xi[2]
            x <- readLines(f, n=nunits)
            for (iunit in 1:nunits) {
                xi <- as.numeric(strsplit(x[iunit],split =' ')[[1]])
                outputvalue <- xi[1]
                targetvalue <- xi[2]

            }
        }

    }
}

for each example:
    <I total-updates> <I example-number>
    <I ticks-on-example> <I num-groups>
    for each tick on the example:
    <I tick-number> <I event-number>
    for each WRITE_OUTPUTS group:
    <I num-units> <B targets?>
    for each unit:
    <R output-value> <R target-value>
