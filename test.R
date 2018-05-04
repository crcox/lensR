source('./lensR.R')

f <- file('D:/data/Modelling/0.txt', 'r')
getNetworkDimensions(f)
close(f)

datadir <- 'C:/Users/mbmhscc4/GitHub/SOSLassoSimulations/data/WithWeightDecay'
A <- rbind(
    mutate(loadActivations(file.path(datadir,'0.txt.gz')),subject=1),
    mutate(loadActivations(file.path(datadir,'1.txt.gz')),subject=2),
    mutate(loadActivations(file.path(datadir,'2.txt.gz')),subject=3),
    mutate(loadActivations(file.path(datadir,'3.txt.gz')),subject=4),
    mutate(loadActivations(file.path(datadir,'4.txt.gz')),subject=5),
    mutate(loadActivations(file.path(datadir,'5.txt.gz')),subject=6),
    mutate(loadActivations(file.path(datadir,'6.txt.gz')),subject=7),
    mutate(loadActivations(file.path(datadir,'7.txt.gz')),subject=8),
    mutate(loadActivations(file.path(datadir,'8.txt.gz')),subject=9),
    mutate(loadActivations(file.path(datadir,'9.txt.gz')),subject=10)
)
A$group <- A$group + 2
B <- filter(A,istarget)
B$activation <- B$target
B$target <- NaN
B$istarget <- FALSE
B$group <- B$group - 4
A <- rbind(A,B)
A$group <- factor(A$group, levels = 1:6, labels = c('CI','II','CH','IH','CO','IO'))

tmp <- dplyr::arrange(reshape2::dcast(A,example+subject~group+unit, value.var = 'activation'),subject,example)
write.csv(x = tmp, file = 'C:/Users/mbmhscc4/MATLAB/MRI/FacePlaceObject/LCN_2014/9c2_NEWDATA.csv', row.names = FALSE)
library('dplyr')
library('ggplot2')
library('reshape2')
filter(A,example==1)

ggplot(A, aes(x=unit,y=activation,group=example,color=example)) + geom_line() + facet_wrap('group')

M <- as.matrix(reshape2::dcast(filter(A,group==1),example~unit,value.var = 'activation')[,2:8])
D <- dist(M)
image(as.matrix(D))

M <- as.matrix(reshape2::dcast(filter(A,group==2),example~unit,value.var = 'activation')[,2:8])
D <- dist(M)
image(as.matrix(D))

M <- as.matrix(reshape2::dcast(filter(A,group==3),example~unit,value.var = 'activation')[,2:19])
D <- dist(M)
image(as.matrix(D))

M <- as.matrix(reshape2::dcast(filter(A,group==4),example~unit,value.var = 'activation')[,2:19])
D <- dist(M)
image(as.matrix(D))
