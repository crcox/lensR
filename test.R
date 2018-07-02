library('dplyr')
library('ggplot2')
library('reshape2')
# install.packages('ggdendro')
library('ggdendro')
source('./lensR.R')

f <- file('D:/data/Modelling/0.txt', 'r')
getNetworkDimensions(f)
close(f)

datadir <- 'D:/data/Modelling/JitteredOrthography'
#A <- rbind(
#    mutate(loadActivations(file.path(datadir,'jitternet_0.txt')),subject=1),
#    mutate(loadActivations(file.path(datadir,'jitternet_1.txt')),subject=2),
#    mutate(loadActivations(file.path(datadir,'jitternet_2.txt')),subject=3),
#    mutate(loadActivations(file.path(datadir,'jitternet_3.txt')),subject=4),
#    mutate(loadActivations(file.path(datadir,'jitternet_4.txt')),subject=5),
#    mutate(loadActivations(file.path(datadir,'jitternet_5.txt')),subject=6),
#    mutate(loadActivations(file.path(datadir,'jitternet_6.txt')),subject=7),
#    mutate(loadActivations(file.path(datadir,'jitternet_7.txt')),subject=8),
#    mutate(loadActivations(file.path(datadir,'jitternet_8.txt')),subject=9),
#    mutate(loadActivations(file.path(datadir,'jitternet_9.txt')),subject=10)
#)

A <- mutate(loadActivations(file.path(datadir,'jitternet_0.txt.gz')),subject=1)
example_labels <- read.csv(file.path(datadir,'test_example_names.txt'), header=F, stringsAsFactors = F)[[1]]

A$example <- factor(A$example, 1:max(A$example), example_labels)
A$group <- factor(A$group, 1:max(A$group), c('hidden','output'))
A$word <- as.factor(sub('(.*)[0-9]$','\\1',as.character(A$example)))
A.hidden <- filter(A,group=='hidden')
summary(A.hidden)

# Multidimensional scaling
# (The point of the next line is to drop the individual letters... very lame way of doing it)
A.plot <- filter(A.hidden, word %in% levels(word)[c(3:7,9:11,13,14,16,18,20,21,24:26,30,32,33,34,36,37)])
A.plot <- reshape2::dcast(A.plot,example+word~unit,value.var = 'activation')
summary(A.plot)
ggplot(A.plot, aes(x=`1`,y=`2`,color=word)) + geom_point()

# Hierarchical Cluster Analysis and Dendrogram
D <- as.dist(1-lsa::cosine(t(as.matrix(A.plot[,3:6]))))
hc <- hclust(D,members = as.character(A.plot$word))
hc$labels <- as.character(A.plot$word)
dendr <- dendro_data(hc, type="rectangle")

ggplot() +
    geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=2) +
    coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
    theme(axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_rect(fill="white"),
    panel.grid=element_blank())
ggsave('dendrogram_jitted_orth.png',dpi=300,width=4,height=16)




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
