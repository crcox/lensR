library('dplyr')
library('ggplot2')
D <- loadActivations('D:/data/test_subset.out.gz')
D$contributingSpokes <- c(rep(1,12), rep(2,12), rep(3,4))
#D$input <- factor(c(rep(1,100), rep(2,100), rep(3,100), rep(4,100)), levels = 1:4, labels = c('visual','audio','motor','all'))
D$input <- factor(c(rep(1,25), rep(2,25), rep(3,25), rep(4,25)), levels = 1:4, labels = c('visual','audio','motor','all'))

avg <- D %>%
    group_by(input, group, contributingSpokes) %>%
    summarize(err = sqrt(sum((target-activation)^2))/n())

avg$group <- factor(avg$group, levels = 1:3, labels = c('visual', 'audio', 'motor'))
avg$contributingSpokes <- factor(avg$contributingSpokes, levels = 1:3, labels = c('unimodal', 'bimodal', 'multimodal'))
ggplot(avg, aes(x = input, y = err, color = contributingSpokes)) + geom_point() + facet_wrap('group')
