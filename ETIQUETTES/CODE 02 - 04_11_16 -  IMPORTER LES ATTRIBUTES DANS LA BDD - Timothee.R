enq.final <- read.csv("enq final.csv")
etiquettes <- read.csv("etiquettes.csv")
etiquettes <- as.character(etiquettes$x)
attributes(enq.final)$variable.labels <- etiquettes
attributes(enq.final)$variable.labels[120]


