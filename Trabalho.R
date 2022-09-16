rm(list=ls())

names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(con, FALSE, ";", col.names = names)

summary(cepagri)