rm(list=ls())

names <- c("horario", "temp", "vento", "umid", "sensa")
path <- '/Users/u20124/Downloads/cepagri.csv'
cepagri <- read.csv(path, F, ";", col.names = names)

summary(cepagri)


# Código para retirar NA da tabela
tabelaSemNa <- na.omit(cepagri)

# Código para pegar os valores sem NA e sem 0 na parte da umidade(não existe umidade 0)
# tabelaDia <- formatarVetorPorDia(tabelaSemNa[tabelaSemNa[,4]!=0,])


# Código para função formataPorDia que pega os valores médios da medida escolhida
formataPorDia <- function(tabela, qualMedida)
{
  dia  <-  format(as.Date(tabela[1,1], "%d/%m/%Y-%H:%M"), "%d")
  vetorValoresDia <- NULL
  mediaAtual	<-NULL
  vetorMedias <- NULL
  for(linhaAtual in 1:nrow(tabela))
  {
    if(dia != format(as.Date(tabela[linhaAtual,1], "%d/%m/%Y-%H:%M"), "%d") )
    {
      mediaAtual <- mean(vetorValoresDia)
      vetorMedias <- append(vetorMedias, mediaAtual)
      vetorValoresDia <- as.double(tabela[linhaAtual, qualMedida]) 
      dia <- format(as.Date(tabela[linhaAtual,1], "%d/%m/%Y-%H:%M"), "%d")
    }
    else
    {
      valorAtual <- as.double(tabela[linhaAtual, qualMedida])
      vetorValoresDia <- append(vetorValoresDia, valorAtual)
    }
  }
  mediaAtual <- mean(vetorValoresDia)
  vetorMedias <- append(vetorMedias, mediaAtual)
  return(vetorMedias)
}












# Código para pegar o primeiro valor de cada dia para deixar a quantidade de dados no vetor igual para o código acima. Função formatarVetorPorDia
formatarVetorPorDia <- function(tabela){
  
  r <- NULL
  
  vetor <- NULL
  
  if(length(tabela) > 0)
    
  {
    
    r = format(as.Date("01/01/1990-00:00", "%d/%m/%Y-%H:%M"),"%d")
    
    for(i in 1:nrow(tabela))
      
    {
      
      if(format(as.Date(tabela[i,1], "%d/%m/%Y-%H:%M"),"%d") != r){
        
        r = format(as.Date(tabela[i,1], "%d/%m/%Y-%H:%M"), "%d")
        
        vetor <- append(vetor, i)
        
      }
      
    }
    
  }
  
  return(tabela[vetor,])
  
}





# Código para função formataPorMes que pega os valores médios da medida escolhida
formataPorMes <- function(tabela, qualMedida)
{
  mes  <-  format(as.Date(tabela[1,1], "%d/%m/%Y-%H:%M"), "%m")
  vetorValoresMes <- NULL
  mediaAtual	<-NULL
  vetorMedias <- NULL
  for(linhaAtual in 1:nrow(tabela))
  {
    if(mes != format(as.Date(tabela[linhaAtual,1], "%d/%m/%Y-%H:%M"), "%m") )
    {
      mediaAtual <- mean(vetorValoresMes)
      vetorMedias <- append(vetorMedias, mediaAtual)
      vetorValoresMes<- as.double(tabela[linhaAtual, qualMedida]) 
      mes <- format(as.Date(tabela[linhaAtual,1], "%d/%m/%Y-%H:%M"), "%m")
    }
    else
    {
      valorAtual <- as.double(tabela[linhaAtual, qualMedida])
      vetorValoresMes <- append(vetorValoresMes, valorAtual)
    }
  }
  mediaAtual <- mean(vetorValoresMes)
  vetorMedias <- append(vetorMedias, mediaAtual)
  return(vetorMedias)
}

# Código para pegar o primeiro valor de cada mês para deixar a quantidade de dados no vetor igual para o código acima. Função formatarVetorPorMes
formatarVetorPorMes <- function(tabela){
  
  r <- NULL
  
  vetor <- NULL
  
  if(length(tabela) > 0)
    
  {
    
    r = format(as.Date("01/01/1990-00:00", "%d/%m/%Y-%H:%M"),"%m")
    
    for(i in 1:nrow(tabela))
      
    {
      
      if(format(as.Date(tabela[i,1], "%d/%m/%Y-%H:%M"),"%m") != r){
        
        r = format(as.Date(tabela[i,1], "%d/%m/%Y-%H:%M"),"%m")
        
        vetor <- append(vetor, i)
        
      }
      
    }
    
  }
  
  return(tabela[vetor,])
  
}

# Código para função formataPorAno que pega os valores médios da medida escolhida
formataPorAno <- function(tabela, qualMedida)
{
  ano <-  format(as.Date(tabela[1,1], "%d/%m/%Y-%H:%M"), "%Y")
  vetorValoresAno <- NULL
  mediaAtual	<-NULL
  vetorMedias <- NULL
  for(linhaAtual in 1:nrow(tabela))
  {
    if(ano != format(as.Date(tabela[linhaAtual,1], "%d/%m/%Y-%H:%M"), "%Y") )
    {
      mediaAtual <- mean(vetorValoresAno)
      vetorMedias <- append(vetorMedias, mediaAtual)
      vetorValoresAno<- as.double(tabela[linhaAtual, qualMedida]) 
      ano <- format(as.Date(tabela[linhaAtual,1], "%d/%m/%Y-%H:%M"), "%Y")
    }
    else
    {
      valorAtual <- as.double(tabela[linhaAtual, qualMedida])
      vetorValoresAno <- append(vetorValoresAno, valorAtual)
    }
  }
  mediaAtual <- mean(vetorValoresAno)
  vetorMedias <- append(vetorMedias, mediaAtual)
  return(vetorMedias)
}


# Código para pegar o primeiro valor de cada mês para deixar a quantidade de dados no vetor igual para o código acima. Função formatarVetorPorAno
formatarVetorPorAno <- function(tabela){
  
  r <- NULL
  
  vetor <- NULL
  
  if(length(tabela) > 0)
    
  {
    
    r = format(as.Date("01/01/1990-00:00", "%d/%m/%Y-%H:%M"),"%Y")
    
    for(i in 1:nrow(tabela))
      
    {
      
      if(format(as.Date(tabela[i,1], "%d/%m/%Y-%H:%M"),"%Y") != r){
        
        r = format(as.Date(tabela[i,1], "%d/%m/%Y-%H:%M"),"%Y")
        
        vetor <- append(vetor, i)
        
      }
      
    }
    
  }
  
  return(tabela[vetor,])
  
}

# Código para pegar os valores sem NA e sem 0 na parte da umidade(não existe umidade 0). Basicamente um código para conseguir relacionar o tamanho da tabela por ano com a quantidade de médias. Ex: para relacionar duas tabelas, as duas precisam ter a mesma quantidade de valores -> tabelaDia tem 2913 linhas e valoresPorDia tem 2913 valores, logo conseguem se relacionar. Para mudar se vai ser por dia/mês/ano, basta trocar o “Dia” por “Mes” ou “Ano”
tabelaDia <- formatarVetorPorDia(tabelaSemNa[tabelaSemNa[,4]!=0,])
tabelaAno <- formatarVetorPorAno(tabelaSemNa[tabelaSemNa[,4]!=0,])
tabelaMes <- formatarVetorPorMes(tabelaSemNa[tabelaSemNa[,4]!=0,])

# Código para pegar os valores médios, que foram determinados para serem a sensação(5: se quiser mudar a medida, basta trocar o 5 por outro número abaixo dele como 1,2,3,4), por dia da tabela sem os zeros da coluna das umidades. Caso queira pegar por mês ou ano, basta trocar no valoresPorDia e no formataPorDia, o Dia por Mes ou por Ano.
# temperatura
temperaturasPorDia <- formataPorDia(tabelaSemNa[tabelaSemNa[,4]!=0,], 2)
temperaturasPorMes <- formataPorMes(tabelaSemNa[tabelaSemNa[,4]!=0,], 2)
temperaturasPorAno <- formataPorAno(tabelaSemNa[tabelaSemNa[,4]!=0,], 2)

# vento 
ventoPorDia <- formataPorDia(tabelaSemNa[tabelaSemNa[,4]!=0,], 3)
ventoPorMes <- formataPorMes(tabelaSemNa[tabelaSemNa[,4]!=0,], 3)
ventoPorAno <- formataPorAno(tabelaSemNa[tabelaSemNa[,4]!=0,], 3)

# umidade 
umidadePorDia <- formataPorDia(tabelaSemNa[tabelaSemNa[,4]!=0,], 4)
umidadePorMes <- formataPorMes(tabelaSemNa[tabelaSemNa[,4]!=0,], 4)
umidadePorAno <- formataPorAno(tabelaSemNa[tabelaSemNa[,4]!=0,], 4)

# sensação 
sensaçãoPorDia <- formataPorDia(tabelaSemNa[tabelaSemNa[,4]!=0,], 5)
sensaçãoPorMes <- formataPorMes(tabelaSemNa[tabelaSemNa[,4]!=0,], 5)
sensaçãoPorAno <- formataPorAno(tabelaSemNa[tabelaSemNa[,4]!=0,], 5)

# Código para a criação dos gráficos pela média do dia/mês/ano (depende de qual função for chamada:
                                                                # Para dia: tabelaDia — valoresPorDia
                                                                # Para mes: tabelaMes — valoresPorMes
                                                                # Para ano: tabelaAno — valoresPorAno
                                                                
                                                                # No formatarVetorPorDia, pega a tabelaSemNa e sua primeira coluna(a data). O formataPorDia pega a tabelaSemNa, tira a média da medida colocada(3 = ventos) e armazena em um vetor próprio.
                

ny <- airquality; ny$Month <- factor(month.abb[ny$Month], levels = month.abb, ordered = TRUE)
ggplot(ny,aes(x=month.abb,y=Temp,group=Month,fill=Month))+geom_boxplot()+scale_fill_brewer(palette="Pastel1")

ggplot(tabelaMes, aes(x=month.abb, y=temperaturasPorMes)) + geom_boxplot() + scale_fill_brewer(palette="Pastel1")

ggplot(tabelaMes,aes(x=month.abb, y=umidadePorMes, group=month.abb,fill=month.abb)) + geom_boxplot() + scale_fill_brewer(palette="Pastel1")

ggplot(data=tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"),
   y = temperaturasPorAno, group=1)) + geom_line(linetype = "dashed") + geom_point() + labs(x = "Ano", y = "Temperatura")

ggplot(data=tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"),
   y = temperaturasPorAno/umidadePorAno, group=1)) + geom_line(linetype = "dashed") + geom_point() + labs(x = "Ano", y = "Razão entre temperatura e umidade")

ggplot(data=tabelaDia, aes(x = as.Date(tabelaDia[,1], "%d/%m/%Y-%H:%M"),
   y = valoresPorDia, group=1)) + geom_line(linetype = "dashed") + geom_point() + labs(x = "Data", y = "Temperatura")


ggplot(data=tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"),
 y = temperaturasPorAno, group=1)) + geom_line(linetype = "dashed") + geom_point() + labs(x = "Data", y = "Temperatura") + geom_line(aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"), y = umidadePorAno))


ggplot(data=tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"),
 y = temperaturasPorAno, group=1)) + geom_line(linetype = "dashed", colour="red") + geom_point() + labs(x = "Data", y = "Temperatura e umidade ") + geom_line(aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"), y = sensaçãoPorAno))


ggplot(tabelaDia, aes(x = temperaturasPorDia , y = ventoPorDia)) + geom_point() + labs(x="Temperatura", y="Vento")

tabelaSemNa[tabelaSemNa$]

ggplot(tabelaAno, aes(x = temperaturasPorAno)) + geom_histogram(color = "White", bins=20)



ggplot(tabelaDia, aes(x = temperaturasPorDia, y = umidadePorDia)) + geom_line(linetype = "solid") + geom_point() + labs(x = "Temperatura", y = "Umidade")

ggplot(tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"), y = valoresPorAno)) + geom_density()

ggplot(tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M") , y = valoresPorAno)) + geom_smooth() + labs(x = "Data", y = "Temperatura")

ggplot(tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M") , y = umidadePorAno)) + geom_boxplot()

ggplot(tabelaMes, aes(x = as.Date(tabelaMes[,1], "%d/%m/%Y-%H:%M"), y = umidadePorMes)) + geom_boxplot()


ggplot(tabelaMes, aes(x=temperaturasPorMes, y=sensaçãoPorMes)) + geom_line() + labs(x='Temperatura', y='Sensação');

















temps <- ggplot(tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"), y = temperaturasPorAno)) + geom_point() + geom_line(colour="red") + geom_point() + labs(x = "Ano", y = "Temperatura") 
temps + geom_line(tabelaAno, aes(x = as.Date(tabelaAno[,1], "%d/%m/%Y-%H:%M"), y = mean(temperaturasPorAno)), colour="blue") 