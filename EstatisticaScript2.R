#Gabriel Cavalcanti(gbc7) e Vinicius Vilela(vsv)

#Objetivo 2 - Media - OK
media = function(vetor){
  soma = 0;
  for(x in vetor){
    soma = soma + x
  }
  return(soma/length(vetor))
} 

#Objetivo 3 - Desvio Padrão - OK
desv = function(vetor){
  total = 0;
  for (x in vetor) {
    total = total + (x - media(vetor))^2
  }
  total = sqrt(total/(length(vetor)-1))
  return(total)
} 

#Objetivo 4 - Moda - OK
moda = function(vetor){
  quantidadeElemento = 1
  modda = vetor[1]
  quantidadeElementoParcial = 1
  contador = 2
  while(contador < length(vetor)){
    if(vetor[contador] == vetor[contador-1]){
      quantidadeElementoParcial = quantidadeElementoParcial + 1
    } else {
      if(!(quantidadeElementoParcial < quantidadeElemento)){
        modda = vetor[contador-1]
        quantidadeElemento = quantidadeElementoParcial
        quantidadeElementoParcial = 1
      }
    }
    contador = contador + 1
  }
  return(modda)
} 

#Objetivo 5 - Filmes que possuem nota maior ou igual a 6 - OK
filmesMaioresQueSeis = function(vetorNotas, vetorTitulos){
  
  vetorPos = c()
  vetorTitulosAux = vetorTitulos
  pos = 1
  for (x in vetorNotas) {
    if(x < 6){
      vetorPos = c(vetorPos, pos)
    }
    pos = pos + 1
  }
  
  vetorTitulosAux = vetorTitulosAux[-vetorPos]
  
  return(vetorTitulosAux)
} 

#Objetivo 6 = Quantidade de filmes que tem nota menor que 6 - OK
menoresNotas = function(vetor){ 
  contador = 0
  for(x in vetor){
    if(x < 6){
      contador = contador +1
    } else{
      break
    }
  }
  return(contador)
}

#Objetivo 7 - Funções que retornam caracteristicas dos menores - OK
menoreMaiorNotas = function(vetor){
  menor = vetor[1]
  maior = vetor[length(vetor)]
  resultado = c(menor, maior)
  return(resultado)
} 
menorMaiorNome = function(vetorNot, vetorTitulo){
  
  aux = c()
  
  #Encontrando o menor
  menor = 1000;
  pos = 1;
  posX = 1;
  for (x in vetorNot) {
    if(x < menor){
      menor = x
      posX = pos
    }
    pos = pos + 1
  }
  
  posY = 1
  for (y in vetorTitulo) {
    if(posX == posY){
      menorNome = y;
      aux = c(aux, y)
      break
    }
    posY = posY + 1
  }
  
  #Encontrando o maior
  maior = 0;
  pos = 1;
  posX = 1;
  for (x in vetorNot) {
    if(x > maior){
      maior = x
      posX = pos
    }
    pos = pos + 1
  }
  
  posY = 1;
  for (y in vetorTitulo) {
    if(posX == posY){
      maiorNome = y
      aux = c(aux, y)
      break
    }
    posY = posY + 1
  }
  
  return(aux)
  
} 
menorMaiorAnos = function(vetorNota, vetorAnos){
  
  aux = c()
  
  #Encontrando o menor
  menor = 1000;
  pos = 1;
  posX = 1;
  for (x in vetorNota) {
    if(x < menor){
      menor = x
      posX = pos
    }
    pos = pos + 1
  }
  
  posY = 1
  for (y in vetorAnos) {
    if(posX == posY){
      menorNome = y;
      aux = c(aux, y)
      break
    }
    posY = posY + 1
  }
  
  #Encontrando o maior
  maior = 0;
  pos = 1;
  posX = 1;
  for (x in vetorNota) {
    if(x > maior){
      maior = x
      posX = pos
    }
    pos = pos + 1
  }
  
  posY = 1;
  for (y in vetorAnos) {
    if(posX == posY){
      maiorNome = y
      aux = c(aux, y)
      break
    }
    posY = posY + 1
  }
  
  return(aux)
  
} 

#Objetivo 8 - Função que retorna ano com mais filmes acima de seis e meio - OK
anosAcimaDeSeis = function(vetorN, vetorA){
  
  counter = 1
  while(vetorN[counter] < 6.5){
    counter = counter + 1
  }
  firstYear = vetorA[counter]
  vectorYears = c(firstYear)
  vectorNotes = c(1)
  counter = counter+1
  counter1 = 1
  while(counter <= length(vetorN)){
    if(!(vetorN[counter] < 6.5)){
      while(counter1 <= length(vectorYears)){
        if(vectorYears[counter1] == vetorA[counter]){
          vectorNotes[counter1] = vectorNotes[counter1] + 1
          break
        } else{
          counter1 = counter1 + 1
        }
      }
      if(counter1 == (length(vectorYears)+1)){
        vectorYears = c(vectorYears, vetorA[counter])
        vectorNotes = c(vectorNotes, 1)
      }
      counter1 = 1
    }
    counter = counter + 1
  }
  counter = 1
  bigger = 1
  while(counter <= length(vectorNotes)){
    if(!(vectorNotes[counter] <= bigger)){
      bigger = counter
    }
    counter = counter + 1
  }
  year = vectorYears[bigger]
  return(year)
}

#Objetivo 9 - Histograma
histograma = function(vetorNotas, vetorAnos){
  vetorAnosAux = c()
  vetorCountAux = c()
  
  pos = 1
  for (x in vetorAnos) {
    if(vetorNotas[pos] >= 6){
      vetorAnosAux = c(vetorAnosAux, x)
    }
    pos = pos + 1
  }
  
  k = hist(vetorAnosAux, col = "darkblue", border = "black", xlab = "Anos", ylab = "Frequência",
           main = "Frequência de filmes com notas\niguas ou maiores a seis", )
  return(k)
}

#função Sort Implementada
sortFunction = function(vetorNotas){
  vetorNotasAux = vetorNotas
  
  n = length(vetorNotas)

  for (i in 1:(n-1)) {
    for (j in 1:(n-i)) {
      if(vetorNotasAux[j] > vetorNotasAux[j+1]){
        aux = vetorNotasAux[j]
        vetorNotasAux[j] <- vetorNotasAux[j+1]
        vetorNotasAux[j+1] <- aux
      }
    }
  }
}

#________________________________________________________________________________________

x = read.csv("Sandler.csv")
vetorNotas = (x$NOTAS)
vetorTitulos = (x$TITULOS)
vetorAnos = (x$ANO)
notasOrdenadas = sort(vetorNotas, decreasing = FALSE)

print("Questão 1:")
tabela = (data.frame(Titulo = vetorTitulos, Nota = vetorNotas, Ano = vetorAnos))
print(tabela)

print("Questão 2:")
medi = media(notasOrdenadas)
print(medi)

print("Questão 3:")
desv = desv(notasOrdenadas)
print(desv)

print("Questão 4:")
mode = moda(notasOrdenadas)
print(mode)

print("Questão 5:")
filmesMaioresQueSeiss = filmesMaioresQueSeis(vetorNotas, vetorTitulos)
print(filmesMaioresQueSeiss)

print("Questão 6:")
menoresNotass = menoresNotas(notasOrdenadas)
print(menoresNotass)

print("Questão 7:")
maiorMenorNotass = menoreMaiorNotas(notasOrdenadas)
menorMaiorNomee = menorMaiorNome(vetorNotas, vetorTitulos)
menorMaiorAnoss = menorMaiorAnos(vetorNotas, vetorAnos)
tabela = data.frame(Titulo = menorMaiorNomee, Nota = maiorMenorNotass, Ano = menorMaiorAnoss)
print(tabela)

print("Questão 8:")
anosAcimaDeSeiss = anosAcimaDeSeis(vetorNotas, vetorAnos)
print(anosAcimaDeSeiss)

print("Questão 9:")
histogramaa = histograma(vetorNotas,vetorAnos)
print(histogramaa)
