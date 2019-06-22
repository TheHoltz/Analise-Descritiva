descrever <- function(dados){
  
  if (!'moments' %in% installed.packages()) install.packages('moments')
  
  out <- list();
  
  gmean <- function(x){
    return(prod(x)**(1/length(x)))
  }
  
  hmean <- function(x){
    return((sum(x**(-1))/length(x))**(-1))
  }
  
  out$info <- paste('O conjunto de dados possui',length(dados),
                    'observações, sendo a de menor valor observado',min(dados),
                    'e por outro lado a de maior',max(dados))
  
  out$media <- paste('Em função das tendencias centrais, o conjunto de dados apresenta como média aritmetica',
                     round(mean(dados),3),'; média geométrica', round(gmean(dados),3),
                     '; média harmônica', round(hmean(dados),3), "e mediana",median(dados));
  
  out$desvio <- paste('O conjunto apresenta um desvio padrão de ', round(sd(dados),2))
  
  if(moments::skewness(dados) >= 1){
    out$assimetria <- paste('Vale ressaltar também que o conjunto possui uma assimetria (skewness) de ',round(moments::skewness(dados),3),
                            'indicando que boa parte dos dados estão a direita da média.')
  } else {
    out$assimetria <- paste('Vale ressaltar também que o conjunto possui uma assimetria (skewness) de ',round(moments::skewness(dados),3),
                            'indicando que boa parte dos dados estão a esquerda da média.')
  }
  
  out$Curtose <- paste('Por último mas não menos importante, possui uma curtose (medida de achatamento da distribuição) de',
                       round(moments::kurtosis(dados),3))
  
  return(out)
  
}

descreverFator <- function(x){
  out <- list()
  grupos <- data.frame(prop.table(table(x)))
  grupos <- grupos[order(grupos[,2]),]

  out[[1]] <- paste('O grupo com menor frequência',grupos[1,1],' possui',round(grupos[1,2]*100),'% das observações, sendo ele o de menor participação.')

  for (i in 2:dim(grupos)[1]-1) {
    out[[i+1]] <- paste('Os de',grupos[i+1,1],' possuem',round(grupos[i+1,2]*100),'% das observações.')
  }
  
  out[[dim(grupos)[1]]] <- paste('O grupo com maior frequência ',grupos[dim(grupos)[1],1],'possui',round(grupos[dim(oi)[1],2]*100),
                    '% das observações, sendo ele o de maior participação.')
  
  return(out)
}
