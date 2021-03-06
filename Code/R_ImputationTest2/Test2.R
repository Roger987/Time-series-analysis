require(statip)
require(ggplot2)
source("Functions.R")
source("timeOrdered.R")
source("Distances.R")
source("Entropies.R")

#Função geradora do mapa logístico
series_generator_map <- function(r, x_0, n, n_series, init){
  Series <- matrix(nrow = n_series, ncol = n)
  a <- x_0
  for(i in c(2:init)){
    aa <- r*a*(1 - a)
    a <- aa
  }
  x_0 <- r*a*(1 - a)
  
  for(i in c(1:n_series)){
    Series[i,1] <- x_0
    for(j in c(2:n)){
      Series[i,j] <- r*Series[i,j-1]*(1 - Series[i,j-1])
    }
    x_0 <- r*Series[i,n]*(1 - Series[i,n])
  }
  return(Series)
}

atack <- function(n_series,series,p,n){
  Series.to.analysis <- matrix(nrow = n_series, ncol = n)
  for(j in c(1:n_series)){
    x <- series
    indicator <- rbern(n,p)
    for(i in c(2:n)){
      if(indicator[i]==1){ 
        x[i] = x[i-1]
      }
    }
    Series.to.analysis[j,] <- x
  }
  Series.to.analysis
}

cotas <- function(dimension){
  
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  p = qplot(xlab=expression(H), ylab=expression(C)) +
    theme(plot.title = element_text(hjust=0.5)) +
    geom_line(aes(x=c2x, y=c2y), size=2, alpha=.5) +
    geom_line(aes(x=c1x, c1y), size=2, alpha=.5) + 
    scale_x_continuous(limits=c(0, 1)) +
    scale_y_continuous(limits=c(0, 0.5))
  
  return(p)
}

HCPlane <- function(p, Entropy.Complexity, dimension){
  
  #entropycolumns = c(1,3,5,7,9)
  #complexitycolumns = c(2,4,6,8,10)
  
  #for(i in c(1:dim(Entropy.Complexity)[1])){
  p = p + 
    #geom_segment(aes(x=Entropy.Complexity[i,entropycolumns[1:4]],
    #                 xend=Entropy.Complexity[i,entropycolumns[2:5]],
    #                 y=Entropy.Complexity[i,complexitycolumns[1:4]],
    #                 yend=Entropy.Complexity[i,complexitycolumns[2:5]]), linetype="dotted") +
    geom_point(data = Entropy.Complexity, aes(x = Entropy, y = Complexity,colour = Imputation), 
               shape = 19, 
               size = 3)
  #}
  
  return(p)
}

Entropy.complexity.values <- function(Series.to.analysis, dimension, delay){
  
  prob = prob1 = prob2 = prob3 = prob4 = matrix(nrow = dim(Series.to.analysis)[1], ncol = factorial(dimension))
  Entropy = Complexity = Complexity1 = Entropy1 = Complexity2 = Entropy2 = Complexity3 = Entropy3 = Complexity4 = Entropy4 = rep(0,(dim(Series.to.analysis)[1]))
  symbols <- definePatterns(dimension)
  
  for(i in c(1:dim(Series.to.analysis)[1])){
    
    patterns <- formationPattern(Series.to.analysis[1,],dimension,delay,0)
    elements <- formationPattern(Series.to.analysis[1,],dimension,delay,1)
    
    #prob[i,] <- timeOrdered(Series.to.analysis[i,], dimension, delay, patterns, symbols)
    #Entropy[i] <- shannonNormalized(prob[i,])
    #Complexity[i] <- Ccomplexity(prob[i,])
    
    start.time <- Sys.time()
    prob1[1,] <- timeOrdered(Series.to.analysis[1,], dimension, delay, patterns, symbols) #Green
    Entropy1[1] <- shannonNormalized(prob1[1,])
    Complexity1[1] <- Ccomplexity(prob1[1,])
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    
    dyn.load("TimeOrderedImputation.so")
    
    start.time <- Sys.time()
    prob2[1,] <- .Call("TimeOrderedImputation", Series.to.analysis[1,], dimension, delay, as.integer(as.vector(t(patterns))), as.vector(t(elements)), as.integer(as.vector(t(symbols))))
    Entropy2[1] <- shannonNormalized(prob2[1,])
    Complexity2[1] <- Ccomplexity(prob2[1,])
    end.time <- Sys.time()
    time.taken2 <- end.time - start.time
    
    #prob2[i,] <- timeOrdered(Series.to.analysis[i,], dimension, delay, patterns, symbols) #Green
    #Entropy2[i] <- shannonNormalized(prob2[i,])
    #Complexity2[i] <- Ccomplexity(prob2[i,])
    
    #prob3[i,] <- randomImputation(Series.to.analysis[i,], dimension, delay, patterns, elements, symbols) #Orange
    #Entropy3[i] <- shannonNormalized(prob3[i,])
    #Complexity3[i] <- Ccomplexity(prob3[i,])
    
    #prob4[i,] <- dataDriven(Series.to.analysis[i,], dimension, delay, patterns, elements, symbols, prob1[i,]) #Wine
    #Entropy4[i] <- shannonNormalized(prob4[i,])
    #Complexity4[i] <- Ccomplexity(prob4[i,])
    
    print(i)
  }
  Entropy.Complexity <- data.frame(Entropy1, Complexity1,Entropy2, Complexity2,Entropy3, Complexity3,Entropy4, Complexity4)
  #Entropy.Complexity <- data.frame(Entropy, Complexity)
  Ent
  ropy.Complexity
}

#Parâmetros definidos
x_0 = 0.1
r = 4
n = 300000
n_series = 10^2
init = 10^3
p = 0.3
i = j = 1
dimension = 6
delay = 1

#Gerando a série
series.map <- series_generator_map(r, x_0, n, n_series, init)

#Contaminando a série
Series.to.analysis <- matrix(nrow = n_series, ncol = n)
while(j <= n_series){
  Series.to.analysis[j,] = atack(1,series.map[j,],p,n)
  j = j + 1
}

#Calculando os valores de Entropia e Complexidade
Entropy.Complexity1 <- Entropy.complexity.values(series.map, dimension, delay)
write.csv(Entropy.Complexity1, file = "EntropyComplexity_Normal.csv")

Entropy.Complexity2 <- Entropy.complexity.values(Series.to.analysis, dimension, delay)
write.csv(Entropy.Complexity2, file = "EntropyComplexity_Time_Ordered.csv")

data <- read.csv("EntropyComplexity_Data_Driven.csv")
case <- read.csv("EntropyComplexity_Complete_Case.csv")
random <- read.csv("EntropyComplexity_Random_Imputation.csv")
time <- read.csv("EntropyComplexity_Time_Ordered.csv")
normal <- read.csv("EntropyComplexity_Normal.csv")

Entropy.Complexity <- data.frame(Entropy = numeric(n_series*5), Complexity = numeric(n_series*5), Imputation = character(n_series*5), stringsAsFactors = FALSE)

Entropy.Complexity$Entropy[1:100] = normal[,2]
Entropy.Complexity$Complexity[1:100] = normal[,3]
Entropy.Complexity$Imputation[1:100] = "Original Series"

Entropy.Complexity$Entropy[101:200] = case[,2]
Entropy.Complexity$Complexity[101:200] = case[,3]
Entropy.Complexity$Imputation[101:200] = "Complete Case"

Entropy.Complexity$Entropy[201:300] = random[,6]
Entropy.Complexity$Complexity[201:300] = random[,7]
Entropy.Complexity$Imputation[201:300] = "Random Imputation"

Entropy.Complexity$Entropy[301:400] = time[,4]
Entropy.Complexity$Complexity[301:400] = time[,5]
Entropy.Complexity$Imputation[301:400] = "Time Ordered"

Entropy.Complexity$Entropy[401:500] = data[,8]
Entropy.Complexity$Complexity[401:500] = data[,9]
Entropy.Complexity$Imputation[401:500] = "Data Driven"

#Entropy.Complexity <- data.frame(EntropyNormal, ComplexityNormal, EntropyCompleteCase, ComplexityCompleteCase, EntropyRandomImputation, ComplexityRandomImputation, EntropyTimeOrdered,ComplexityTimeOrdered, EntropyDataDriven, ComplexityDataDriven)

#Entropy.Complexity <- data.frame(Entropy.Complexity1, Entropy.Complexity2)

# Paleta montada a partir de https://coolors.co/
rainbow_colors <- palette(c("#494947", #DarkGreen
                            "#7494EA", #MutedDarkBlue
                            "#B14AED", #Violet
                            "#44CCFF", #BrightLightBlue
                            "#35FF69", #BrightGreen
                            "#ED8438", #Orange
                            "#E7AD99", #Pink
                            "#C18C5D", #LightBrown
                            "#BF6F00", #DarkYellow
                            "#FB4D3D", #BrightRed
                            "#495867") #DarkGray
)

p <- cotas(dimension)
pHC <- HCPlane(p, Entropy.Complexity, dimension)
pHC <- pHC + scale_colour_manual(values=rainbow_colors[1:5])
print(pHC)