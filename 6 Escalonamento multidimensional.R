
# Exemplo ilustrativo -----------------------------------------------------

# Distâncias entre as capitais do sudeste do Brasil

d <- matrix(c(  0, 442, 590, 515,
              442,   0, 433, 518,
              590, 433,   0, 940,
              515, 518, 940,   0), ncol = 4, nrow = 4)

capitais <- c("Belo Horizonte", "Rio de Janeiro", "São Paulo", "Vitória")
colnames(d) <- capitais
rownames(d) <- capitais
d

dist <- as.dist(d)

#install.packages("MASS")
library(MASS)

# Escalonamento multidimensional
EM <- isoMDS(dist)
EM$stress
EM$points

# Criando um data.frame com as informações
dados <- data.frame(Capital = capitais,
                       X = EM$points[,1],
                       Y = EM$points[,2])

# Gerando o gráfico
library(ggplot2)
ggplot(data = dados, aes(x = X, y = Y, label = Capital)) +
  geom_text(size = 5)

# Rotacionando o gráfico
angulo <- 0.4
aux1 <- matrix(c(cos(angulo), -sin(angulo), sin (angulo), cos(angulo)),2,2)
aux2 <- EM$points %*% aux1

# Criando um data.frame com as novas informações
dados <- data.frame(Capital = capitais,
                    X = aux2[,1],
                    Y = aux2[,2])

# Gerando o gráfico
# install.packages("ggrepel")
library(ggrepel)
ggplot(data = dados, aes(x = X, y = Y, label = Capital)) +
  geom_point(colour = "red", size = 3) +
  geom_text_repel(size = 5) +
  lims(x = c(-700,700), y = c(-300,700))


# Exemplo: concordância entre parlamentares -------------------------------

nomes <- c("Hunt (R)", "Sandman (R)", "Howard (D)", "Thompson (D)",
          "Frelinghuysen (R)", "Forsythe (R)", "Widnall (R)", 
          "Roe (D)", "Helstoski (D)", "Rodino (D)", "Minish (D)", 
          "Rinaldo (R)", "Maraziti (R)", "Daniels (D)", "Pattern (D)")

d <- matrix(c(0, 8,15,15,10, 9, 7,15,16,14,15,16, 7,11,13,
              8, 0,17,12,13,13,12,16,17,15,16,17,13,12,16,
             15,17, 0, 9,16,12,15, 5, 5, 6, 5, 4,11,10, 7,
             15,12, 9, 0,14,12,13,10, 8, 8, 8, 6,15,10, 7,
             10,13,16,14, 0, 8, 9,13,14,12,12,12,10,11,11,
              9,13,12,12, 8, 0, 7,12,11,10, 9,10, 6, 6,10,
              7,12,15,13, 9, 7, 0,17,16,15,14,15,10,11,13,
             15,16, 5,10,13,12,17, 0, 4, 5, 5, 3,12, 7, 6,
             16,17, 5, 8,14,11,16, 4, 0, 3, 2, 1,13, 7, 5,
             14,15, 6, 8,12,10,15, 5, 3, 0, 1, 2,11, 4, 6,
             15,16, 5, 8,12, 9,14, 5, 2, 1, 0, 1,12, 5, 5,
             16,17, 4, 6,12,10,15, 3, 1, 2, 1, 0,12, 6, 4,
              7,13,11,15,10, 6,10,12,13,11,12,12, 0, 9,13,
             11,12,10,10,11, 6,11, 7, 7, 4, 5, 6, 9, 0, 9,
             13,16, 7, 7,11,10,13, 6, 5, 6, 5, 4,13, 9, 0), 
            ncol = 15, byrow = TRUE)

rownames(d) <- nomes
colnames(d) <- nomes

d

dist <- as.dist(d)

# Escalonamento multidimensional com 2 dimensões
EM2 <- isoMDS(dist, k = 2)
EM2$stress

# Escalonamento multidimensional com 3 dimensões
EM3 <- isoMDS(dist, k = 3)
EM3$stress

# Para k=2

# Criando um data.frame com as informações
dados <- data.frame(Nomes= nomes,
                    X = EM2$points[,1],
                    Y = EM2$points[,2])

# Gerando o gráfico
ggplot(data = dados, aes(x = X, y = Y, label = Nomes)) +
  geom_text(size = 5)

# Colorindo os nomes de acordo com o partido
#install.packages("stringr")
library(stringr)
nomes.novo <- str_split_fixed(nomes, " ",2)

dados$Nomes <- nomes.novo[,1]
dados$Partido <- nomes.novo[,2]

ggplot(data = dados, aes(x = X, y = Y, label = Nomes, Group = Partido)) +
  geom_text_repel(aes(colour = Partido), size = 5) +
  scale_color_manual(values=c("red", "blue"),
                     labels = c("Republicanos", "Democratas"))

# Exemplo: Emprego em Países Europeus -------------------------------------

paises <- read.table("Euroemp.txt", h=T)
paises

# AGR = agricultura, florestal e pesca; 
# MIN = mineração e exploração de pedreiras; 
# FAB = fabricação;
# FEA = fornecimento de energia e água;
# CON = construção; 
# SER = serviços; 
# FIN = finanças;
# SSP = serviços sociais e pessoais; 
# TC = transportes e comunicações.


rownames(paises) <- paises$Country
paises.padronizado <- scale(paises[,3:11])
dist <- dist(paises.padronizado, method = "euclidean")

# Escalonamento multidimensional com 2 dimensões
EM2 <- isoMDS(dist, k = 2)
EM2$stress

# Escalonamento multidimensional com 3 dimensões
EM3 <- isoMDS(dist, k = 3)
EM3$stress

# Para k=2

# Criando um data.frame com as informações
dados <- data.frame(Nomes = paises$Country,
                    Grupo = paises$Group, 
                    X = EM2$points[,1],
                    Y = EM2$points[,2])

# Gerando o gráfico
ggplot(data = dados, aes(x = X, y = Y, label = Nomes, Group = Grupo)) +
  geom_text_repel(aes(colour = Grupo), size = 6) +
  scale_color_manual(values=c("green", "blue", "red", "black"),
                     labels = c("Países de leste europeu", 
                                "Área européia de livre comércio",
                                "União Européia",
                                "Outros países")) +
  theme(legend.position="bottom")
  

