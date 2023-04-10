# Importando o cojunto de dados dos pardais sobreviventes da tempestade

# Survivorship =   sobrevivência
# Total_length =   comprimento total;
# Alar_extent =    extensão alar;
# L_beak_head =    comprimento do bico e cabeça;
# L_humerous =     comprimento do úmero;
# L_keel_sternum = comprimento da quilha do esterno.


# dados em .txt
pardais <- read.table(file = "Bumpus_sparrows.txt", 
                      header = TRUE)
pardais


# Matrizes de correlação e de covariância
R <- cor(pardais[,-1])
round(R, 3)

C <- var(scale(pardais[,-1]))
round(C, 3)

# Autovalores e autovetores da matriz
eigen(C)

# Usando prcomp para a ACP com scale = TRUE significa que os dados serão padronizados
pardais_acp <- prcomp(pardais[,-1], scale = TRUE)
summary(pardais_acp)

pardais_acp$sdev # raíz quadrada dos autovalores
pardais_acp$rotation # autovetores


# Interpretando os autovetores
print(pardais_acp) 
# PC1: Pode ser chamado de indice de tamanho das pardocas
# PC2: contraste entre extensão alar, comprimento do bico e cabeça,
#      comprimento do úmero contra o comprimento da quilha do esterno. Pode
#      representar uma diferença de forma entre pardocas.


# Criando um gráfico com os dois primeiros CP
biplot <- data.frame(pardais_acp$x, Survivorship = pardais$Survivorship)
biplot

library(ggplot2)
ggplot(data = biplot, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Survivorship)) +
  labs(
    x = "CP1 (72,32%)",
    y = "CP2 (10,63%)",
    color = "Sobrevivência") +
  scale_color_manual(values=c("black", "red"),
                     labels = c("Não sobreviventes", "Sobreviventes")) +
  theme(legend.position="bottom") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)


# Utilizando o pacote factoextra
#install.packages("factoextra")
library(factoextra)
fviz_pca_biplot(pardais_acp, ggtheme = theme_grey())


# Exemplo 2 - Empregos nos países europeus --------------------------------

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

# Usando prcomp para a ACP com scale = TRUE significa que os dados serão padronizados
paises_acp <- prcomp(paises[,-c(1,2)], scale=TRUE)
summary(paises_acp)


# Interpretando os autovetores
print(paises_acp)   
# CP1 é um contraste entre os números engajados em AGR (agricultura, florestal 
#     e pesca) MIN (mineração e exploração de pedreiras) versus os números 
#     engajados em outras ocupações.
# CP2 é o contraste entre os números para MAN (fabricação) e TC (transporte e 
#     comunicação) com os números em CON (construção), SER (indústrias e serviços) 
#     e FIN (finança)
# CP3 é o contraste entre os números para MIN, SPS e TC com os números em AGR, 
#     MAN e PS.
# CP4 é o contraste entre os números para CON com os números em FIN e SPS.


# Criando um gráfico com os dois primeiros CP
biplot <- data.frame(paises_acp$x, Country = paises$Country)
biplot

ggplot(data = biplot, aes(x = PC1, y = PC2, label = Country)) +
  geom_text( size = 3) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(
    x = "CP1 (34,58%)",
    y = "CP2 (20,10%)")

# Exemplo agrupamento + ACP com cães pré-históricos --------------------------------------

# Mand.Breadth = Largura da mandíbula;
# Mand.Height = Altura da mandíbula;
# Mol1.Length =  Comprimento do primeiro molar;
# Mol1.Breadth = Largura do primeiro molar;
# Mol1.3.Length = Comprimento do primeiro ao terceiro molar inclusive;
# Mol1.4.Length = Comprimento do primeiro ao quarto molar inclusive;

dogs <- read.table(file = "Prehistoric dogs.txt", 
                   header = TRUE)


dogs_acp <- prcomp(dogs[,-1], scale=TRUE)
summary(dogs_acp)
print(dogs_acp)   

CPs <- data.frame(Group = dogs$Group, dogs_acp$x)

# Utilizando a função kmeans
kmedias <- kmeans(CPs[,-1], centers=2, algorithm = "Lloyd")
kmedias

# Criando uma coluna para agrupamento
CPs$Cluster <- as.factor(kmedias$cluster)

ggplot(data = CPs, aes(x = PC1, y = PC2, label = Group)) +
  geom_text(aes(color = Cluster), size = 5)
