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


# transformando sobrevivencia em fator
pardais$Survivorship
is.factor(pardais$Survivorship)

pardais$Survivorship <- as.factor(pardais$Survivorship)
pardais$Survivorship
# character serve para sequências de caracteres, ou seja, textos. 
# factor serve para categorias.

# dados em .csv
# pardais2 <- read.csv(file = "Bumpus_sparrows.csv")


# Gráfico de dispersão ----------------------------------------------------

# Gráfico de dispersão 2D utilizando a função plot

plot(x = pardais$Total_length, 
     y = pardais$Alar_extent)

# Adicionando os rótulos dos eixos x e y
plot(x = pardais$Total_length, 
     y = pardais$Alar_extent,
     xlab = "Comprimento total (mm)",
     ylab = "Extensão alar (mm)")

# Adicionando a informação 
par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(x = pardais$Total_length, 
     y = pardais$Alar_extent,
     xlab = "Comprimento total (mm)",
     ylab = "Extensão alar (mm)",
     col = pardais$Survivorship,
     pch = 19)

legend("topright", inset=c(-.60,0), legend=c("NS","S"), pch=19, title="Sobrevivência",  col = c(1,2))


# Utilizando pacotes 

# install.packages("ggplot2")  # Instala os pacotes ggplot2
install.packages(c("ggplot2", "scatterplot3d", "rgl", "GGally", "tidyr")) 


# Utilizando o pacote ggplot2
library(ggplot2)

ggplot(data = pardais, aes(x = Total_length, y = Alar_extent)) +
  geom_point()

ggplot(data = pardais, aes(x = Total_length, y = Alar_extent)) +
  geom_point(aes(color = Survivorship)) 

ggplot(data = pardais, aes(x = Total_length, y = Alar_extent)) +
  geom_point(aes(color = Survivorship)) +
  labs(
    x = "Comprimento total (mm)",
    y = "Extensão alar (mm)",
    color = "Sobrevivência") +
  scale_color_manual(values=c("black", "red"),
                     labels = c(NS = "Não sobreviventes", S = "Sobreviventes")) +
  theme(legend.position="bottom")


# Gráfico de dispersão 3D

#install.packages("scatterplot3d") # Instala o pacote ScatterPlot3D
library("scatterplot3d")
scatterplot3d(x = pardais$Total_length, 
              y = pardais$Alar_extent, 
              z = pardais$L_beak_head)

?scatterplot3d


#install.packages("rgl")
library("rgl")
plot3d(x = pardais$Total_length,
       y = pardais$Alar_extent,
       z = pardais$L_beak_head)


plot3d(x = pardais$Total_length,
       y = pardais$Alar_extent,
       z = pardais$L_beak_head,
       xlab = "Comprimento total",
       ylab = "Extensao alar", 
       zlab = "Comprimento do bico e cabeca",
       col = as.integer(pardais$Survivorship),
       size=5)
legend3d("topright", legend = levels(pardais$Survivorship), col = c(2,1), pch=19)


# Matriz de dispersão ---------------------------------------------------

pairs(x = pardais[,-1])

pairs(pardais[,-1], 
      labels = c("C. total", 
                 "Extensão alar", 
                 "C. bico \n e cabeça", 
                 "C. úmero", 
                 "C. quilha do \n esterno"),
      col = c("black", "red")[pardais$Survivorship],
      pch = 19)


#install.packages("GGally")
library("GGally")
ggpairs(pardais[,-1])

ggpairs(pardais,
        columns = 2:6,
        aes(color = Survivorship),
        columnLabels =  c("C. total", 
                          "Extensão \n alar", 
                          "C. bico e \n cabeça", 
                          "C. úmero", 
                          "C. quilha do \n esterno"),
        legend = 1) +
  scale_color_manual(values = c(S = "black", NS = "red")) +
  scale_fill_manual(values = c(S = "black", NS = "red")) +
  labs(color = "Survivorship") +
  theme(legend.position = "bottom") 


# Gráfico de perfis -------------------------------------------------------

# Importando o cojunto de dados dos cães pré históricos

# Mand.Breadth = Largura da mandíbula;
# Mand.Height = Altura da mandíbula;
# Mol1.Length =  Comprimento do primeiro molar;
# Mol1.Breadth = Largura do primeiro molar;
# Mol1.3.Length = Comprimento do primeiro ao terceiro molar inclusive;
# Mol1.4.Length = Comprimento do primeiro ao quarto molar inclusive;


# dados em .txt
dogs <- read.table(file = "Prehistoric dogs.txt", 
                   header = TRUE)
dogs

dogs$Group <- as.factor(dogs$Group)

#install.packages("tidyr")
library("tidyr")
dogs.long <- gather(data = dogs, 
                     key = "Variaveis", 
                     value = "Valor", 
                     -c(Group))

ggplot(dogs.long, 
       aes(x = Variaveis, 
           y = Valor, 
           group = Group)) + 
  geom_line() + geom_point()


# Reordenando as variáveis
dogs.long$Variaveis = with(dogs.long, reorder(Variaveis, Valor))

ggplot(dogs.long, 
       aes(x = Variaveis, 
           y = Valor, 
           group = Group)) + 
  geom_line() + geom_point()



ggplot(dogs.long, 
       aes(x = Variaveis, 
           y = Valor, 
           group = Group)) + 
  geom_line(aes(linetype=Group,
                color = Group),
            size = 1) + 
  geom_point(aes(color=Group)) +
  theme(legend.position="bottom") +
  labs(linetype='Grupo',
       color = 'Grupo',
       x = "",
       y = "Medidas da mandíbula") +
  scale_x_discrete(labels=c("Mand.Breadth" = "Largura mandíbula", 
                            "Mand.Height" = "Altura mandíbula",
                            "Mol1.Length" = "Compr. 1 molar",
                            "Mol1.Breadth" = "Largura 1 molar",
                            "Mol1.3.Length" = "Compr. 1 a 3 molar",
                            "Mol1.4.Length" = "Compr. 1 a 4 molar")) +
  scale_colour_manual(
    values = c(rep("black", 6), "red"),
    aesthetics = c("colour"))
