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

# colocar as espécies como nome da linha
row.names(dogs) <- dogs$Group
dogs <- dogs[,-1]
dogs


# Padronizando os dados
dogs.padronizado <- scale(dogs)
dogs.padronizado


# Criando uma matriz de distâncias euclidianas 
distancia <- dist(dogs.padronizado, method = "euclidean")
distancia


# Analise de cluster pelo método do vizinho mais próximo
cluster.simple <- hclust(d = distancia, method = "single")

# Os pesos ou distancias de agrupamento
cluster.simple$height

# Dendrograma
# dev.off() # para voltar as configurações normais do plot 
plot(as.dendrogram(cluster.simple))
plot(as.dendrogram(cluster.simple),
     xlab = "", ylab = "Distância", main = "")


# Analise de cluster pelo método do vizinho mais distante e dendrograma
cluster.completa <- hclust(d = distancia, method = "complete")
plot(as.dendrogram(cluster.completa),
     xlab = "", ylab = "Distância", main = "")


# Analise de cluster pelo método de ligação média e dendrograma
cluster.media <- hclust(d = distancia, method = "average")
plot(as.dendrogram(cluster.media),
     xlab = "", ylab = "Distância", main = "")


# Exemplo Paises europeus -------------------------------------------------

paises <- read.table("Euroemp.txt", h=T)
paises

# Unir as informações de país e grupo
paises$name <- apply(paises[, 1:2], 1, paste, collapse = " - ")

# colocar os paises como nome da linha
row.names(paises) <- paises$name
paises <- paises[-c(1,2,12)]
paises

# Padronizando os dados
paises.padronizado <- scale(paises)

# Criando uma matriz de distâncias euclidianas 
distancia <- dist(paises.padronizado, method = "euclidean")

# Analise de cluster pelo método do vizinho mais próximo e dendrograma
cluster.simple <- hclust(d = distancia, method = "single")
par(mar=c(4,1,1,9))
plot(as.dendrogram(cluster.simple),
     xlab = "Distância", ylab = "", main = "", horiz=T)
abline(v = 3, col = "red")
dev.off() #cancelando as configurações do par


# Método k-médias ---------------------------------------------------------

data(iris)

# Observando os dados
library("GGally")
ggpairs(iris,
        columns = 1:4,
        aes(color = Species))

# Utilizando a função kmeans
kmedias <- kmeans(scale(iris[,-5]), centers=3, algorithm = "Lloyd")
kmedias

table(kmedias$cluster, iris$Species)

# Criando uma coluna para agrupamento
iris2 <- iris
iris2$Cluster <- as.factor(kmedias$cluster)

ggplot(data = iris2, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species, shape = Cluster))

ggplot(data = iris2, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species, shape = Cluster))


# Exemplo cães pré-históricos --------------------------------------

dogs <- read.table(file = "Prehistoric dogs.txt", 
                   header = TRUE)

ggpairs(dogs,
        columns = 2:7)

dogs.padronizado <- scale(dogs[,-1])
dogs.padronizado

# Utilizando a função kmeans
kmedias <- kmeans(dogs.padronizado, centers=2, algorithm = "Lloyd")
kmedias

# Criando uma coluna para agrupamento
dogs2 <- dogs
dogs2$Cluster <- as.factor(kmedias$cluster)

dogs2[,c(1,8)]

ggplot(data = dogs2, aes(x = Mand.Breadth, y = Mand.Height, label = Group)) +
  geom_text(aes(color = Cluster), size = 5)
