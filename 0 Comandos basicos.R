# Tipos de dados ----------------------------------------------------------

# O R possui basicamente os seguintes tipos ou classes de dados:

# numeric (números)

1 + 2

a <- 1
a
b <- 2
b

a + b


# character (valores nominais, não numéricos)
c <- 'aluno'
c

a + c #erro: não é possível somar objetos não numéricos


# logical
d <- TRUE
e <- FALSE

  
# vetores (conjunto de elementos pertencentes à mesma classe)
# a função c() (concatenar) pode ser utilizada para criar vetores de objetos
x <- c(8, 9, 10)
x

x[3] # elemento na terceira posição do vetor x

y <- c('x', 'y', 'z')
y


# matrizes
A <- matrix(data = c(1, 2, 3, 4),
            ncol = 2,
            nrow = 2)
A

A[1,2] # elemento da primeira linha e segunda coluna da matriz A

B <- matrix(data = c(1, 2, 3, 4, 5, 6),
            ncol = 3)
B

C <- matrix(data = c(1, 2, 3, 4, 5, 6),
            ncol = 2,
            byrow = TRUE)
C

A + A   # soma de matrizes
B %*% C # multiplicação de matrizes


# data.frame
# são comumente utilizados na importação e criação de dados
dados <- data.frame(Nome = y, Valor = x)
dados
