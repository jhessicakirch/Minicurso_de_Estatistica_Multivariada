# Exemplo ANOVA -----------------------------------------------------------

# Acessando o conjunto de dados Egyptian skulls

cranios <- read.table("Egyptian skulls.txt", h=T)
cranios

cranios$Period <- as.factor(cranios$Period)

# Maximum.breadth      = largura máxima;    
# Basibregmatic.height = altura basibregmática;
# Basialveolar.length  = comprimento basialveolar; 
# Nasal.height         = altura nasal.


# Visualizando os dados com ggplot

library(ggplot2)

ggplot(data = cranios,
       aes(y = Maximum.breadth)) +
  geom_boxplot(aes (fill = Period))

ggplot(data = cranios,
       aes(x = Maximum.breadth)) +
  geom_histogram() 


# ANOVA
fit <- lm(Maximum.breadth ~ Period, data = cranios)
anova(fit)
qf(p = .95, df1 = 4, df2 = 145)
# Como Fcal = 5,95 é maior que o Ftab = 2,43, rejeita-se H0
# Existe efeito de período na largura máxima do crânio


# Exemplo MANOVA ----------------------------------------------------------

fit <- manova(cbind(Maximum.breadth, Basibregmatic.height, 
                    Basialveolar.length, Nasal.height) 
              ~ Period, data = cranios)

# lambda de Wilks
summary(fit, test = "Wilks")

# maior raiz de Roy
summary(fit, test = "Roy")

# traço de Pillai
summary(fit, test = "Pillai")

# traço de Hotelling-Lawley
summary(fit, test = "Hotelling-Lawley")

# Conclusão: pelo menos um dos vetores de médias de tratamentos difere dos demais.


# Pressupostos ------------------------------------------------------------

#normalidade

#install.packages("MVN")
library(MVN)
mvn(data = cranios[,2:5], mvnTest = "royston", univariatePlot =
      "qqplot")

#homocedasticidade

#install.packages("biotools")
library(biotools)

boxM(data = cranios[,-1], grouping = cranios$Period)
# como p-valor > 0.05, concluimos que as diferenças que
# observamos nas matrizes de correlação não são signficativas. 