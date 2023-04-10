# Empregos nos países europeus --------------------------------
  
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
  
# Análise de fatores utilizando o pacote psych ----------------------------

# Instalar o pacote psych
#install.packages("psych")
library(psych)

# Utilizando a função "principal" do pacote psych
fit.pc <- principal(paises[,3:11], nfactors=4, rotate="varimax")
fit.pc                  
round(fit.pc$values, 3) # Autovalores
fit.pc$loadings         # Autovetores
fit.pc$loadings[1:9,1:4]
fit.pc$communality      # Comunalidade



# Exemplo pardais

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

fit.pc <- principal(pardais[,2:6], nfactors=2, rotate="varimax")
fit.pc                  
round(fit.pc$values, 3) # Autovalores
fit.pc$loadings         # Autovetores
fit.pc$communality      # Comunalidade

# Criando um gráfico biplot
biplot <- data.frame(fit.pc$scores, Survivorship = pardais$Survivorship)
biplot

library(ggplot2)
ggplot(data = biplot, aes(x = RC1, y = RC2)) +
  geom_point(aes(color = Survivorship)) +
  labs(
    x = "RC1",
    y = "RC2",
    color = "Sobrevivência") +
  scale_color_manual(values=c("black", "red"),
                     labels = c("Não sobreviventes", "Sobreviventes")) +
  theme(legend.position="bottom") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

