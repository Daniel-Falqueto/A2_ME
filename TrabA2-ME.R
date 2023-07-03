library(tidyverse)
library(lattice)
library(ggplot2)
library(lme4)
library(modelsummary)
library(caret)

#Dados
Q.agua <- read.csv("Water_QualitY_Testing.csv")

# Define o método de treinamento LOO
train.control <- trainControl(method = "LOOCV")

#Modelo com menor AIC e menor desvio residual mas intercepto muito grande
modelo21 <- glm(DissolvedOxygen ~ pH + Turbidity + Temperature + Turbidity:Temperature + Turbidity:pH + Temperature:pH + Turbidity:Temperature:pH, Q.agua, family = gaussian)
#Plot dos coeficientes
modelplot(modelo21)
# Treina o modelo21 para LOO
model21 <- train(DissolvedOxygen ~ pH + Turbidity + Temperature + Turbidity:Temperature + Turbidity:pH + Temperature:pH + Turbidity:Temperature:pH,
               data = Q.agua, method = "glm",
               trControl = train.control)
# Mostra os resultados
print(model21)

# O modelo escolhido
modelo24 <- glm(DissolvedOxygen ~ pH +  Turbidity:Temperature + Turbidity:pH + Temperature:pH, Q.agua, family = gaussian)
model24 <- train(DissolvedOxygen ~ pH +  Turbidity:Temperature + Turbidity:pH + Temperature:pH,
                data = Q.agua, method = "glm",
                trControl = train.control)
print(model24)
modelplot(modelo24)

# Modelo similar ao escolhido 
modelo26 <- glm(DissolvedOxygen ~ pH + I(Turbidity^2)  + Temperature:Turbidity + Turbidity:pH + Temperature:pH, Q.agua, family = gaussian)
model26 <- train(DissolvedOxygen ~ pH + I(Turbidity^2)  + Temperature:Turbidity + Turbidity:pH + Temperature:pH,
               data = Q.agua, method = "glm",
               trControl = train.control)
# Mostra os resultados
print(model26)
modelplot(modelo26)


modelo31 <- glm(DissolvedOxygen ~ pH  + Temperature:Turbidity + Turbidity:pH + Temperature:pH + Conductivity:Temperature, Q.agua, family = gaussian)
model31 <- train(DissolvedOxygen ~  pH + Temperature:Turbidity + Turbidity:pH + Temperature:pH + Conductivity:Temperature,
                 data = Q.agua, method = "glm",
                 trControl = train.control)
# Mostra os resultados
print(model31)

modelo32 <- glm(DissolvedOxygen ~ pH + Temperature:Turbidity + Turbidity:pH + Temperature:pH + Conductivity:pH, Q.agua, family = gaussian)
model32 <- train(DissolvedOxygen ~ pH + Temperature:Turbidity + Turbidity:pH + Temperature:pH + Conductivity:pH,
                 data = Q.agua, method = "glm",
                 trControl = train.control)
# Mostra os resultados
print(model32)

modelo34 <- glm(DissolvedOxygen ~ pH  + Temperature:Turbidity + Turbidity:pH + Temperature:Conductivity, family = gaussian)
model34 <- train(DissolvedOxygen ~ pH  + Temperature:Turbidity + Turbidity:pH + Temperature:Conductivity,
                 data = Q.agua, method = "glm",
                 trControl = train.control)
# Mostra os resultados
print(model34)

modelo35 <- glm(DissolvedOxygen ~ pH + Conductivity + Temperature:Turbidity + Turbidity:pH + Conductivity:pH, Q.agua, family = gaussian)
model35 <- train(DissolvedOxygen ~ pH + Conductivity + Temperature:Turbidity + Turbidity:pH + Conductivity:pH,
                 data = Q.agua, method = "glm",
                 trControl = train.control)
# Mostra os resultados
print(model35)





#possíveis modelos incluindo conditividade
modelo33 <- glm(DissolvedOxygen ~ pH + Turbidity + Conductivity + Turbidity:pH, Q.agua, family = gaussian)
model33 <- train(DissolvedOxygen ~ pH + Turbidity + Conductivity + Turbidity:pH,
                 data = Q.agua, method = "glm",
                 trControl = train.control)
# Mostra os resultados
print(model33)

modelo31 <- glm(DissolvedOxygen ~ Turbidity + Conductivity + Turbidity:pH, Q.agua, family = gaussian)
model31 <- train(DissolvedOxygen ~  Turbidity + Conductivity + Turbidity:pH,
                 data = Q.agua, method = "glm",
                 trControl = train.control)
# Mostra os resultados
print(model31)

modelo35 <- glm(DissolvedOxygen ~ I(pH^2) + Turbidity + Conductivity + Turbidity:pH, Q.agua, family = gaussian)
model35 <- train(DissolvedOxygen ~ I(pH^2) + Turbidity + Conductivity + Turbidity:pH,
                 data = Q.agua, method = "glm",
                 trControl = train.control)
# Mostra os resultados
print(model35)
