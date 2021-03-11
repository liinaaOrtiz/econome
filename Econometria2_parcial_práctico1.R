# activar paquete wooldridge
library(wooldridge)
data("loanapp")
View(loanapp)
str(loanapp) 
attach(loanapp)

# análisis de gráficos de dispersión
plot(approve,white)

# proponer modelo que estime probabilidad de aprovar un prestamo si es blanco
model.logit = glm(approve ~ white, family = binomial(link = "logit"),data = loanapp)
summary(model.logit)

# calcular probabilidad
loanapp$prob = predict(model.logit, loanapp, type='response')
sum(loanapp$prob >=0.6) # da 1989
sum(loanapp$white >=0.6) #da 1681
sum(loanapp$white < 0.6) #da 308

1681/1989 # da 0.8451
308/1989 # da 0.1548

# calcular probablidad del beta
exp(coef(model.logit))
