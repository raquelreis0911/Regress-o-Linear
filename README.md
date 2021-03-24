# Regress-o-Linear
Faça uma regressão linear simples

#Regressão linar simples

#A regressão linear assuma uma relação causal entre duas variáveis contínuas.

#Entrada dos dados e transformação em um data frame

# x é a variável explicativa

x = c(1.4,1.5,1.7,1.9,2.1,2.2,2.4,3.2,3.7,4.2,4.8,5.2)

# y é a variável resposta

y = c(6.5,5.8,7.8,8.1,10.4,12.3,13.1,17.4,20.1,24.5,25.5,27.1)

#Data frame dos dados

dados = data.frame(x,y)
dados

#Ajustando o modelo de regressão aos dados

modelo_regressão = lm(y ~ x, data = dados)
summary(modelo_regressão)

#Note que função lm() é chamada com o formato lm(y ~ x)
#Ou seja, a variável resposta é y e a preditora é x – sempre nessa ordem!

#Coeficiente de correlação linear de Pearson

cor(dados$x, dados$y)

#Para avaliar se esse resultado é significativo, pode-se realizar um Teste de Hipóteses para o Coeficiente de Correlação (supondo que as suposições do teste sejam satisfeitas):

cor.test(dados$x, dados$y)

#Como o p-valor do teste é bem pequeno, conclui-se que o valor do coeficiente de correlação linear de Pearson TEM SIGNIFICÂNCIA ESTATÍSTICA.

#Gráfico de dispersão

plot(y ~ x, pch = 16, data = dados, main = 'Modelo de regressão', xlab = 'Variável preditora', ylab = 'Variável resposta')

#Esta função ajusta a reta do modelo aos dados

abline(modelo_regressão, col = 'red')

#Coeficiente do modelo (intercepto e beta)

coef(modelo_regressão)

#Intercepto (a) = -1.761914
#Beta (b) = 5.823410

Y3 = -1.7619 + 5.8234*3

#Valores previstos pelo modelo

predict(modelo_regressão)

#Resíduos do modelo

residuals(modelo_regressão)

plot(resid(modelo_regressão) ~ predict(modelo_regressão),pch=16) # Resíduos vs. Y esperado
abline(0,0,col="red") # Coloca uma reta no Y = 0
par(mfrow=c(2,2))
plot(modelo_regressão)

#Exemplo 1

#Análise da correlação entre anos de experiência e salário dos gerentes de uma empresa

#Primeiro passo: gráfico de dispersão

plot(gerentes$Experiencia, gerentes$Salario, type = "p", main = 'Gráfico de dispersão da experiência pelo salário', xlab = 'Anos de experiência', ylab = 'Salário')

#Segundo passo: cálculo do Coeficente de Correlação Linear de Pearson

cor(gerentes$Experiencia, gerentes$Salario)
cor.test(gerentes$Experiencia, gerentes$Salario)

#Verificamos que há alta correlação entre os anos de experiência e o salário.

#Terceiro passo: fazer um boxplot com os dados para verificar se não há outliers

install.packages("car")
library(car)
Boxplot(gerentes$Salario)

#Quarto passo: encontrar a reta de previsão entre a experiênca e o salário

modelo_lm_exp_sal = lm(gerentes$Salario ~ gerentes$Experiencia)
modelo_lm_exp_sal

#Quinto passo: prever os valores dos salários conforme o ano de experiência

#O salário previsto para um gerente com 24 anos de experiência é de: 4225.5

Y_sal_24_anos = 1806.3 + 100.8*24

#Concluímos que anos de experiência e o salário de um gerente possum correlação linear positiva de 0,97.
#Pois, verifica-se que o salário aumenta conforme aumenta os anos de experiência.
#Para o aumento de uma unidade no ano de experiência, em média, o salário aumenta 100.8
