##### Importando os dados #####
dados <- read.csv("path/vgsales.csv", nrows = 15)
View(dados)

##### Pré Análises Estatísticas #####
summary(dados$NA_Sales)
summary(dados$EU_Sales)
summary(dados$JP_Sales)

eixox <- "Vendas em milhares"
hist(dados$JP_Sales, main="Japão", xlab=eixox)
shapiro.test(dados$JP_Sales)

hist(dados$EU_Sales, main="Estados Unidos", xlab=eixox)
shapiro.test(dados$EU_Sales)

hist(dados$NA_Sales, main="Europa", xlab=eixox)
shapiro.test(dados$NA_Sales)

title <- "Vendas nas maiores regiões"
subtitle <- c("América do Norte", "Europa", "Japão")
boxplot(dados$NA_Sales, dados$EU_Sales, dados$JP_Sales, main=title, names=subtitle)

##### Armazenando os dados em um DataFrame #####
regiao <- c(rep("NA_Sales", 15), rep("EU_Sales", 15), rep("JP_Sales", 15))
vendas <- c(dados$NA_Sales, dados$EU_Sales, dados$JP_Sales)
df <- data.frame(regiao, vendas)

##### Análise de Normalidade e Homocedasticidade #####
shapiro.test(df$vendas)
hist(df$vendas)
bartlett.test(df$vendas~df$regiao)

##### Análise de Variância - ANOVA #####
dados.aov <- aov(vendas ~ regiao, data = df)
summary(dados.aov)