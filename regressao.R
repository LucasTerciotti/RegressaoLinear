hr <- read.csv(("https://raw.githubusercontent.com/Efsilvaa/EPSMLRepo/master/Data/housingSmall2.csv"),
                    stringsAsFactors=FALSE,
                    na.strings = c(""))

#preparação
str(hr)
colnames(hr)
colnames(hr)[2]="area"
colnames(hr)[3]="garagem"
colnames(hr)[4]="quartos"
colnames(hr)[5]="preco"
pr <- hr$preco

library(ggplot2)
g <-  ggplot(data = hr, aes(x=area,y=preco))
g <-  g+geom_point()+ 
  labs(title="Preços de Venda de imóveis",y="Preço(xR$1000,00)",x="Área(x100m2)")
g

#regressão
hr_lm <- lm(data=hr,preco~area)
hr_lm
#usando a regressão para prever o valor de uma casa de 200m²
y = hr_lm$coef[1] + 2.0*(hr_lm$coef[2])
y

#estudar esse fitted:
h <- fitted(hr_lm)
help(fitted)
#comparando os valores previstos com os gerados pela previsão do fitted, observa que essa regressão não descreve bem a variação dos dados.


#jeito1:
plot(hr$area,hr$preco)
abline(hr_lm)

#plotar pelo ggplot:
g + geom_abline(intercept = hr_lm$coef[1],
                slope = hr_lm$coef[2])

#regressão pelo ggplot direto:
g <- g + 
  stat_smooth(method = lm, se = FALSE,
              formula = y ~ x,
              colour = "Black", linetype = "solid")
g

#buscando a intersecção:
p <- g + geom_vline(xintercept = 2.0, size = 1, colour = "green", linetype = "longdash")
p <- p + geom_hline(yintercept = 211.1184, size = 1, colour = "green", linetype = "longdash") 

p

#fazendo uma regressão polinomial quadrática e deixando Raw = true (o que mantem a correlação)
help("geom_smooth")
k <- g + stat_smooth(method =lm,se = FALSE,formula=y~poly(x,2, raw = TRUE),colour="Red") 

#Estudos:
k + stat_smooth(method =lm,se = FALSE,formula=y~x+I(x^2)+I(x^3),colour="Blue") 
j <- k + stat_smooth(method =lm,se = FALSE,formula=y~poly(x,17, raw = TRUE),colour="Blue") 

help(lm)
hr_lm2 <- lm(data=hr, hr$preco ~ poly(hr$area,17,raw = TRUE))
hr_lm2
help("??as is")

plot(hr$area,hr$preco)
abline(hr_lm2)
help(abline)

#Estudos:
help("anova.lm")
ano
library(hydroGOF)
hr_lm <- lm(data=hr,preco~area)
hr_lm2 <- lm(data=hr, preco~area+garagem+quartos)
hr_lm3 <- lm(data=hr, preco~area+quartos)
h1 <- fitted(hr_lm)
h2 <- fitted(hr_lm2)
h3 <- fitted(hr_lm3)
anova(hr_lm)
anova(hr_lm2, test="Chisq")
anova(hr_lm3, test="Chisq")
mse1 <- mse(h1,pr)
mse2 <- mse(h2,pr)
mse3 <- mse(h3,pr)
#as previsões feitas por h2 permitiram reduzir bem o valor do MSE da regressão, tornando-a mais eficiente que h1

#prevendo um valor com base na regressão hr_lm2:
a=1
g=0
q=2

y = 32.333993+(a*35.244193)+(g*3.701131)+(q*37.128)


