# Workshop R - UFPE
# Encontro 4

# Dados para o encontro

## Atividade prática Encontro 2 ----

setwd("./dados/")

# carregando arquivos CENSO ESCOLAR 2016
load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")

# carregando dados PNUD
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(readxl) == F) install.packages('readxl'); require(readxl)

setwd("./dados/")
pnud <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet = 2)
head(pnud)
unique(pnud$ANO)

# selecionando dados de 2010 e do Estado de Pernambuco
pnud_pe_2010 <- pnud %>% filter(ANO == 2010 & UF == 26)

rm(pnud)  # removendo base pnud

# Processando bases de dados do CENSO ESCOLAR conforme enunciado e adicionando 
# outras variáveis

# Turmas
turmas_pe_sel <- turmas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_turmas = n(), 
            turmas_disc_prof = sum(IN_DISC_PROFISSIONALIZANTE, na.rm = T),
            turmas_disc_inf = sum(IN_DISC_INFORMATICA_COMPUTACAO, na.rm = T),
            turmas_disc_mat = sum(IN_DISC_MATEMATICA, na.rm = T),
            turmas_disc_pt = sum(IN_DISC_LINGUA_PORTUGUESA, na.rm = T),
            turmas_disc_en = sum(IN_DISC_LINGUA_INGLES, na.rm = T))
# verificacao
dim(turmas_pe_sel)[1] == length(unique(turmas_pe$CO_MUNICIPIO))
summary(turmas_pe_sel)

# Escolas
escolas_pe_sel <- escolas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_escolas = n(), 
            n_escolas_priv = sum(TP_DEPENDENCIA == 4, na.rm = T),
            escolas_func = sum(TP_SITUACAO_FUNCIONAMENTO == 1, na.rm = T),
            escolas_agua_inex = sum(IN_AGUA_INEXISTENTE, na.rm = T),
            escolas_energia_inex = sum(IN_ENERGIA_INEXISTENTE, na.rm = T),
            escolas_esgoto_inex = sum(IN_ESGOTO_INEXISTENTE, na.rm = T),
            escolas_internet = sum(IN_INTERNET, na.rm = T),
            escolas_alimentacao = sum(IN_ALIMENTACAO, na.rm = T))

# verificacao
dim(escolas_pe_sel)[1] == length(unique(escolas_pe$CO_MUNICIPIO))
summary(escolas_pe_sel)

# Docentes
docentes_pe_sel <- docentes_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_docentes = n(), 
            docentes_media_idade = mean(NU_IDADE),
            docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T),
            docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T)
  )

# verificacao
dim(docentes_pe_sel)[1] == length(unique(docentes_pe$CO_MUNICIPIO))
summary(docentes_pe_sel)

# Matriculas
matriculas_pe_sel <- matricula_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_matriculas = n(), 
            alunos_media_idade = mean(NU_IDADE),
            alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T),
            alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T),
            alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T),
            matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T),
            matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T),
            matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T)
  )

# verificacao
dim(matriculas_pe_sel)[1] == length(unique(matricula_pe$CO_MUNICIPIO))
summary(matriculas_pe_sel)

# UNINDO BASES CENSO E PNUD ----------------------------------------------------

# matriculas
censo_pnud_pe_sel <- pnud_pe_2010 %>% full_join(matriculas_pe_sel, 
                                                by = c("Codmun7" = "CO_MUNICIPIO")
)
dim(pnud_pe_2010)
dim(matriculas_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)

# escolas
censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(escolas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)
dim(escolas_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)

# turmas
censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(turmas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)
dim(turmas_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)

# docentes
censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(docentes_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)
dim(docentes_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)

# salvando nova base -----------------------------------------------------------
setwd("./dados")
save(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.RData")
write.csv2(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.csv",
           row.names = F)

rm(list = ls())  # limpando area de trabalho

# carregando nova base -----------------------------------------------------------
setwd("./dados")
load("2016_censo_pnud_pe_sel.RData")

# Estatística Descritiva ----

## Medidas de posição ----

# Média Aritmética
mean(censo_pnud_pe_sel$n_matriculas)

# Mediana
median(censo_pnud_pe_sel$n_matriculas)

# Moda
y <- c(sample(1:10, 100, replace = T))
table(y)
table(y)[which.max(table(y))]

# Quantis
summary(censo_pnud_pe_sel$n_matriculas)

# Percentis / Decis...
?quantile
quantile(censo_pnud_pe_sel$n_matriculas, probs = seq(0,1, .01))

## Medidas de dispersão ----

# Amplitude
max(censo_pnud_pe_sel$n_matriculas) - min(censo_pnud_pe_sel$n_matriculas)
# ou
range(censo_pnud_pe_sel$n_matriculas)[2] - range(censo_pnud_pe_sel$n_matriculas)[1]

# Variância
var(y)

# Desvio padrão
sd(y)
# ou
y %>% var %>% sqrt

# Coeficiente de variação
100*sd(y)/mean(y)

## Atividade prática
# Utilizando a variável IDHM, calcule:
#   - a média
# - os decis
# - o segundo quartil ou mediana
# - a amplitude da amostra
# - a variância e o desvio padrão da amostra.

# Variáveis aleatórias ----

## Variáveis aleatórias discretas ----

# Distribuição Binomial

p <- 0.25 # probabilidade
n <- 100  # número de tentativas
x <- 20  # número de sucessos em n tentativas

dbinom(x, n, p)

x <- c(0:50)
bin <- dbinom(x, n, p)
plot(x, bin, type = "h", xlab = "Sucessos", ylab = "Probabilidade",
     main = "Distribuição binomial")

### Distribuição De Poisson

x <- 2
lambda <- 2.3

# distribuição de Poisson com parâmetros x e lambda:
dpois(x,lambda)

x <- 0:10
poisson <- dpois(x, lambda)

plot(x, poisson, xlab = "Número de eventos no tempo",
     ylab = "Probabilidade de Poisson", main = "Distribuição de Poisson")

### Atividade prática

# Num determinado posto de gasolina, dados coletados indicam que um  número  médio  de  6  clientes  por  hora  param para  colocar  gasolina  numa bomba.
# 
# - Qual é a probabilidade de 3 clientes pararem qualquer hora? 
#   - Qual é a probabilidade de 3 clientes ou menos pararem em qualquer hora? 
#   - Qual é o valor esperado, a média, e o desvio padrão para esta distribuição?
  
## Variáveis aleatórias contínuas ----
  
### Distribuição Normal
  
1-pnorm(179, 170, 6)  # pnorm(x, média, desvio padrão)

#- Qual a altura em que a probabilidade de encontrarmos valores menores que ela
#seja de 80%?
  
qnorm(0.8, 170,6)

# Inferência Estatística ----

## Coeficiente de correlação ----

movies <- read.csv(url("http://s3.amazonaws.com/dcwoods2717/movies.csv"))
head(movies)
str(movies)

# criando variável profit (lucro)
movies <- movies %>% mutate(profit = gross - budget)

# grafico de dispersao 
plot(movies$rating, movies$profit)

# correlacao
cor(movies$rating, movies$profit)

# teste de correlacao
cor.test(movies$rating, movies$profit)  # p-valor < .00000000000000022

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(GGally) == F) install.packages('GGally'); require(GGally)

movies <- read.csv(url("http://s3.amazonaws.com/dcwoods2717/movies.csv"))
movies <- movies %>% mutate(profit = gross - budget)

# correlacao
ggcorr(movies[, c(4:12)])

if(require(GGally) == F) install.packages('GGally'); require(GGally)

# correlacao
ggcorr(movies[, c(4:12)], label = T)

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)

# Forte correlação positiva:

# Plot votes vs reviews
qplot(movies$votes, 
      movies$reviews, 
      data = movies, 
      geom = c("point", "smooth"), 
      method = "lm", 
      alpha = I(1 / 5), 
      se = F)

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)

# Fraca correlação positiva:

# Plot profit over years
qplot(movies$year, 
      movies$profit, 
      data = movies, 
      geom = c("point", "smooth"), 
      method = "lm", 
      alpha = I(1 / 5), 
      se = F)

# Regressão linear ----

# construindo variavel dependente
censo_pnud_pe_sel$docentes_esc <- censo_pnud_pe_sel$n_docentes /
  censo_pnud_pe_sel$n_escolas

reg <- lm(IDHM_E ~ + docentes_esc + n_matriculas + escolas_energia_inex, 
          data = censo_pnud_pe_sel)

names(reg)

# Os mais importantes listados são os seguintes:

# regressão$fitted.values ou predict(), que calcula os valores preditos da variável 
# resposta para cada elemento da amostra (faz uma previsão);

# regressão$residuals: calcula o erro ou os resíduos (valor observado - valor predito)
# para cada ponto da amostra;
# regressão$coefficients:  obtém uma estimativa dos coeficientes da regressão

options(scipen=999)
summary(reg)

# Atividade Prática 

# * Utilize a base `censo_pnud_pe_sel` para selecionar mais de duas variáveis contínuas de interesse e 
# produzir graficamente a matriz de correlação, apresentando o coeficiente de correlação calculado.

# Comunicando nossas análises ----

# carregando pacotes
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(lubridate) == F) install.packages("lubridate"); require(lubridate)
if(require(scales) == F) install.packages("scales"); require(scales)

# carregando dados
setwd("./dados/")

# carregando arquivos CENSO ESCOLAR 2016
load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")

# Gráficos com o `ggplot2` ----

## Gráficos de dispersão ----

ggplot(data = censo_pnud_pe_sel, aes(x = n_matriculas, y = n_docentes) ) + 
  geom_point(color = "red", size = 2) +
  labs(x = "Número de Matrículas", y = "Número de Docentes")

# outro exemplo e matriz de correlacao
# pacotes 
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(GGally) == F) install.packages('GGally'); require(GGally)

# dados
movies <- read.csv(url("http://s3.amazonaws.com/dcwoods2717/movies.csv"))
head(movies)
str(movies)

# criando variável profit (lucro)
movies <- movies %>% mutate(profit = gross - budget)

# Forte correlação positiva:

# Plot votes vs reviews
qplot(movies$votes, 
      movies$reviews, 
      data = movies, 
      geom = c("point", "smooth"), 
      method = "lm", 
      alpha = I(1 / 5), 
      se = F)

# Fraca correlação positiva:

# Plot profit over years
qplot(movies$year, 
      movies$profit, 
      data = movies, 
      geom = c("point", "smooth"), 
      method = "lm", 
      alpha = I(1 / 5), 
      se = F)

# correlacao
ggcorr(movies[, c(4:12)])

# correlacao
ggcorr(movies[, c(4:12)], label = T)

## Gráficos de coluna ----

# cor / raça docentes
ggplot(docentes_pe, aes(as.factor(TP_COR_RACA))) + 
  geom_bar()

# trabalhando com fatores ----

tamanho <- factor(c("pequeno", "grande", "médio", "pequeno", "médio"))
tamanho

tamanho <- factor(c("pequeno", "grande", "médio", "pequeno", "médio"),
                  levels = c("pequeno", "médio", "grande"))
tamanho

# verificando base censo_pnud

class(censo_pnud_pe_sel$Município)
censo_pnud_pe_sel$Município <- as.factor(censo_pnud_pe_sel$Município)
censo_pnud_pe_sel$Município

# ordenando factors por uma variável
censo_pnud_pe_sel$Município <- factor(censo_pnud_pe_sel$Município,  
                                      levels= unique(as.character(
                                        censo_pnud_pe_sel$Município[order(
                                          censo_pnud_pe_sel$IDHM, 
                                          decreasing = T)])))

# - Atividade em aula:
#   Faça novamente o gráfico de barras com a variável `TP_COR_RACA` do banco `docentes`, 
# mas de modo que a categoria `0` seja a última do lado direito do gráfico.

## Histograma ----

ggplot(censo_pnud_pe_sel, aes(n_escolas)) + 
  geom_histogram()

ggplot(censo_pnud_pe_sel, aes(n_escolas)) + 
  geom_histogram(binwidth = 10)

## Gráficos de linha ----

# ajustando dados
matricula_pe_nasc <- matricula_pe %>% 
  select(NU_MES, NU_ANO) %>%
  mutate(nascimento = make_date(NU_ANO, NU_MES)) %>%
  filter(nascimento >= "2000-01-01" & nascimento <= "2009-01-01") %>%
  group_by(nascimento) %>% 
  summarise(n_matriculas = n())

summary(matricula_pe_nasc)

ggplot(matricula_pe_nasc, aes(nascimento, n_matriculas)) + 
  geom_line() +
  scale_x_date(labels = date_format("%m-%Y"))

ggplot(matricula_pe_nasc, aes(nascimento, n_matriculas)) + 
  geom_line() +
  scale_x_date(labels = date_format("%b-%Y"), 
               breaks = date_breaks("6 months"))

ggplot(matricula_pe_nasc, aes(nascimento, n_matriculas)) + 
  geom_line() +
  scale_x_date(labels = date_format("%b-%Y"), 
               breaks = date_breaks("year"))

ggplot(matricula_pe_nasc, aes(nascimento, n_matriculas)) + 
  geom_line() +
  scale_x_date(labels = date_format("%b-%Y"), 
               breaks = date_breaks("2 years"))

ggplot(matricula_pe_nasc, aes(nascimento, n_matriculas)) + 
  geom_line() +
  scale_x_date(labels = date_format("%b-%Y"), 
               breaks = date_breaks("year")) +
  theme(axis.text.x = element_text(colour='black', angle = 45, hjust = 1, vjust = 1, 
                                   face = "bold"))

## Faceting ----

ggplot(docentes_pe, aes(as.factor(TP_COR_RACA))) + 
  geom_bar() 

docentes_pe_sel <- docentes_pe %>% 
  select(TP_SEXO, TP_COR_RACA) %>%
  group_by(TP_SEXO, TP_COR_RACA) %>% 
  summarise(n_docentes = n()) %>%
  mutate(prop = n_docentes/sum(n_docentes))

docentes_pe_sel

ggplot(docentes_pe_sel, aes(as.factor(TP_COR_RACA), y = prop)) + 
  geom_bar(stat = "identity") + facet_wrap(~TP_SEXO)

matriculas_pe_sel <- matricula_pe %>% 
  select(TP_SEXO, TP_COR_RACA) %>%
  group_by(TP_SEXO, TP_COR_RACA) %>% 
  summarise(n_matriculas = n()) %>%
  mutate(prop = n_matriculas/sum(n_matriculas))

matriculas_pe_sel

graph <- ggplot(matriculas_pe_sel, aes(as.factor(TP_COR_RACA), y = prop)) + 
  geom_bar(stat = "identity") + facet_wrap(~TP_SEXO)

setwd("./imagens")
ggsave(filename = "matriculas_cor_raca_sx.png", plot = graph)

if(require(png) == F) install.packages('png'); require(png)
if(require(grid) == F) install.packages('grid'); require(grid)

## Box-plot ----

# mtcars
ggplot(mtcars) + 
  geom_boxplot(aes(x = as.factor(cyl), y = mpg))

# censo_pnud
graph <-  ggplot(censo_pnud_pe_sel, aes(0, IDHM)) + 
  geom_boxplot()  +
  scale_x_discrete() +
  geom_jitter(alpha = .25, size = 1.5) +
  xlab(NULL) + 
  ylab(NULL) + 
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "grey")) +
  theme(axis.text.y = element_text(colour='black', angle = 0, size = 12, hjust = 0, vjust = 0.5)) 

graph

setwd("./imagens")
ggsave(filename = "box_plot.png", plot = graph)

## Coeficientes Regressão ----

if(require(dotwhisker) == F) install.packages('dotwhisker'); require(dotwhisker)
if(require(broom) == F) install.packages('broom'); require(broom)

censo_pnud_pe_sel$docentes_esc <- censo_pnud_pe_sel$n_docentes /
  censo_pnud_pe_sel$n_escolas

reg <- lm(IDHM_E ~ docentes_esc + n_matriculas + escolas_energia_inex, 
          data = censo_pnud_pe_sel)
summary(reg)

dwplot(reg,  vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

setwd("./imagens")
png(filename="modelo_idhe.png")
dwplot(reg,  vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))
dev.off()

# Mapas ----

## Mapas com o `ggplot2` ----
  
# pacotes ----------------------------------------------------------------------
if(require(rgdal) == F) install.packages("rgdal"); require(rgdal)
if(require(maptools) == F) install.packages("maptools"); require(maptools)
if(require(ggmap) == F) install.packages("ggmap"); require(ggmap)
if(require(mapproj) == F) install.packages("mapproj"); require(mapproj)
if(require(RgoogleMaps) == F) install.packages("RgoogleMaps"); require(RgoogleMaps)


if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)

# carregando bases ----------------------------------------------------------

# Carregando shapefile
shapefile_pe <- readOGR("./pe_municipios/", "26MUE250GC_SIR")

shapefile_pe@data

plot(shapefile_pe)

# Convertendo o shapefile para dataframe ----
shapefile_df <- fortify(shapefile_pe)

dim(shapefile_df)
names(shapefile_df)
head(shapefile_df)

shapefile_data <- fortify(shapefile_pe@data)
shapefile_data$id <- row.names(shapefile_data)

shapefile_df <- full_join(shapefile_df, shapefile_data, by="id")

names(shapefile_df)
head(shapefile_df)

# Agora vamos remover Fernando de Noronha (2605459) da base e produzir o mapa novamente ----

shapefile_df <- shapefile_df %>% filter(CD_GEOCMU != "2605459")

# mapa ggplot
map <- ggplot() +
  geom_polygon(data = shapefile_df, 
               aes(x = long, y = lat, group = group, fill = IDHM), 
               colour = "black", fill = 'white', size = .2) + 
  coord_map()

map

# Fazendo união com a base do CensoEscolar + PNUD ----
censo_pnud_pe_sel$Codmun7 <- as.factor(censo_pnud_pe_sel$Codmun7)
shapefile_df <- shapefile_df %>% left_join(censo_pnud_pe_sel, 
                                           by = c("CD_GEOCMU" = as.character("Codmun7")))
head(shapefile_df)

# sugestão para escolha de cores ----
# http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
# https://www.w3schools.com/colors/colors_picker.asp
# https://ggplot2.tidyverse.org/reference/scale_gradient.html
# https://www.w3schools.com/colors/colors_picker.asp

# mapa IDHM ----
map <- ggplot() + geom_polygon(data = shapefile_df, 
                               aes(x = long, y = lat, group = group, fill = IDHM), 
                               colour = "gray") +
  theme_void() +
  coord_map()

map

# mapa IDHM - fundo vazio ----
map <- ggplot() + geom_polygon(data = shapefile_df, 
                               aes(x = long, y = lat, group = group, fill = IDHM), 
                               colour = "gray", size = .2) +
  theme_void() +  # essa é a função que dexa o fundo vazio
  coord_map()

map

# mapa IDHM - fundo vazio e nova cor da escala ----

map <- ggplot() + geom_polygon(data = shapefile_df, 
                               aes(x = long, y = lat, group = group, fill = IDHM), 
                               colour = "gray", size = .2) +
  theme_void() +  # essa é a função que dexa o fundo vazio
  #  scale_fill_manual(values = c("Black", "Orange", "Brown")) +
  scale_fill_gradient2(low = "#f03b20", mid="#feb24c", high = "#ffeda0",
                       midpoint = median(shapefile_df$IDHM),
                       limits = range(shapefile_df$IDHM)) +
  coord_map()

map

# salvando mapa
setwd("./imagens")
ggsave(filename = "pernambuco_municipios.png", map)


### Atividade prática

# 1. Faça um mapa apenas com os municípios da Regiao Metropolitana do Recife e seu IDHM.
# Use o link abaixo para auxiliar nessa tarefa:
#   - https://pt.wikipedia.org/wiki/Regi%C3%A3o_Metropolitana_do_Recife

## Mapas com o `ggplot2` e o Google Maps -------

# https://cloud.google.com/maps-platform/user-guide/account-changes/#no-plan

# git do pacote ggmap
# https://github.com/dkahle/ggmap
# https://github.com/dkahle/ggmap/issues/51

# instalando pacote direto do repositório Git
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

# carregando pacote
library(ggmap)

# registrando api
register_google(key = "sua_api_aqui")

# mapas de Recife
recife1 <- get_map("Recife")
ggmap(recife1)

recife1 <- get_map("Recife", maptype = c("satellite"))
ggmap(recife1)

recife2 <- get_map("Recife", zoom = 10)
ggmap(recife2)

recife3 <- get_map("Recife", zoom = 12)
ggmap(recife3)

recife4 <- get_map("Av. Acadêmico Hélio Ramos – Cidade Universitária, Recife – PE, 
                   50670-901", zoom = 15)
ggmap(recife4)

cfch <- geocode("CFCH – Cidade Universitária, Recife – PE")
cfch

ggmap(recife4) + geom_point(data = cfch, aes(lon, lat), color = "red", size = 2)  

reitoria_ufpe <- geocode("Reitoria UFPE - Cidade Universitária, Recife - PE")

mapa_ufpe <- ggmap(recife4) + geom_point(data = cfch, aes(lon, lat), 
                                         color = "red", size = 2) +  
  geom_point(data = reitoria_ufpe, aes(lon, lat), color = "blue", size = 2)

mapa_ufpe

setwd("./imagens")
ggsave(filename = "mapa_ufpe.png", mapa_ufpe)


### Atividades em aula: 
# - Use o mapa `recife3` para apresentar a imagem do google maps e adicione as fronteiras 
# da cidade de Recife.
# - Obtenha um mapa do Google Maps com zoom num endereço qualquer de sua escolha e 
# apresente um mapa com um ponto azul na localização do endereço.

# Relatórios ----

## Shiny

## R Markdown ---- VER APOSTILA ----



