#Trabalho Exploratória
#Natasha Nuto
#Gustavo Amorim
#Eduardo Schneider


if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,readxl,lubridate,janitor,data.table,scales,stringr)


library(data.table)
setwd('E:/Natasha/trabalho exploratória')
basquete <- fread('2022-2023 NBA Player Stats - Playoffs.csv')


#Tema

cores <- c("#B8DFF8", "#FFC4D8", "#97DFC6", "#DDA0DD", "#FBEC5D")

theme <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores),
      scale_colour_manual(values = cores)
    )
  )
}


#Análise 1 - Relação entre "Arremessos de 3 pontos(3P)" e a "Pontuação média por jogo (PTS)" dos jogadores

ggplot(basquete) +
  aes(x = `3P`, y = PTS) + 
  geom_point(colour = "#B8DFF8", size = 3) +
  labs(
    x = "Arremessos de 3 pontos por jogo",
    y = "Pontuação média por jogo"
  ) +
  theme()

ggsave("exploratoria2.png", width = 158, height = 93, units = "mm")

#Quadro de Medidas Resumo - Arremessos de 3 pontos(3P)

quadro_resumo3P <- basquete %>% 
  summarize(Média = round(mean(`3P`),2),
            `Desvio Padrão` = round(sd(`3P`),2),
            `Variância` = round(var(`3P`),2),
            `Mínimo` = round(min(`3P`),2),
            `1º Quartil` = round(quantile(`3P`, probs = .25),2),
            Mediana = round(quantile(`3P`, probs = .5),2),
            `3º Quartil` = round(quantile(`3P`, probs = .75),2),
            `Máximo` = round(max(`3P`),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)

#Média         0,97
#Desvio Padrão 0,99
#Variância     0,99
#Mínimo         0,00
#1º Quartil     0,00
#Mediana        0,70
#3º Quartil     1,70
#Máximo         4,40

summary(basquete$`3P`)
sd(basquete$`3P`)
var(basquete$`3P`)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. Desvio Padrão    VariaÂncia
#0.00  0.00    0.70      0.97  1.70      4.40     0.99            0.99


#Quadro de Medidas Resumo - Pontuação média por jogo (PTS)

quadro_resumoPTS <- basquete %>% 
  summarize(Média = round(mean(PTS),2),
            `Desvio Padrão` = round(sd(PTS),2),
            `Variância` = round(var(PTS),2),
            `Mínimo` = round(min(PTS),2),
            `1º Quartil` = round(quantile(PTS, probs = .25),2),
            Mediana = round(quantile(PTS, probs = .5),2),
            `3º Quartil` = round(quantile(PTS, probs = .75),2),
            `Máximo` = round(max(PTS),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)

#Média          8,70
#Desvio Padrão  8,38
#Variância     70,15
#Mínimo         0,00
#1º Quartil     2,00
#Mediana        6,50
#3º Quartil   12,70
#Máximo       34,50

summary(basquete$PTS)
sd(basquete$PTS)
var(basquete$PTS)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   Desvio Padrão  Variância
#0.000   2.00   6.50   8.70    12.70     34.50       8.38        70.15       

#Coeficente de Correlação de Pearson

correlacao <- cor(basquete$`3P`, basquete$PTS)

#O Coeficente de Correlação de Pearson é igual 0.76144. Apresenta uma correlação diretamente proporcional forte.



#Análise 2 - Frequência de posição por time dos top 5 jogadores com maior pontuação média

#Top 5 jogadores com maior pontuação média e seus respectivos times

analise5 <- basquete %>%
  select(Player, Tm, PTS,Pos) %>%
  arrange(desc(PTS))

head(analise5,5)

#Os 5 melhores jogadores são    
#Player  Tm  PTS Pos
#1:   Kawhi Leonard LAC 34.5  SF
#2:    Devin Booker PHO 33.7  SG
#3: Anthony Edwards MIN 31.6  SG
#4:   Stephen Curry GSW 30.5  PG
#5:    Nikola Joki? DEN 30.0   C

#Os times a serem avaliadas são Los Angeles Clippers(LAC), Phoenix Suns(PHO), Minnesota Timberwolves(MIN), Golden State Warriors(GSW) e Denver Nuggets(DEN).
#As posições possíveis são C: Center (Pivô), PF: Power Forward (Ala-Pivô), SF: Small Forward (Ala), SG: Shooting Guard (Ala-Armador) e PG: Point Guard (Armador)


#Filtrando banco para apenas os times selecionados

times <- analise5 %>%
  filter(Tm %in% c("LAC", "PHO", "MIN", "GSW", "DEN"))

#Tabela de contigência

tabela_contingencia <- table(times$Pos, times$Tm)

#DEN GSW LAC MIN PHO
#C    3   1   2   2   3
#PF   4   3   3   3   2
#PG   3   2   3   2   2
#SF   2   4   3   1   2
#SG   3   3   3   5   5


#Gráfico de colunas bivariado com frequências

posicao_time <- times %>%
  mutate(
    Posição = case_when(
      Pos %>% str_detect("C") ~ "Pivô",
      Pos %>% str_detect("PF") ~ "Ala-Pivô",
      Pos %>% str_detect("SF") ~ "Ala",
      Pos %>% str_detect("SG") ~ "Ala-Armador",
      Pos %>% str_detect("PG") ~ "Armador",
      
    ),
    Time = case_when(
      Tm %>% str_detect("LAC") ~ "LAC",
      Tm %>% str_detect("PHO") ~ "PHO",
      Tm %>% str_detect("MIN") ~ "MIN",
      Tm %>% str_detect("GSW") ~ "GSW",
      Tm %>% str_detect("DEN") ~ "DEN"
    )
  ) %>%
  group_by(Posição, Time) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  group_by(Posição) %>%
  mutate(freq_relativa = freq / sum(freq))

freq_relativa_plot <- posicao_time %>%
  filter(!is.na(freq_relativa)) %>%
  mutate(freq_relativa = round(freq_relativa, 3))  

porcentagens <- str_c(freq_relativa_plot$freq_relativa * 100, "%")

legendas <- str_squish(str_c(freq_relativa_plot$freq, " (", porcentagens, ")"))

ggplot(freq_relativa_plot) +
  aes(
    x = fct_reorder(Time, freq, .desc = TRUE), y = freq_relativa * 100,
    fill = Posição
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    aes(label = scales::percent(freq_relativa, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 1.75
  ) +
  labs(x = "Time", y = "Frequência Relativa") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1, suffix = "%"),
    breaks = seq(0, 30, by = 10),
    limits = c(0, 30)
  ) +
  theme()

ggsave("p10.png", width = 158, height = 93, units = "mm")



#Qui-Quadrado

quiquadrado <- chisq.test(tabela_contingencia)

#O valor do Qui-Quadrado da tebla de contingência acima é 5.764.

#O valor do Coeficiente de Contingência é 0.278.

#O valor do Coeficiente de Contingência Corrigido é 0.311.



#Análise 3 - Variação da Pontuação média por Idade

#A idade míninma no banco é 19 e a máxima é 40. Dividiremos classes de 19-25, 25-30, 30-35 ,35-40.

#Criando nova coluna com as classes determinadas acima

basquete$faixa <- cut(basquete$Age, breaks = c(19, 25, 30, 35, 40), labels = c("19-25", "25-30", "30-35", "35-40"))

#Data Frame para a análise

analise3 <- select(basquete, faixa, PTS)
analise3 <- na.omit(analise3)


#Boxplot Bivariado da Variação da Pontuação média por Idade

ggplot(analise3) +
  aes(x = faixa, y = PTS) +
  geom_boxplot(fill = c("#B8DFF8"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Faixa de idade", y = "Pontuação média") +
  theme()


ggsave("analise3.png", width = 158, height = 93, units = "mm")


#Quadro de Medidas Resumo

quadro_resumo_analise3 <- analise3 %>% 
  group_by(faixa) %>% 
  summarize(Média = round(mean(PTS),2),
            `Desvio Padrão` = round(sd(PTS),2),
            `Variância` = round(var(PTS),2),
            `Mínimo` = round(min(PTS),2),
            `1º Quartil` = round(quantile(PTS, probs = .25),2),
            Mediana = round(quantile(PTS, probs = .5),2),
            `3º Quartil` = round(quantile(PTS, probs = .75),2),
            `Máximo` = round(max(PTS),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)

#V1     V2     V3     V4
#faixa          19-25  25-30  30-35  35-40
#Média           7,60   9.89   9.36   8.72
#Desvio Padrão   7,92   8.02  10.22   7.12
#Variância      62,73  64.31 104.40  50.76
#Mínimo             0      0      0      2
#1º Quartil      1,68   2.90   1.30   4.70
#Mediana         5,10   8.50   6.50   6.35
#3º Quartil      9,93  14.90  17.80  10.00
#Máximo          31,6   33.7   34.5   24.5

#Tabela de quantidade de jogadores em cada faixa de idade

tabela <- table(analise3$faixa)

#19-25 25-30 30-35 35-40 
#88    81    37     8 


#Análise 4 - Pontos marcados por posição

nba <- fread('2022-2023 NBA Player Stats - Playoffs.csv')

unique(nba$Pos)
# "C" = Center "SG" = Shooting Guard  "PF" = Power Foward  
# "PG" = Point Guard "SF"  = Small Foward

tot <- nba$Tm != 'TOT'
nba <- nba[tot,]

# Análise 1: posição com maior média de pontos.
# OBS:AJUSTAR FUNÇÕES, ESCOLHER GRÁFICO E SEGUIR MAIS UMA ANÁLISE

pos_c <- nba$Pos == 'C'
nba_c <- nba[pos_c,]
media_c <- mean(nba_c$PTS)
dp_c <- sd(nba_c$PTS)
cv_c <- (dp_c/media_c)*100
max_c <- max(nba_c$PTS)
summary(nba_sf)
# media de FG dos C 3,0339
# desvio padrão dos C 2,1497
# coeficiente de variação dos C

pos_sg <- nba$Pos == 'SG'
nba_sg <- nba[pos_sg,]
media_sg <- mean(nba_sg$PTS)
dp_sg <- sd(nba_sg$PTS)
cv_sg <- (dp_sg/media_sg)*100
max_sg <- max(nba_sg$PTS)
# média de FG dos SG 3,2456

pos_pf <- nba$Pos == 'PF'
nba_pf <- nba[pos_pf,]
media_pf <- mean(nba_pf$PTS)
dp_pf <- sd(nba_pf$PTS)
cv_pf <- (dp_pf/media_pf)*100
max_pf <- max(nba_pf$PTS)
# média de FG dos PF 3,4442

pos_pg <- nba$Pos == 'PG'
nba_pg <- nba[pos_pg,]
media_pg <- mean(nba_pg$PTS)
dp_pg <- sd(nba_pg$PTS)
cv_pg <- (dp_pg/media_pg)*100
max_pg <- max(nba_pg$PTS)
# média de FG dos PG 3,6924

pos_sf <- nba$Pos == 'SF'
nba_sf <- nba[pos_sf,]
media_sf <- mean(nba_sf$PTS)
dp_sf <- sd(nba_sf$PTS)
cv_sf <- (dp_sf/media_sf)*100
max(nba_sf$PTS)

# média de FG dos SF 3,0825

# Assuming nba is your data frame

ggplot(nba, aes(x = Pos, y = PTS, fill = Pos)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") + 
  scale_fill_manual(values = c("#B8DFF8", "#FFC4D8", "#97DFC6", "#DDA0DD", "#FBEC5D")) +
  labs(x = "Posição", y = "Pontos")

nba_table <- table(nba$Pos, nba$PTS)
nba_stats <- assocstats(nba_table)
nba_stats
#c = 0,765 t = 0,982 C* = 0,779
cont_nba <- (0.765/sqrt(28/29))
cont_nba



#Análise 5 - Frequência de Turnover's por jogo pela posição do jogador

tot <- basquete$Tm != 'TOT'
basquete <- basquete[tot,]

total_turnover <- basquete %>%
  mutate(
    Posicao = case_when(
      str_detect(Pos, "C") ~ "Pivô",
      str_detect(Pos, "PF") ~ "Ala-Pivô",
      str_detect(Pos, "SF") ~ "Ala",
      str_detect(Pos, "SG") ~ "Ala-Armador",
      str_detect(Pos, "PG") ~ "Armador",
      TRUE ~ "Outro"
    )
  ) %>%
  group_by(Posicao) %>%
  summarise(Total_TOV = sum(TOV))

ggplot(total_turnover, aes(x = Posicao, y = Total_TOV , fill = Posicao)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) + 
  labs(fill = "Posição") +
  labs(x = "Turnover's", y = "Posição") +
  theme()

ggsave("r3.png", width = 158, height = 93, units = "mm")


