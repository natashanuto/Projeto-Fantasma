if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,readxl,lubridate,janitor,data.table,scales,stringr)


library(data.table)
setwd('E:/Natasha/Projeto-Fantasma/banco')
vendas <- fread("vendas.csv")
view(vendas)
devolucao <- fread("devolução.csv")
view(devolucao)

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
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
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}


#Análise 1


#Data frame para análise 1

analise1 <- select(vendas, `""Data Venda""`, `""Category""`, `""Price""`)
analise1 <- na.omit(analise1)
duplicados1 <- duplicated(analise1)
analise1_sem_duplicados <- analise1[!duplicated(analise1),]
analise1_sem_duplicados$`""Data Venda""` <- analise1_sem_duplicados$`""Data Venda""` <- gsub("\"", "", analise1_sem_duplicados$`""Data Venda""`)

#Lubridate - Trabalhando a coluna "Data Venda"

analise1_sem_duplicados$`""Data Venda""` <- mdy(analise1_sem_duplicados$`""Data Venda""`)
analise1_sem_duplicados$`""Data Venda""` <- month(analise1_sem_duplicados$`""Data Venda""`)

meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
analise1_sem_duplicados$`""Data Venda""` <- meses[analise1_sem_duplicados$`""Data Venda""`]


#Gráfico de linhas faturamento anual por categoria

somaprecos <- analise1_sem_duplicados %>%
  group_by(`""Data Venda""`, `""Category""`) %>%
  summarise(Faturamento = sum(`""Price""`)) %>%
  mutate(Categoria = case_when(
    str_detect(`""Category""`, "\"\"Women's Fashion\"\"") ~ "Moda Feminina",
    str_detect(`""Category""`, "\"\"Men's Fashion\"\"") ~ "Moda Masculina",
    str_detect(`""Category""`, "\"\"Kids' Fashion\"\"") ~ "Moda Infantil",
  ))

somaprecos$`""Data Venda""` <- factor(somaprecos$`""Data Venda""`, levels = meses)
somaprecos <- somaprecos[order(match(somaprecos$`""Data Venda""`, meses)), ]

categoria <- c("Moda Feminina", "Moda Masculina", "Moda Infantil")
somaprecos$Categoria <- factor(somaprecos$Categoria, levels = categoria)

ggplot(somaprecos) +
  aes(x = `""Data Venda""`, y = Faturamento, group = Categoria, color = Categoria) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Categoria", labels = categoria) +
  labs(x = "Mês", y = "Faturamento") +
  theme_estat()

ggsave("analise.pdf", width = 158, height = 93, units = "mm")
  
#Análise 2


#Variação de preço por marca

marcas <- unique(vendas$`""Brand""`)

#As marcas a serem avaliadas são Adidas, H&M, Zara, Gucci e Nike.

#Data frame para Análise 2

analise2 <- select(vendas, `""Brand""`, `""Price""`)
analise2 <- na.omit(analise2)
duplicados2 <- duplicated(analise2)
analise2_sem_duplicados <- analise2[!duplicated(analise2),]

#Adidas

adidas <- analise2_sem_duplicados[analise2_sem_duplicados$`""Brand""`== "\"\"Adidas\"\""]
summary(adidas$`""Price""`)
sd(adidas$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
#10.00   35.00   51.00   50.98   67.00   96.00     20.93


#Gucci

gucci <- analise2_sem_duplicados[analise2_sem_duplicados$`""Brand""`== "\"\"Gucci\"\""]
summary(gucci$`""Price""`)
sd(gucci$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
#10.00   34.50   50.00   50.15   64.50   92.00    20.06


#H&M

hnm <- analise2_sem_duplicados[analise2_sem_duplicados$`""Brand""`== "\"\"H&M\"\""]
summary(hnm$`""Price""`)
sd(hnm$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. Desvio Padrão
#10.00   33.25   49.50   49.76   65.75  100.00   21.29


#Nike

nike <- analise2_sem_duplicados[analise2_sem_duplicados$`""Brand""`== "\"\"Nike\"\""]
summary(nike$`""Price""`)
sd(nike$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. Desvio Padrão
#10.00   32.25   49.50   48.82   65.75   90.00    21.34


#Zara

zara <- analise2_sem_duplicados[analise2_sem_duplicados$`""Brand""`== "\"\"Zara\"\""]
summary(zara$`""Price""`)
sd(zara$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
#10.00   35.50   51.00   51.22   66.50   90.00     20.22


# Quadro Resumo


quadro_resumo <- analise2_sem_duplicados %>% 
  group_by(`""Brand""`) %>% 
  summarize(Média = round(mean(`""Price""`),2),
            `Desvio Padrão` = round(sd(`""Price""`),2),
            `Variância` = round(var(`""Price""`),2),
            `Mínimo` = round(min(`""Price""`),2),
            `1º Quartil` = round(quantile(`""Price""`, probs = .25),2),
            Mediana = round(quantile(`""Price""`, probs = .5),2),
            `3º Quartil` = round(quantile(`""Price""`, probs = .75),2),
            `Máximo` = round(max(`""Price""`),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)

#\hline
#""Brand"" & ""Adidas"" & ""Gucci"" & ""H\&M"" & ""Nike"" & ""Zara"" \\ 
#Média & 50,98 & 50.15 & 49.76 & 48.82 & 51.22 \\ 
#Desvio Padrão & 20,93 & 20.06 & 21.29 & 21.34 & 20.22 \\ 
#Variância & 437,89 & 402.55 & 453.29 & 455.23 & 408.79 \\ 
#Mínimo & 10 & 10 & 10 & 10 & 10 \\ 
#1º Quartil & 35,00 & 34.50 & 33.25 & 32.25 & 35.50 \\ 
#Mediana & 51,0 & 50.0 & 49.5 & 49.5 & 51.0 \\ 
#3º Quartil & 67,00 & 64.50 & 65.75 & 65.75 & 66.50 \\ 
#Máximo &  96 &  92 & 100 &  90 &  90 \\ 

#Gráfico dos preços por marca


ggplot(analise2_sem_duplicados) +
  aes(x = gsub("\"", "", `""Brand""`), y = `""Price""`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marcas", y = "Preço da peça") +
  theme_estat()

ggsave("analise2semduplicatas.pdf", width = 158, height = 93, units = "mm")


#Análise 3

cores <- unique(vendas$`""Color""`)

# As cores a serem analisadas são "Preto","Amarelo","Branco","Azul","Verde" e "Vermelho".

#Data frame para a Análise 3

analise3 <- select(vendas, `""Category""`, `""Color""`)
analise3 <- na.omit(analise3)

# Gráfico da relação entre categorias (apenas feminino e masculino) e cores


colors2 <- analise3 %>%
  mutate(
    Cor = case_when(
      str_detect(`""Color""`, "\"\"Black\"\"") ~ "Preto",
      str_detect(`""Color""`, "\"\"Yellow\"\"") ~ "Amarelo",
      str_detect(`""Color""`, "\"\"White\"\"") ~ "Branco",
      str_detect(`""Color""`, "\"\"Blue\"\"") ~ "Azul",
      str_detect(`""Color""`, "\"\"Green\"\"") ~ "Verde",
      str_detect(`""Color""`, "\"\"Red\"\"") ~ "Vermelho"
    ),
    Categoria = case_when(
      str_detect(`""Category""`, "\"\"Women's Fashion\"\"") ~ "Moda Feminina",
      str_detect(`""Category""`, "\"\"Men's Fashion\"\"") ~ "Moda Masculina",
      TRUE ~ as.character(`""Category""`)
    )
  ) %>%
  filter(`""Category""` != "\"\"Kids' Fashion\"\"") %>%
  filter(!is.na(Cor)) %>%
  group_by(Cor, Categoria) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  group_by(Cor) %>%
  mutate(freq_relativa = freq / sum(freq))


freq_relativa_p <- colors2 %>% filter(!is.na(freq_relativa))

ggplot(freq_relativa_p) +
  aes(
    x = fct_reorder(Cor, freq, .desc = TRUE), y = freq_relativa * 100,
    fill = Categoria
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    aes(label = scales::percent(freq_relativa, accuracy = 0.01)),
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 2
  ) +
  labs(x = "Cores", y = "Frequência Relativa") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1, suffix = "%")) +
  theme_estat()

ggsave("análise3c.pdf", width = 158, height = 93, units = "mm")



#Análise 4


#Data frame para Análise 4

analise4 <- select(vendas, `""Rating""`, `""Price""`)
analise4 <- na.omit(analise4)
analise4$`""Rating""` <- as.numeric(analise4$`""Rating""`)
analise4$`""Rating""` <- round(analise4$`""Rating""`, 1)


#Gráfico de Dispersão Bivariado para ilustrar "Relação entre preço e avaliação"


ggplot(analise4_sem_duplicados) +
  aes(x =`""Price""`, y = `""Rating""`) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Preço por peça",
    y = "Avaliação por peça"
  ) +
  theme_estat()

ggsave("análise4semduplicatas.pdf", width = 158, height = 93, units = "mm")

#Quadro resmuo Preço 

quadro_resumo <- analise4_sem_duplicados %>% 
  summarize(Média = round(mean(`""Price""`),2),
            `Desvio Padrão` = round(sd(`""Price""`),2),
            `Variância` = round(var(`""Price""`),2),
            `Mínimo` = round(min(`""Price""`),2),
            `1º Quartil` = round(quantile(`""Price""`, probs = .25),2),
            Mediana = round(quantile(`""Price""`, probs = .5),2),
            `3º Quartil` = round(quantile(`""Price""`, probs = .75),2),
            `Máximo` = round(max(`""Price""`),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)

#Média & 50,51 \\ 
#Desvio Padrão & 19,27 \\ 
#Mínimo & 10 \\ 
#1º Quartil & 36 \\ 
#Mediana & 50 \\ 
#3º Quartil & 64,5 \\ 
#Máximo & 100 \\ 

summary(analise4_sem_duplicados$`""Price""`)
sd(analise4_sem_duplicados$`""Price""`)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão 
#10.00   36.00   50.00   50.51   64.50  100.00    19.27

#Quadro resumo Avaliação

quadro_resumo <- analise4_sem_duplicados %>% 
  summarize(Média = round(mean(`""Rating""`),2),
            `Desvio Padrão` = round(sd(`""Rating""`),2),
            `Variância` = round(var(`""Rating""`),2),
            `Mínimo` = round(min(`""Rating""`),2),
            `1º Quartil` = round(quantile(`""Rating""`, probs = .25),2),
            Mediana = round(quantile(`""Rating""`, probs = .5),2),
            `3º Quartil` = round(quantile(`""Rating""`, probs = .75),2),
            `Máximo` = round(max(`""Rating""`),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)

#Média & 2,52 \\ 
#Desvio Padrão & 0,59 \\ 
#Mínimo & 1,1 \\ 
#1º Quartil & 2,1 \\ 
#Mediana & 2,5 \\ 
#3º Quartil & 3 \\ 
#Máximo & 4,2 \\

summary(analise4_sem_duplicados$`""Rating""`)
sd(analise4_sem_duplicados$`""Rating""`)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão 
#1.10   2.10   2.50     2.52   3.00     4.20     0.59

#Calculando o Coeficiente de Correlação de Pearson

coeficiente_pearson <- cor(analise4_sem_duplicados$`""Rating""`, analise4_sem_duplicados$`""Price""`)

#O Coeficiente de Correlação de Pearson é de 0.9074029. Apresenta uma correlação diretamente proporcional forte.



#Análise 5

devolucoes <- unique(vendas$`""Motivo devolução"""`)

#Os tipos de devolução são "Não informado", "Arrependimento" ou "Produto com defeito"
#As marcas do banco de dados são Adidas, H&M, Zara, Gucci e Nike.

#Data frame para Análise 5

analise5 <- select(vendas, `""Motivo devolução"""`, `""Brand""`,`""...1.y""`)
analise5 <- na.omit(analise5)
duplicados5 <- duplicated(analise5)
analise5_sem_duplicados <- analise5[!duplicated(analise5),]


#Devoluções com motivo "Produto com defeito"

produto_com_defeito <- analise5_sem_duplicados[analise5_sem_duplicados$`""Motivo devolução"""` == "\"\"Produto com defeito\"\"\"",]
frequencia_absoluta_pcd <- table(produto_com_defeito$`""Brand""`)

#Frequências absolutas são: 
#Adidas  Gucci    H&M   Nike   Zara 
#   25     19     27     25     19 

frequencia_relativa_pcd <- (frequencia_absoluta_pcd / sum(frequencia_absoluta_pcd)) * 100
frequencia_porcentagem_arredondada_pcd <- round(frequencia_relativa_pcd, 2)

#Frequências relativas em porcentagem são:
#Adidas  Gucci    H&M   Nike   Zara 
# 21.74  16.52   23.48  21.74  16.52



#Devoluções com motivo "Não informado"

nao_informado <- analise5_sem_duplicados[analise5_sem_duplicados$`""Motivo devolução"""`== "\"\"Não informado\"\"\"", ]
frequencia_absoluta_ni <- table(nao_informado$`""Brand""`)

#Frequências absolutas são:
#Adidas  Gucci    H&M   Nike   Zara 
#   25     22     20     24     20 

frequencia_relativa_ni <- (frequencia_absoluta_ni / sum(frequencia_absoluta_ni)) * 100
frequencia_porcentagem_arredondada_ni <- round(frequencia_relativa_ni, 2)

#Frequências relativas em porcentagem são:
#Adidas  Gucci    H&M   Nike   Zara 
# 22.52  19.82  18.02  21.62  18.02 



#Devoluções com motivo "Arrependimento"

arrependimento <- analise5_sem_duplicados[analise5_sem_duplicados$`""Motivo devolução"""` == "\"\"Arrependimento\"\"\"", ]
frequencia_absoluta_ar <- table(arrependimento$`""Brand""`)

#Frequências absolutas são:
#Adidas  Gucci    H&M   Nike   Zara 
#   18     21     17     34     31 

frequencia_relativa_ar <- (frequencia_absoluta_ar / sum(frequencia_absoluta_ar)) * 100
frequencia_porcentagem_arredondada_ar <- round(frequencia_relativa_ar, 2)

#Frequências relativas em porcentagem são:
#Adidas  Gucci    H&M   Nike   Zara 
# 14.88  17.36  14.05  28.10  25.62 




#Gráfico para ilustrar a tabela


devolucao_marcas <- analise5_sem_duplicados %>%
  mutate(
    Devolucao = case_when(
      `""Motivo devolução"""` %>% str_detect("\"\"Arrependimento\"\"\"") ~ "Arrependimento",
      `""Motivo devolução"""` %>% str_detect("\"\"Produto com defeito\"\"\"") ~ "Produto com defeito",
      `""Motivo devolução"""` %>% str_detect("\"\"Não informado\"\"\"") ~ "Não informado"
    ),
    Marca = case_when(
      `""Brand""` %>% str_detect("\"\"Adidas\"\"") ~ "Adidas",
      `""Brand""` %>% str_detect("\"\"Gucci\"\"") ~ "Gucci",
      `""Brand""` %>% str_detect("\"\"H&M\"\"") ~ "H&M",
      `""Brand""` %>% str_detect("\"\"Nike\"\"") ~ "Nike",
      `""Brand""` %>% str_detect("\"\"Zara\"\"") ~ "Zara"
    )
  ) %>%
  group_by(Devolucao, Marca) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  group_by(Devolucao) %>%
  mutate(freq_relativa = freq / sum(freq))

freq_relativa_plot <- devolucao_marcas %>% filter(!is.na(freq_relativa))

ggplot(freq_relativa_plot) +
  aes(
    x = fct_reorder(Marca, freq, .desc = TRUE), y = freq_relativa * 100,
    fill = Devolucao
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    aes(label = scales::percent(freq_relativa, accuracy = 0.01)),
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 2
  ) +
  labs(x = "Marca", y = "Frequência Relativa") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1, suffix = "%")) +
  theme_estat()

ggsave("analise5semduplicad.pdf", width = 158, height = 93, units = "mm")


#Análise 6

#Data frame para análise 6

analise6 <- select(vendas, `""Rating""`, `""Brand""`)
analise6 <- na.omit(analise6)
duplicados6 <- duplicated(analise6)
analise6_sem_duplicados <- analise6[!duplicated(analise6),]


# Gráfico para representação de avaliação média por marca

ggplot(analise6_sem_duplicados) +
  aes(x = gsub("\"", "", `""Brand""`), y = `""Rating""`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Avaliação") +
  theme_estat()
ggsave("analise6.pdf", width = 158, height = 93, units = "mm")


#Medidas resumos de avaliação por marca

#Adidas

adidas6 <- analise6_sem_duplicados[analise6_sem_duplicados$`""Brand""`== "\"\"Adidas\"\""]
summary(adidas6$`""Rating""`)
sd(adidas6$`""Rating""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
# 1.21   2.20    2.54     2.54  2.91     3.94     0.50


#Gucci

gucci6 <- analise6_sem_duplicados[analise6_sem_duplicados$`""Brand""`== "\"\"Gucci\"\""]
summary(gucci6$`""Rating""`)
sd(gucci6$`""Rating""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
# 1.17   2.17   2.53     2.52   2.83     3.69      0.48 


#H&M

hnm6 <- analise6_sem_duplicados[analise6_sem_duplicados$`""Brand""`== "\"\"H&M\"\""]
summary(hnm6$`""Rating""`)
sd(hnm6$`""Rating""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. Desvio Padrão
# 1.24   2.16   2.47     2.51   2.82     4.18    0.49 


#Nike

nike6 <- analise6_sem_duplicados[analise6_sem_duplicados$`""Brand""`== "\"\"Nike\"\""]
summary(nike6$`""Rating""`)
sd(nike6$`""Rating""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. Desvio Padrão
# 1.06   2.18    2.54      2.49   2.81    3.65   0.50 


#Zara

zara6 <- analise6_sem_duplicados[analise6_sem_duplicados$`""Brand""`== "\"\"Zara\"\""]
summary(zara6$`""Rating""`)
sd(zara6$`""Rating""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
# 1.06   2.23   2.56     2.54   2.88     3.95      0.48 


# Quadro Resumo


quadro_resumo <- analise6_sem_duplicados %>% 
  group_by(`""Brand""`) %>% 
  summarize(Média = round(mean(`""Rating""`),2),
            `Desvio Padrão` = round(sd(`""Rating""`),2),
            `Variância` = round(var(`""Rating""`),2),
            `Mínimo` = round(min(`""Rating""`),2),
            `1º Quartil` = round(quantile(`""Rating""`, probs = .25),2),
            Mediana = round(quantile(`""Rating""`, probs = .5),2),
            `3º Quartil` = round(quantile(`""Rating""`, probs = .75),2),
            `Máximo` = round(max(`""Rating""`),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)

#""Brand"" & ""Adidas"" & ""Gucci"" & ""H\&M"" & ""Nike"" & ""Zara"" \\ 
#Média & 2,54 & 2.52 & 2.51 & 2.49 & 2.54 \\ 
#Desvio Padrão & 0,50 & 0.48 & 0.49 & 0.50 & 0.48 \\ 
#Variância & 0,25 & 0.23 & 0.24 & 0.25 & 0.23 \\ 
#Mínimo & 1,21 & 1.17 & 1.24 & 1.06 & 1.06 \\ 
#1º Quartil & 2,20 & 2.17 & 2.16 & 2.18 & 2.23 \\ 
#Mediana & 2,54 & 2.53 & 2.47 & 2.54 & 2.56 \\ 
#3º Quartil & 2,91 & 2.83 & 2.82 & 2.81 & 2.88 \\ 
#Máximo & 3,94 & 3.69 & 4.18 & 3.65 & 3.95 \\ 