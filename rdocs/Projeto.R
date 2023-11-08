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

#Organizando o banco de dados

vendas1 <- na.omit(vendas1)
duplicados <- duplicated(vendas1)

vendas1$X..Category.. <- ifelse(vendas1$X..Category.. == "Women's Fashion", "Moda Feminina", vendas1$X..Category..)
vendas1$X..Category..<- ifelse(vendas1$X..Category.. == "Men's Fashion", "Moda Masculina", vendas1$X..Category..)
vendas1$X..Category.. <- ifelse(vendas1$X..Category..== "Kids' Fashion", "Moda Infantil", vendas1$X..Category..)

vendas1$X..Price.. <- as.numeric(vendas1$X..Price..)

faturamento_anual_total <- sum(vendas1$X..Price..)

#O faturamento anual total é de 17.734

#Faturamento anual de Moda Feminina

moda_feminina <- vendas1[vendas1$X..Category..== "Moda Feminina",]
faturamento_moda_feminina <- sum(moda_feminina$X..Price..)

#O faturamento anual da Moda Feminina é de 6.027

freq_moda_feminina <- (faturamento_moda_feminina) / (faturamento_anual_total)

#A frequência relativa do faturalmento anual da Moda Feminina é 34,00%


#Faturamento anual de Moda Masculina

moda_masculina <- vendas1[vendas1$X..Category..== "Moda Masculina",]
faturamento_moda_masculina <- sum(moda_masculina$X..Price..)

#O faturamento anual da Moda Masculina é de 5.487

freq_moda_masculina <- (faturamento_moda_masculina) / (faturamento_anual_total)

#A frequência relativa do faturalmento anual da Moda Masculina é 30,94%


#Faturamento anual de Moda Infantil

moda_infantil <- vendas1[vendas1$X..Category..== "Moda Infantil",]
faturamento_moda_infantil <- sum(moda_infantil$X..Price..)

#O faturamento anual da Moda Infantil é de 6.220

freq_moda_infantil<- (faturamento_moda_infantil) / (faturamento_anual_total)

#A frequência relativa do faturalmento anual da Moda Infantil é 35,07%


#Gráfico de Setor para ilustrar "Faturamento anual por categoria"



#Análise 2


#Variação de preço por marca
 

marcas <- unique(vendas$`""Brand""`)
marcas

#As marcas a serem avaliadas são Adidas, H&M, Zara, Gucci e Nike.


#Adidas

vendas1$X..Price.. <- as.numeric(vendas1$X..Price..)
adidas <- vendas[vendas$`""Brand""`== "\"\"Adidas\"\""]
summary(adidas$`""Price""`)
sd(adidas$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
#10.00   43.25   52.00   51.80   62.00   89.00     16.46


#Gucci

vendas1$X..Price.. <- as.numeric(vendas1$X..Price..)
gucci <- vendas[vendas$`""Brand""`== "\"\"Gucci\"\""]
summary(gucci$`""Price""`)
sd(gucci$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
#10.00   34.00   45.00   45.52   56.00   92.00    16.38


#H&M

vendas1$X..Price.. <- as.numeric(vendas1$X..Price..)
hnm <- vendas[vendas$`""Brand""`== "\"\"H&M\"\""]
summary(hnm$`""Price""`)
sd(hnm$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. Desvio Padrão
#25.00   36.00   47.00   48.21   55.00  100.00   16.18


#Nike

vendas1$X..Price.. <- as.numeric(vendas1$X..Price..)
nike <- vendas[vendas$`""Brand""`== "\"\"Nike\"\""]
summary(nike$`""Price""`)
sd(nike$`""Price""`)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. Desvio Padrão
#10.00   43.00   55.00   53.54   65.00   90.00    16.90


#Zara

vendas1$X..Price.. <- as.numeric(vendas1$X..Price..)
zara <- vendas[vendas$`""Brand""`== "\"\"Zara\"\""]
summary(zara$`""Price""`)
sd(zara$`""Price""`)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Desvio Padrão
#10.00   44.25   51.00   52.27   61.00   85.00     15.36


# Quadro Resumo

vendas1$X..Price.. <- as.numeric(vendas1$X..Price..) #Transformar para númerico

quadro_resumo <- vendas1 %>% 
  group_by(X..Brand..) %>% 
  summarize(Média = round(mean(X..Price..),2),
            `Desvio Padrão` = round(sd(X..Price..),2),
            `Variância` = round(var(X..Price..),2),
            `Mínimo` = round(min(X..Price..),2),
            `1º Quartil` = round(quantile(X..Price.., probs = .25),2),
            Mediana = round(quantile(X..Price.., probs = .5),2),
            `3º Quartil` = round(quantile(X..Price.., probs = .75),2),
            `Máximo` = round(max(X..Price..),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)

#X..Brand.. & Adidas & Gucci & H\&M & Nike & Zara \\ 
#Média & 51,80 & 45.52 & 48.21 & 53.54 & 52.27 \\ 
#Desvio Padrão & 16,46 & 16.38 & 16.18 & 16.90 & 15.36 \\ 
#Variância & 270,88 & 268.16 & 261.80 & 285.47 & 235.96 \\ 
#Mínimo & 10 & 10 & 25 & 10 & 10 \\ 
#1º Quartil & 43,25 & 34.00 & 36.00 & 43.00 & 44.25 \\ 
#Mediana & 52 & 45 & 47 & 55 & 51 \\ 
#3º Quartil & 62 & 56 & 55 & 65 & 61 \\ 
#Máximo &  89 &  92 & 100 &  90 &  85 \\


#Gráfico dos preços por marca


vendas <- na.omit(vendas1)

ggplot(vendas) +
  aes(x = gsub("\"", "", `""Brand""`), y = `""Price""`) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marcas", y = "Preço da peça") +
  theme_estat()

ggsave("preco_marcas.pdf", width = 158, height = 93, units = "mm")


#Análise 3

vendas1 <- as.data.frame(lapply(vendas, function(col) str_replace_all(col, '"', '')))

cores <- unique(vendas$`""Color""`)
cores
# As cores a serem analisadas são "Preto","Amarelo","Branco","Azul","Verde" e "Vermelho".


# Gráfico da relação entre categorias (apenas feminino e masculino) e cores

colors2 <- vendas1 %>%
  mutate(Color = case_when(
    str_detect(X..Color.., "Black") ~ "Preto",
    str_detect(X..Color.., "Yellow") ~ "Amarelo",
    str_detect(X..Color.., "White") ~ "Branco",
    str_detect(X..Color.., "Blue") ~ "Azul",
    str_detect(X..Color.., "Green") ~ "Verde",
    str_detect(X..Color.., "Red") ~ "Vermelho"
  ),
  Categoria = case_when(
    str_detect(X..Category.., "Women's Fashion") ~ "Moda Feminina",
    str_detect(X..Category.., "Men's Fashion") ~ "Moda Masculina",
    TRUE ~ as.character(X..Category..)  
  )) %>%
  filter(X..Category.. != "Kids' Fashion") %>%
  filter(!is.na(Color)) %>%
  group_by(Color, Categoria) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = scales::percent(freq / sum(freq)))


porcentagens <- str_c(colors2$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(colors2$freq, " (", porcentagens, ")"))

ggplot(colors2) +
  aes(
    x = fct_reorder(Color, freq, .desc = T), y = freq,
    fill = Categoria, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.45,
    size = 1.58
  ) +
  labs(x = "Cores", y = "Frequência") +
  theme_estat()
ggsave("análise3.pdf", width = 158, height = 93, units = "mm")


vendas <- na.omit(vendas)

#Análise 4

#Gráfico de Dispersão Bivariado para ilustrar "Relação entre preço e avaliação"

vendas1 <- na.omit(vendas1)

duplicados <- duplicated(vendas1)
duplicados

vendas1$X..Rating.. <- as.numeric(vendas1$X..Rating..)
vendas1$X..Rating.. <- round(vendas1$X..Rating.., 1)

ggplot(vendas1) +
aes(x = X..Price.., y = X..Rating..) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Preço por peça",
    y = "Avaliação por peça"
  ) +
  theme_estat()

setwd('E:/Natasha/Projeto-Fantasma/resultados')
ggsave("análise4.pdf", width = 158, height = 93, units = "mm")


#Calculando o Coeficiente de Correlação de Pearson

coeficiente_pearson <- cor(vendas1$X..Price.., vendas1$X..Rating..)
coeficiente_pearson

#O Coeficiente de Correlação de Pearson é de 0.9075942. Apresenta uma correlação diretamente proporcional forte.



#Análise 5

devolucoes <- unique(vendas1$X..Motivo.devolução...)
devolucoes

#Os tipos de devolução são "Não informado", "Arrependimento" ou "Produto com defeito"
#As marcas do banco de dados são Adidas, H&M, Zara, Gucci e Nike.


#Devoluções com motivo "Produto com defeito"

produto_com_defeito <- vendas1 [vendas1$X..Motivo.devolução... == "Produto com defeito",] 
frequencia_absoluta_pcd <- table(produto_com_defeito$X..Brand..)

#Frequências absolutas são: 
#Adidas  Gucci    H&M   Nike   Zara 
#   26     17     24     27     18 

frequencia_relativa_pcd <- (frequencia_absoluta_pcd / sum(frequencia_absoluta_pcd)) * 100
frequencia_porcentagem_arredondada_pcd <- round(frequencia_relativa_pcd, 2)

#Frequências relativas em porcentagem são:
#Adidas  Gucci    H&M   Nike   Zara 
# 23.21  15.18  21.43  24.11  16.07


#Devoluções com motivo "Arrependimento"

arrependimento <- vendas1[vendas1$X..Motivo.devolução... == "Arrependimento", ]
frequencia_absoluta_ar <- table(arrependimento$X..Brand..)

#Frequências absolutas são:
#Adidas  Gucci    H&M   Nike   Zara 
#   20     23     19     35     30 

frequencia_relativa_ar <- (frequencia_absoluta_ar / sum(frequencia_absoluta_ar)) * 100
frequencia_porcentagem_arredondada_ar <- round(frequencia_relativa_ar, 2)

#Frequências relativas em porcentagem são:
#Adidas  Gucci    H&M   Nike   Zara 
# 15.75  18.11  14.96  27.56  23.62 


#Devoluções com motivo "Não informado"

nao_informado <- vendas1[vendas1$X..Motivo.devolução... == "Não informado", ]
frequencia_absoluta_ni <- table(nao_informado$X..Brand..)

#Frequências absolutas são:
#Adidas  Gucci    H&M   Nike   Zara 
#   28     25     18     23     18 

frequencia_relativa_ni <- (frequencia_absoluta_ni / sum(frequencia_absoluta_ni)) * 100
frequencia_porcentagem_arredondada_ni <- round(frequencia_relativa_ni, 2)

#Frequências relativas em porcentagem são:
#Adidas  Gucci    H&M   Nike   Zara 
# 25.00  22.32  16.07  20.54  16.07 



#Gráfico para ilustrar a tabela
  

devolucao_marcas <- vendas1 %>%
  mutate(
    Devolução = case_when(
      X..Motivo.devolução... %>% str_detect("Arrependimento") ~ "Arrependimento",
      X..Motivo.devolução... %>% str_detect("Produto com defeito") ~ "Produto com defeito",
      X..Motivo.devolução... %>% str_detect("Não informado") ~ "Não informado"
    ),
    Marca = case_when(
      X..Brand.. %>% str_detect("Adidas") ~ "Adidas",
      X..Brand.. %>% str_detect("Gucci") ~ "Gucci",
      X..Brand.. %>% str_detect("H&M") ~ "H&M",
      X..Brand.. %>% str_detect("Nike") ~ "Nike",
      X..Brand.. %>% str_detect("Zara") ~ "Zara"
    )
  ) %>%
  group_by(Devolução, Marca) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  group_by(Devolução) %>%
  mutate(freq_relativa = freq / sum(freq))

ggplot(devolucao_marcas) +
  aes(
    x = fct_reorder(Devolução, freq, .desc = TRUE), y = freq,
    fill = Marca
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    aes(label = paste0(freq, " (", scales::percent(freq_relativa), ")")),
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 1.5
  ) +
  labs(x = "Tipo de Devolução", y = "Frequência") +
  theme_estat()

ggsave("analise5.pdf", width = 158, height = 93, units = "mm")
  