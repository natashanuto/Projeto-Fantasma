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



#Análise 2


#Variação de preço por marca
 

marcas <- unique(vendas$`""Brand""`)
marcas

#As marcas a serem avaliadas são Adidas, H&M, Zara, Gucci, Nike.


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


vendas <- na.omit(vendas)

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


