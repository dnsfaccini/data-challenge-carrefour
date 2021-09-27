##Carregando pacotes

library(dplyr)
library(rtweet)
library(tm)
library(wordcloud)
library(syuzhet)

# Buscando Tweets com função search_tweets() do pacote rtweet

carrefour_tweets <- search_tweets(
  "#carrefour",
  include_rts = FALSE
)

save(carrefour_tweets, file = "Carrefour_Tweets.RData")

# Quantidade de Tweets

nrow(carrefour_tweets)

#Menor data

min(carrefour_tweets$created_at)

#Maior data

max(carrefour_tweets$created_at)

# Visualizando a série temporal de frequência dos tweets no decorrer do tempo usando a função ts_plot()

ts_plot(carrefour_tweets, "hour") +
  theme_minimal() +
  theme(plot.title = ggplot2::element_text (face = "bold")) +
  labs (
    x = NULL, y = NULL,
    title = "Frequência do uso da hashtag #vacina nos ultimos 9 dias",
    subtitle = "Contagem de tweets agrupados em intervalos de dia",
    caption = "\nFonte: Dados coletados do Twitter"
  )

# Separando apenas a coluna de Tweets do DataFrame obtido pelo rtweet

carrefour_text <- carrefour_tweets$text

# Para fazer a limpeza dos textos podemos utilizar as funções do pacote tm, ou podemos criar as nossas próprias funções

# Função para limpeza dos textos
limpar_texto <- function(texto) {
  # Convertendo o texto para minúsculo
  texto <- tolower(texto)
  # Removendo o usuário adicionado no comentário
  texto <- gsub("@\\w+", "", texto)
  # Removendo as pontuações
  texto <- gsub("[[:punct:]]", "", texto)
  # Removendo links
  texto <- gsub("http\\w+", "", texto)
  # Removendo tabs 
  texto <- gsub("[ |\t]{2,}", "", texto)
  # Removendo espaços no início do texto
  texto <- gsub("^ ", "", texto)
  # Removendo espaços no final do texto
  texto <- gsub(" $", "", texto)
  return(texto)
}

# Executando a função de limpeza de dados

carrefour_text <- limpar_texto(carrefour_text)

# Convertendo os textos em corpus.

carrefour_corpus <- VCorpus(VectorSource(carrefour_text))

# Removendo Stopwords.

carrefour_corpus %>% tm_map(removeWords, stopwords("portuguese"))

#Através de uma Wordcloud podemos visualizar os termos mais frequentes no conjunto de dados

wordcloud(
  carrefour_corpus,
  min.freq = 5,
  max.words = 30,
  random.order = F,
  colors = brewer.pal(8, "Dark2")
)

#Agora transformaremos o corpus em uma matriz de documentos-termos para criarmos um gráfico de barras com os termos e sua frequência.


# Transformando o corpus em matriz de documentos-termos
carrefour_doc <-  DocumentTermMatrix(carrefour_corpus)

# Removendo os termos menos frequentes
carrefour_doc1 <- removeSparseTerms(carrefour_doc, 0.97)

# Gerando uma matrix ordenada, com o termos mais frequentes
carrefour_freq <- 
  carrefour_doc1 %>% 
  as.matrix() %>% 
  colSums() %>% 
  sort(decreasing = T)

# Criando um dataframe com as palavras mais frequentes
df_carrefour_freq <- data.frame(
  word = names(carrefour_freq),
  freq = carrefour_freq)

# Gerando um gráfico da frequência

df_carrefour_freq %>%
  filter(!word %in% c("carrefour")) %>% 
  subset(freq > 10) %>%
  ggplot(aes(x = reorder(word, freq),y = freq)) +
  geom_bar(stat = "identity", fill='#0c6cad', color="#075284") +
  theme(axis.text.x = element_text(angle = 45, hjus = 1)) +
  ggtitle("Termos relacionados ao Carrefour mais frequentes no Twitter") +
  labs(y = "Frequência", x = "Termos") +
  coord_flip()

#Realizando a análise de sentimentos dos tweets.

# Obtendo os emoções
carrefour_sentimento <- get_nrc_sentiment(
  carrefour_doc$dimnames$Terms,
  #language = "portuguese"
)

# Calculando a frequência dos sentimentos
carrefour_sentimento_freq <-carrefour_sentimento %>%
  colSums() %>% 
  sort(decreasing = T)

# Criando um dataframe com os sentimentos traduzidos, que será utilizado como conversão de domínio. 
sentimetos_traducao <- 
  data.frame(
    sentiment = c(
      "positive",
      "negative",
      "trust",
      "anticipation",
      "fear",
      "joy",
      "sadness",
      "surprise",
      "anger",
      "disgust"
    ),
    sentimentos = c(
      "Positivo",
      "Negativo",
      "Confiança",
      "Expectativa",
      "Medo",
      "Alegria",
      "Tristeza",
      "Surpresa",
      "Raiva",
      "Nojo"
    )
  )

# Tranformando os resultados da frequência em um dataframe e juntando ao dataframe de tradução
df_sentimento <- 
  data.frame(
    sentiment = names(carrefour_sentimento_freq),
    freq = carrefour_sentimento_freq
  ) %>% 
  left_join(sentimetos_traducao, by = "sentiment") %>% 
  dplyr::select(-sentiment) %>% 
  arrange(desc(freq))

##Visualizando a frequência dos sentimentos em relação a #vacina

ggplot(data = df_sentimento,
       aes(x = reorder(sentimentos, -freq), y = freq)) +
  geom_bar(aes(fill=sentimentos), stat = "identity") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjus = 1)) +
  xlab("Sentimentos") +
  ylab("Frequência")
