library(flextable)
library(GGally)
library(ggraph)
library(gutenbergr)
library(igraph)
library(Matrix)
library(network)
library(quanteda)
library(sna)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)
library (tidytext)
library(dplyr)
library(widyr)

example=fashion_greenwashing_examples
example = tips_for_fashion_greenwashing
example = Advocacy_and_Regulation_
tag=example$tags
tag_data=data.frame(doc_id=seq(1:nrow(example)),text=example$tags)
(tag_data <- as_tibble(tag_data) %>%   
    mutate(text = as.character(text)))
#stop_words$word=cbind(stop_words$word,c("nordstrom"),c("macys"),c("bloomies"),c("bloomingdales"),#                      
#c("saks"),c("does't"),c("JCP"),c("Jcppenney"),c("i've"),#                      
#c("don't"),c("rack"),c("i'm"),c("TJX"),c("tj"),c("porter"),c("net"),#                      
#c("macy"),c("jcpenney"),c("penney"),c("kohl"),c("tjmaxx"),#                      
#c("maxx"),c("jcp"),c("jc"),#                      
#c("kohls"))
usenet_words <- tag_data %>% 
  unnest_tokens(word, text) %>%  
  filter(str_detect(word, "[a-z']$"),         
        !word %in% stop_words$word)

usenet_words %>%  
  count(word, sort = TRUE)

title_word_pairs <- usenet_words %>%   
  pairwise_count(word, doc_id, sort = TRUE, upper = FALSE)

title_word_pairs
set.seed(1234)

title_word_pairs %>%  
  filter(n >= 2) %>%
  graph_from_data_frame() %>%  
  ggraph(layout = "fr") +  
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +  
  geom_node_text(aes(label = name), repel = TRUE,                  
                 point.padding = unit(0.2, "lines")) +  
  theme_void()

