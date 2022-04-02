# Analysis final
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
# libraries
packages = c('plotly', 'shiny', 'LDAvis', 'lda',
             'DT',  'tidyverse', 'stopwords', 'tm', 'topicmodels',
             'readxl', 'kableExtra', 'knitr', 'tidyquant','rmarkdown','tidyr',
             'data.table','XML','xml2','httr','dplyr','knitr', 'tokenizers', 'shinydashboard', 'tidytext')

for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

stopwords_en= stopwords::stopwords("en")
# Reading Data removing irrelevant data columns filter (supplier_name != "Unknown") + lower character columns
df <- read_csv("data/procrumentRaw.csv") %>%
  select(2:4,6:7) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(tender_description = tolower(tender_description)) %>%
  mutate(agency = tolower(agency)) %>%
  mutate(supplier_name = tolower(supplier_name))

# removal of punct and blank
df_backed <- df

df$tender_description <-  gsub('\'s',' ',df$tender_description)
df$tender_description <-  gsub('[[:punct:][:blank:][:digit:]]+',' ',df$tender_description)
stopwords_en= stopwords::stopwords("en")

# removal of redundant words
replace_list <- c(list("maintain","provision", "service", "services","invitation", "tender", 'republic of singapore', 'singapore', 'year' , '\'s', 'period', 'term', 'contract', 'option', 'supplies', 'delivery') ,as.vector(stopwords_en))

replacement_list <- paste0("\\b(", paste0(replace_list, collapse="|"), ")\\b")
df$tender_description <- gsub(replacement_list, "", df$tender_description)

df$tender_description <- gsub('\\s+', ' ', df$tender_description)

# stemmings
library("stringi")
df[["tender_description"]] <-
  sapply(stri_split_fixed(df[["tender_description"]], ","), function(x) {
    x <- lapply(stri_split_fixed(stri_trim_both(x), " "), function(y) {
      paste(SnowballC::wordStem(y), collapse = " ")
    })
    paste(x, collapse = ", ")
  })

write.csv(df, "cleaned_text.csv", row.names = FALSE)

df_ori <- read_csv("data/cleaned_text.csv")
df<- df_ori
# adding a unique doc id
df$doc_id <- 1:nrow(df)

df_topic<- df %>%
  select(doc_id,tender_description,agency)
names(df_topic) <- c('doc_id','text','document')

df_topic$document = paste0(df_topic$document, '_', df_topic$doc_id)

# split into words
df_dtm <- df_topic %>%
  unnest_tokens(word, text)

# find document-word counts
df_dtm <- df_dtm %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE)

df_dtm  <- df_dtm %>%
  cast_dtm(document, word, n)

# CHANGE NUMBER OF TOPICS BY CHANGING K
dflda <- LDA(df_dtm, k = 5, control = list(seed = 1234))

# retrieve topic
# working
#> A LDA_VEM topic model with 4 topics.
df_tidy_lda <- tidy(dflda, matrix = "gamma")

df_doctopic  <- df_tidy_lda %>%
  group_by(document) %>%
  summarize(topic = which.max(c(gamma))) %>%
  separate(document, into = c('Document_Name', 'doc_id'), sep = "_")

df_combined <-  merge(x = df, y = df_doctopic, by = "doc_id", all.y = TRUE)
df_combined <- df_combined[,-7]
write.csv(df_combined, "cleaned_text2.csv", row.names = FALSE)
