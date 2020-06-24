require(quanteda)
require(readtext)
require(quanteda.textmodels)
require(quanteda.corpora)
require(newsmap)

corp_uk <- corpus(data_char_ukimmig2010)
summary(corp_uk)

docvars(corp_uk, "Party") <- names(data_char_ukimmig2010)
docvars(corp_uk, "Year") <- 2010
summary(corp_uk)

texts(data_corpus_inaugural)[58]

data(data_corpus_irishbudget2010, package = "quanteda.textmodels")
summary(data_corpus_irishbudget2010)

tokeninfo <- summary(data_corpus_inaugural)
tokeninfo$Year <- docvars(data_corpus_inaugural, "Year")
if (require(ggplot2))
  ggplot(data = tokeninfo, aes(x = Year, y = Tokens, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = c(seq(1789, 2017, 12)), breaks = seq(1789, 2017, 12)) +
  theme_bw()


tokeninfo[which.max(tokeninfo$Tokens), ]
# Adding two corpus
corp1 <- corpus(data_corpus_inaugural[1:5])
corp2 <- corpus(data_corpus_inaugural[53:58])
corp3 <- corp1 + corp2
summary(corp3)

summary(corpus_subset(data_corpus_inaugural, Year > 1990 & President == "Obama"))

# Keywords-in-context
kwic(data_corpus_inaugural, pattern = "terror")
kwic(data_corpus_inaugural, pattern = "terror", valuetype = "regex")

kwic(data_corpus_inaugural, pattern = phrase("United States")) %>%
  tail()

# Document-level variables
head(docvars(data_corpus_inaugural))

# Tokenising texts
txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!",
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.")
tokens(txt, remove_numbers = T, remove_punct = T)

## sentence level       
tokens(c("Kurt Vongeut said; only assholes use semi-colons.",
         "Today is Thursday in Canberra:  It is yesterday in London.",
         "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"),
       what = "sentence")

tokens("New York City is located in the United States.", remove_punct = T) %>%
  tokens_compound(pattern = phrase(c("New York City", "United States")))

# Constructing dfm
corp_inaug_post2000 <- corpus_subset(data_corpus_inaugural, Year > 2000)

dfmat_inaug_post2000 <- dfm(corp_inaug_post2000,
                            remove = stopwords("english"),
                            stem = TRUE, remove_punct = TRUE)
head(stopwords("en"), 20)

dfmat_uk <- dfm(data_char_ukimmig2010, remove = stopwords("english"), remove_punct = TRUE)
dfmat_uk

topfeatures(dfmat_uk, 20)

# Wordcloud
set.seed(100)
textplot_wordcloud(dfmat_uk, min_count = 6, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

# Grouping documents
dfmat_ire <- dfm(data_corpus_irishbudget2010, groups = "party",
                 remove = stopwords("english"), remove_punct = TRUE)
dfm_sort(dfmat_ire)[, 1:10]
