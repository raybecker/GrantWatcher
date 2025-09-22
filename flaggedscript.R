require(tm)
library(wordcloud)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(stopwords) # optional
library(tidytext) # for reorder_within() and scale_x_reordered() functions

nsf_terminations_airtable <- read.csv("~/Downloads/NSF-Terminated-Awards.csv", header=TRUE)
# to change dollar amounts to numeric
nsf_terminations_airtable$ObligatedAmt <- as.numeric(gsub("[$,]", "", nsf_terminations_airtable$Obligated))
cleaned <- as.numeric(cleaned)
nsf_terminations_airtable$Recipient <- trimws(nsf_terminations_airtable$Recipient)
x <- sum(subset(nsf_terminations_airtable, Recipient=="Harvard University")$ObligatedAmt)
x
misinformation <- subset(nsf_terminations_airtable, grepl("Misinformation", Title))
                         
nih_terminations_airtable <- read.csv("~/Downloads/nih_terminations_airtable.csv", header=TRUE)
nih_terminations_airtable$datana <- is.na(nih_terminations_airtable[,28])
data_na <- subset(nih_terminations_airtable, datana==TRUE)

# NSF Titles
docs <- iconv(nsf_terminations_airtable$Title, from = "", to = "UTF-8", sub = "byte")
docs <- Corpus(VectorSource(docs))

# Flagged words
docs <- Corpus(VectorSource(nih_terminations_airtable[,28]))
# Abstract text
docs <- Corpus(VectorSource(nih_terminations_airtable[,32]))
docs <- Corpus(VectorSource(data_na[,32]))
docs <-tm_map(docs, removePunctuation)
docs <-tm_map(docs, content_transformer(tolower))
docs <-tm_map(docs, removeNumbers)
docs <-tm_map(docs, function(x)removeWords(x,stopwords()))

##### from frequency counts #####
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
stopwords <- c("will", "also", "using", "use", "can", "new", "aim",
               "one", "first", "well", "two", "may", "work", "goal",
               "provide", "determine", "high", "changes", "aims",
               "associated", "whether", "critical", "associated",
               "summary", "including", "understanding", "proposed",
               "however", "data", "research", "support", "hypothesis",
               "effects", "project", "used", "key", "multiple",
               "factors", "information", "approaches", "important",
               "within", "current", "proposal", "outcomes", "studies",
               "test", "based", "study", "role", "across", "analysis",
               "specific", "identify", "approach", "system", "control",
               "potential", "summaryabstract", "increased", "three", 
               "cause", "abstract", "address", "time", "many", "lead",
               "thus", "increased", "science", "hypothesize", "understand",
               "impact", "years", "evidence", "examine", "loss", "knowledge",
               "complex", "propose", "function", "investigate", "underlying",
               "individuals", "responses", "central", "different", "results",
               "additional", "advanced", "small", "contribute", "grant",
               "limited", "center", "currently", "often", "state", "testing",
               "year", "-", "effect", "questions", "therefore", "characterize",
               "fundamental", "objective", "participants", "severe", "unique",
               "finally", "increase", "public", "shown", "successful", "type",
               "despite", "number", "specifically", "individual", "people",
               "statistical", "following", "implementation", "need", "common",
               "insights", "process", "promote", "essential", "evaluate", 
               "future", "measure", "via", "establish", "functions",
               "identified", "interactions", "several", "structure", "large",
               "innovative", "ability", "growth", "application", "findings",
               "found", "recent", "assess", "care", "known", "leading",
               "related", "focus", "independent", "highly", "significant",
               "together", "trial", "levels", "processing", "overall",
               "among", "effective", "due", "early", "strategies", "design",
               "major", "preliminary", "primary", "advance", "help",
               "second", "components", "help", "dynamics", "enhance",
               "experimental")
d <- subset(d, !(word %in% stopwords))
gowords <- c("collaborative", "stem", "equity", "career", "education",
             "learning", "faculty", "students", "inclusive", "community",
             "participation", "black", "diversity", "student", "undergraduate",
             "minority", "racial", "underrepresented", "women", "equitable",
             "development", "inclusion", "gender", "developing", "teaching",
             "justice", "environmental", "climate", "identity", "access",
             "indigenous", "culture", "diverse", "cultural", "american",
             "culturally", "careers", "latinx", "hispanic", "marginalized")
d <- subset(d, (word %in% gowords))
d <- subset(d, freq >= 20)
ggplot(d, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Word",  y = "Frequency") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(family = "Times", size = rel(0.9), 
        angle = 45, hjust = 1))
genetic <- c("host", "viral", "biomarkers", "circuits", "genomic",
             "targets", "dopamine", "biological", "gene", "signals",
             "mitochondrial", "tissue", "biology", "mutations",
             "protein", "proteins", "cellular", "dna", "vivo",
             "metabolic", "immune", "mouse", "molecular", "mice",
             "signaling", "infection", "genetic", "disease", "cells",
             "cell")
neuroscience <- c("cortex", "neuronal", "pathways", "cortical", 
                  "signaling", "imaging", "neurons", "neural",
                  "brain")
training <- c("trainees", "training", "career", "tools", "program",
              "programming", "develop", "development")

d$category <- ifelse(d$word %in% genetic, 1,
  ifelse(d$word %in% neuroscience, 2,
  ifelse(d$word %in% training, 3, 0)))

d <- d %>%
  mutate(
    category <- case_when(
      word %in% genetic ~ 1,
      word %in% neuroscience ~ 2,
      word %in% training ~ 3,
      TRUE ~ 0
      ),
      cat_label = factor(category,
        levels <- c(0, 1, 2, 3),
        labels <- c("Other", "Genetics", "Neuroscience", "Training"))
  )

df_top5 <- d %>%
  group_by(cat_label) %>%
  # change top number of words here
  slice_max(order_by = freq, n = 20, with_ties = FALSE) %>%
  ungroup()

#  summarise(total_freq = sum(freq), .groups = "drop")

colors <- brewer.pal(n = 3, name = "Set2")

# Top5 plot
ggplot(subset(df_top5, cat_label != "Other"), 
  aes(x = reorder_within(word, freq, cat_label),
  y = freq, fill = cat_label)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_x_reordered() +
  labs(x = "Word",  y = "Frequency") +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(family = "Times", size = rel(0.8), 
          hjust = 1),
        axis.text.x = element_text(family = "Times", size = rel(1.5), 
        angle = 45, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ cat_label, scales = "free_x")
  
# Old plot of just categories
ggplot(subset(d_sum, cat_label != "Other"), aes(x = reorder(cat_label, total_freq),
  y = total_freq, fill = cat_label)) +
  geom_col() +
  scale_fill_manual(values = colors) +
  labs(x = "Category",  y = "Total Frequency", fill = "Category") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(family = "Times", size = rel(1.5), 
                                   angle = 45, hjust = 1))
#A bigger cloud with a minimum frequency of 4
wordcloud(d$word,d$freq,c(8,.3),4)
#Now lets try it with frequent words plotted first
pal <- brewer.pal(9,"Dark2")
pal <- pal[-(1:4)]
wordcloud(d$word,d$freq,c(8,.5),4,,FALSE,,.1, pal)

wordcloud(d$word,d$freq,c(8,.3),4,,FALSE,,.15,pal)

wordcloud(d$word,d$freq,c(3,.05),10,10,TRUE,TRUE,0,pal,,,TRUE,
          vfont=c("gothic english","plain"))