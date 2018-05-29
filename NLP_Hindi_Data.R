#Get the working directory
getwd()

#Set the working directory
#Set as per where you have stored the file
setwd("C:/NU/DSP/text")


#Install required packages

#install.packages("udpipe")       #natural language processing toolkit
                                  #provides language-agnostic 'tokenization', 'parts of speech tagging',
                                  #'lemmatization' and 'dependency parsing' of raw text
#install.packages("utf8")         #Process and print 'UTF-8' encoded international text (Unicode)
#install.packages("lattice")      #powerful and elegant high-level data visualization system
#install.packages("wordcloud")    # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#install.packages("Stopwords")    #Stopwords for hindi. We will use the source as 'stopwords-iso'

library(udpipe)
#Downloading the model for hindi. Uncomment if not already downloaded.
#model <- udpipe_download_model(language = "hindi")

#Loading the downloaded model 
udmodel_hindi <- udpipe_load_model(file = 'hindi-ud-2.0-170801.udpipe')


#Defining the path of  the dataset
filepath <- "hindi article.txt"

#Loading the dataset in utf-8 format
hindi <- readLines(filepath, encoding = "UTF-8")

#Cleaning the data

#Full stops in hindi are replaced with '।' symbol. Removing this from the file 
hindi <- gsub("।", ".", hindi)

#Removing punctations and digits from the file
hindi <- gsub('[[:digit:][:punct:]]+', '', hindi)

#list the stopwords
#stopwords-iso contains stopwords for the hindi language
stopwords <- as.list(head(stopwords::stopwords("hi", source = "stopwords-iso"),225)) 


### 1st method for removing stopwords - Faulty method ###

# In this approach we remove the stopwords from the line read from the file.
# Removing the stopwords resulted in unwanted symbols added to the text. 
# Removing the stopwords from the file 

for (sw in stopwords)
  hindi_faulty <- gsub(sw,"", hindi)

# Removing stopwords which have the . over the matra result in creating a symbol 'ं' (unwanted symbol). 
# Hence cleaning the data by removing the symbol.
hindi_faulty <- gsub("ं", ".", hindi)


### 2nd method - using regex to remove stopwords ###

# Making a pattern of all the stopwords present in the stopwords-iso package.
pat <- paste0("\\b(", paste0(as.list(stopwords), collapse="|"), ")\\b")

# Removing the stopwords from the file.
# This method gives similar results to method one.
hindi_without_stopwords <- gsub(pat, "", hindi)

### 3rd Method - Working method ###

# Remove the stopwords after annotating the text.
# In this method we remove the stopword rows from the annotated vector
# This approach doesn't add unwanted symbols on removing the stopwords

# Annotating the model using udpipe package
x <- udpipe_annotate(udmodel_hindi, hindi)

#For testing purpose only. 
#x<- udpipe_annotate(udmodel_hindi, " नई दिल्ली (जेएनएन)।हर किसी का सपना होता है कि उसके पास भी अपनी एक कार हो, फिर चाहे कार छोटी हो या बड़ी, आज इस रिपोर्ट में हम बात करने जा रहे हैं ऐसी कारों के बारे में जिनकी कीमत 25 लाख रुपये से भी कम है, आइये जानते है"

#Storing it in a dataframe
x<- data.frame(x)

# If x$token matches with any of the stopwords from the list we delete that row from the annotated vector
for (sw in stopwords)
  x <- x[x$token != sw, ]

#Determing the most frequent words by sorting them in descending order (Highest number of times the word has occured)
sorted_freq <- sort(table(x$token), decreasing=T)
sorted_freq

#Displaying the top 10 words of highest frequency
head(sorted_freq, 10)

#Aggregating the frequency of each POS tag 
table(x$upos)

#Displaying the POS of each word
x$upos

# Loading the utf8 package to ensure that the encoding doesnt change.
# Sometimes after performing data cleaning the encoding changes
library(utf8)

## To plot Part-of-speech tags from the given text, use package lattice
library(lattice)

#Displaying the parts of speech in the document starting with highest number of POS (most frequent POS)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

#Displaying the nouns in hindi as they are the most frequent
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

##Wordcloud of the most frequent nouns
#This gives us a gist of what the article is about
library(wordcloud)
wordcloud(words = stats$key, freq = stats$freq, min.freq = 2, max.words = 200,
          random.order = FALSE, colors = brewer.pal(6, "Spectral"))


#Displaying the most occurring adjectives
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

## We can conduct a sentiment analysis with verbs used. Do the bring any 
## signs of optimision or infuse pessimism instead?
## Displaying thr most frequent VERBS. This helps in performing sentimental analysis
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")


## RAKE is one of the most popular unsupervised algorithms for extracting 
## keywords in Information retrieval. RAKE is short for Rapid Automatic 
## Keyword Extraction. It is a domain independent algorithm which tries to 
## determine key phrases in a body of text by analyzing the frequency of
## word appearance as well as its co-occurrence with other words in the corpus
## Let's goup nouns and adjectives for a better understanding of the roles of 
## nouns
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")


## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

# Wordcloud with blackground of all the noun/verb phrases
par(bg="black") 
wordcloud(words = stats$key, freq = stats$freq, min.freq = 1, max.words = 200,
          random.order = FALSE, col=terrain.colors(length(stats$key) , alpha=0.9) , rot.per=0.3 )





