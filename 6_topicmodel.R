
options(stringsAsFactors = FALSE)
library(quanteda)
require(quanteda)
require(topicmodels)

stopwordsde<-readLines("stopwords-de.txt", encoding ="UTF-8")
# removed from the removal list were: recht,rechte,rechten,rechter,rechtes,richtig,tat, vielleicht, wahr
#added some verbs


#---------------------------------create tokens
corpus_tokens <- Lina_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower()%>%
  tokens_remove(pattern = stopwordsde, padding = T)#%>%
 # tokens_replace(x$token, x$lemma,
   #             valuetype = "fixed")




#create collocations
Lina_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 25)
Lina_collocations <- Lina_collocations[1:64, ]
corpus_tokens <- tokens_compound(corpus_tokens, Lina_collocations)


# Create DTM, but remove terms which occur in less than 1%
# of all documents
DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.99, docfreq_type = "prop")
# have a look at the number of documents and terms in the
# matrix
dim(DTM)

#--------------------- remove some weird words, which are there because of the Seitenumbrüche etc.
top_terms <- c("Lina E.","lina","bild","sz", "https://www.saechsische.de","pdf-datei","original_gesamtseiten-pdf_graphic","page_of","gesamtseiten-pdf","page","graphic","dokuments","seite","of")
DTM <- DTM[, !(colnames(DTM) %in% top_terms)]
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
combined_data <- combined_data[sel_idx, ]


##------------------------create topic model

K <- 20
# compute the LDA model, inference via n iterations of
# Gibbs sampling
topicModel <- LDA(DTM, K, method = "Gibbs", control = list(iter = 500, seed = 42, verbose = 25))


terms(topicModel, 30)

#-----------------------------select best parameters and evaluate the model:

"TOPIC MODELS
MAIN METHODOLOGICAL QUESTIONS
− How many topics should be found?
− How many words per topic?
− How should the text be preprocessed? (lemmatization, stop words
out, POS tagging → only nouns?)
− And: What to do with topics that don't make sense – criteria?
− topic coherence
− topic interpretability"

"− Objectivity
− If model assumptions of the generative process of text origin
hold true, algorithmic solution guarantees maximum
intersubjectivity
− Validity – Human Judgement
− Model caputures semantic coherence prominent in and relevant
for a text collection properly
− Reliability – Numeric Evaluation
− Repeated runs of model inference with same parameters on the
same data produce same (or at least similar) results"

"− 3 steps proposed by [10]:
1. Check semantic coherence of top N terms of each topic: Can you assign a topic
label?
2. Employ additional numeric measures of topic coherence to identify broad /
incoherent topics
3. Check if topic distribution over time complies with researcher intuition
− 2 methods introduced by [9]:
− Word intrusion
− Topic intrusion
− 1 tool for visual analysis by [4]
− Nearness of topics (using PCA)
− Re-Ranking of topic terms
[10] Evans, M. S. (2014). A Computational Approach to Qualitative Analysis in Large Textual Datasets. PLoS ONE, 9(2), e87908.
https://doi.org/10.1371/journal.pone.0087908
[4] Sievert, C., & Shirley, K. E. (2014). LDAvis: A method for visualizing and interpreting topics. Proceedings of the workshop on interactive
language learning, visualization, and interfaces, 63–70. http://www.aclweb.org/website/old_anthology/W/W14/W14-31.pdf#page=73
[9] Chang, J., Boyd-Graber, J., Gerrish, S., Wang, C., & Blei, D. (2009). Reading tea leaves: How humans interpret topic models. Neural Information
Processing Systems, 1–9"

"MODEL SELECTION AND EVALUATION / NUMERIC EVALUATION
− Goal: Entirely automatic approaches to judge on model quality
− Determine model quality in one numeric measure
− 3 Approaches:
− Perplexity [5]
How well performs generalization of a learned model to unseen data?
− Coherence [6]
How often do we oberve predicted semantic coherence actually in the data?
− Reliability [7,8]
How reproducible are model results between repeated inference runs?
− CAUTION: None of them replaces careful manual inspection!
[5] Wallach, H. M., Murray, I., Salakhutdinov, R., & Mimno, D. (2009). Evaluation methods for topic models. Proceedings of the 26th Annual
International Conference on Machine Learning - ICML ’09, 1–8. https://doi.org/10.1145/1553374.1553515
[6] Mimno, D., Wallach, H. M., Talley, E., Leenders, M., & McCallum, A. (2011). Optimizing Semantic Coherence in Topic Models. Proceedings of the
Conference on Empirical Methods in Natural Language Processing, 262–272.
[7] Lancichinetti, A., Sirer, M. I., Wang, J. X., Acuna, D., Körding, K., & Amaral, L. A. N. (2015). High-Reproducibility and High-Accuracy Method for
Automated Topic Classification. Phys. Rev. X, 5(1), 011007. https://doi.org/10.1103/PhysRevX.5.011007
[8] Koltcov, S., Nikolenko, S. I., Koltsova, O., & Bodrunova, S. (2016). Stable Topic Modeling for Web Science: Granulated LDA. Proceedings of the 8th
ACM Conference on Web Science, 342–343. https://doi.org/10.1145/2908131.2908184"

#----------------------------
"Workflow:
  1. Preprocessing: clean documents/remove boilerplate, lowercase, remove punctuation, remove stop words, remove
infrequent terms (df(w) < 0.5% document frequency), lemmatization/stemming
2. Initialize Topic assignments for LDA
− set seed, or cluster terms by their co-occurrence statistics
3. Compute a variety of models with different parameters K, α, (fix η = 1 / K)
− for each K, select model with α having highest topic coherence
− select model with best interpretable K topics (use LDAvis as helper tool)
4. Validate selected model
− Rank words: term probability + lambda relevance score (LDAvis) → interprete semantic coherence → label
− Rank topics: topic probability + Rank 1 (background vs major topics), coherence (compared to other topics)
− Read N documents for each topic with highest topic probability
− Check reliability to repeated inference runs
5. Final analysis: time series, cross-sectional analysis
− Leave out uninterpretable models
− Leave out unreliable models"

#------------------------------visualization:
