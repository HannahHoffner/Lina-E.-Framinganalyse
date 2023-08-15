install.packages("LDAvis")
library(LDAvis)
help(createJSON, package = "LDAvis")





# Create an LDAvis data object
ldaVisData <- createJSON(phi =beta,
                         theta = theta,
                         vocab = colnames(DTM),
                         doc.length = rowSums(DTM),
                         term.frequency = colSums(DTM),
                         R = 30, lambda.step = 0.01)


# Create the interactive visualization
serVis(ldaVisData)
