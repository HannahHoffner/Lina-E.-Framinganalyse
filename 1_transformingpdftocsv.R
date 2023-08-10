# set options
options(stringsAsFactors = F)          # no automatic data transformation


library(pdftools)
library(stringr)
library(dplyr)

# Load the PDF text (512articles in the pdf)
txt_output <- pdf_text("pdf24_zusammengefügt.pdf") %>%
  paste(collapse = " ") %>%
  str_squish()

# Split into separate documents using "End of Document" marker
documents <- str_split(txt_output, "End of Document")[[1]]

# Initialize a list to store bodies
bodies <- list()

# Iterate through each document
for (i in seq_along(documents)) {
  # Extract body using "Body" marker and up to "Classification Language"
  body <- str_extract(documents[i], "(?<=Body)(.*?)(?=Classification Language|$)")
  
  # Clean up whitespace
  body <- str_squish(body)
  
  # Add body to the list
  bodies <- c(bodies, body)
}

# Convert list to character vector
bodies <- unlist(bodies)

# Create a data frame
data_df <- data.frame(DocumentNumber = seq_along(bodies), Body = bodies)

# Save as CSV
write.csv(data_df, "outputnexis.csv", row.names = FALSE)
