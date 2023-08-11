library(quanteda)

# List of CSV files
csv_files <- c("outputnexis.csv","outputsueddeutsche.csv", "output_combined_faz.csv")

# Combine CSV files into one
combined_data <- do.call(rbind, lapply(csv_files, read.csv))

# Create a corpus from the combined CSV data
Lina_corpus <- corpus(combined_data$Body, docnames = combined_data$DocumentNumber)
