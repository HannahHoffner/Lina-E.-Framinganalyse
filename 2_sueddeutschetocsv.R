library(rvest)
library(stringr)

# Read the HTML document
html_doc_sueddeutsche <- read_html("sueddeutsch_lina.html")

# Split the document by <h1> marker
sections_sueddeutsche <- html_nodes(html_doc_sueddeutsche, "h1")


# Initialize a list to store bodies
bodies_sueddeutsche <- list()


# Loop through each <h1> section
for (section in sections_sueddeutsche) {
  
  # Select all <p> elements within the section
  p_elements <- html_nodes(section, xpath = "./following-sibling::p")
  
  # Extract text from <p> elements
  body_text <- html_text(p_elements) %>%
    str_squish() %>%
    paste(collapse = " ")  # Combine all <p> texts within the section
  
  # Remove HTML tags and elements from the body text
  body_text <- str_replace_all(body_text, "<.*?>", "")
  
  # Add body to the list
  bodies_sueddeutsche <- c(bodies_sueddeutsche, body_text)
  
}
# Convert lists to character vectors

bodies_sueddeutsche <- unlist(bodies_sueddeutsche)

# Create a data frame
#set starting number so it matches the end of the nexis.csv (675)
starting_number <- 675
data_df_sueddeutsche <- data.frame(DocumentNumber = seq(starting_number, length.out = length(bodies_sueddeutsche)), Body = bodies_sueddeutsche)



# Save as CSV
write.csv(data_df_sueddeutsche, "outputsueddeutsche.csv", row.names = FALSE)

