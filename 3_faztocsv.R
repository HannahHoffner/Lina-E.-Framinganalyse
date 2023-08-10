library(rvest)
library(stringr)

# List of HTML files of the faz
html_files <- c("fazTeil1.html", "fazTeil2.html", "fazTeil3.html")

# Initialize a list to store combined data frames
all_data <- list()

# Initialize a counter for the document numbers to be continious to the other csv's (sueddeutsche ends at 56)(nexis data have 674)
doc_counter <- 731

for (html_file in html_files) {
  # Read the HTML document
  html_doc <- read_html(html_file)
  
  # Split the document by <pre class="docTitle"> marker
  sections <- html_nodes(html_doc, "pre.docTitle")
  
  # Initialize a list to store bodies
  bodies <- list()
  
  # Loop through each section
  for (section in sections) {
    # Find the following sibling <pre class="text"> element relative to the current section
    body_element <- section %>%
      html_nodes(xpath = './following-sibling::pre[contains(@class, "text")][1]')
    
    if (length(body_element) > 0) {
      body_text <- html_text(body_element) %>%
        str_squish() %>%
        paste(collapse = " ")
      body_text <- str_replace_all(body_text, "<.*?>", "")
      bodies <- c(bodies, body_text)
    } else {
      bodies <- c(bodies, "")
    }
  }
  
  # Convert lists to character vectors
  bodies <- unlist(bodies)
  
  # Create a data frame
  data_df <- data.frame(DocumentNumber = doc_counter:(doc_counter + length(bodies) - 1), Body = bodies)
  
  # Increment the document counter
  doc_counter <- doc_counter + length(bodies)
  
  # Store the data frame in the list
  all_data[[html_file]] <- data_df
}

# Combine all data frames into a single data frame
combined_data <- do.call(rbind, all_data)

# Save as CSV
write.csv(combined_data, "output_combined_faz.csv", row.names = FALSE)

