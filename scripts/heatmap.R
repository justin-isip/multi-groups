# Load the data
data <- read.csv("data/nrows_htg.csv", stringsAsFactors = TRUE)

# Extract the information that will be useful later
taxa <- data$Higher_taxon
ntaxa <- length(levels(taxa))

# Create a table of studies and the taxa they examine
library(tidyr); library(dplyr); library(ggplot2)

sum_f <- function(x) {sum(x) > 0}
  
studies <- data %>%
  # Drop the pointless column
  subset(select = -X) %>%
  # Make one column per taxon
  pivot_wider(
    names_from = Higher_taxon,
    values_from = number, values_fill = 0
  ) %>%
  # Change the values in each column to TRUE or FALSE depending on whether the
  # taxon is in the study.
  group_by(SS) %>%
  mutate(
    across(,function(x) {sum(x) > 0}) #  If everything is on one line you do not need the curly brackets
  ) %>%
  ungroup() %>%
  # Remove duplicates.
  distinct()

#sum(studies$Diptera & studies$Coleoptera) # sum the number of trues for studies that contain both diptera and coleoptera


# Build a matrix for plotting.
map <- matrix(
  0, nrow = ntaxa, ncol = ntaxa
)

# This code
colnames(map) <- rownames(map) <- colnames(studies[, 1+1:ntaxa])

# Is the same as this code
colnames(map) <- colnames(studies[, 1+1:ntaxa])
rownames(map) <- colnames(studies[, 1+1:ntaxa])

# This is a nested loop
# Populate the matrix with the study counts. For each pair of taxa, find the
# conjunction of their columns to identify which studies have both, then sum it.
for (i in 1:ntaxa) {
  for (j in i:ntaxa) {
    map[i,j] <- sum(studies[,i+1] & studies[,j+1])
  }
}

# This is a nested loop
# Populate the matrix with the study counts. For each pair of taxa, find the
# conjunction of their columns to identify which studies have both, then sum it.
for (row in 1:ntaxa) {
  for (col in row:ntaxa) {
    map[row,col] <- sum(studies[,row+1] & studies[,col+1])
  }
}

# Make a heatmap using base R, Colv = NA, Rowv = NA removes the dendrogram
heatmap(map,  Colv = NA, Rowv = NA, scale = "column", col = my_colors(100)) # Heatmap with manual colors


# Turn the matrix map back into a dataframe
df <- as.data.frame(map)
df$taxon1 <- rownames(df) # create a new column using the names of each taxon in the rows of df
df <- pivot_longer(df, cols=colnames(map)) # turn the columns from map into rows against the rownames(taxon1) that we just created
colnames(df)[2] <- "taxon2"

# Create a heatmap that showing the number of studies that compare one group with another
df %>%
  ggplot(aes(x = reorder(taxon1, -value), y = reorder(taxon2, -value), fill = value)) +
  geom_tile() +
  # scale_fill_gradient(trans = 'log') + # log scale
  theme(axis.text.x = element_text(angle = 45, hjust= 1)) +
  # geom_text(aes(label = round(log(number), 1)))
  geom_text(aes(label = value), color = "white", size = 3) + 
  guides(fill=guide_legend(title="Study frequency")) +
  xlab("Taxonomic group 1") +
  ylab("Taxonomic group 2") 


