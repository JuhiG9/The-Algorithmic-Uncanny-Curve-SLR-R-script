# Bibliometric cluster analysis for the systematic literature review

packages <- c(
  "readxl", "dplyr", "stringr", "tidyr",
  "writexl", "igraph", "ggplot2", "bibliometrix"
)

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p, dependencies = TRUE)
  }
}

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)
library(igraph)
library(ggplot2)
library(bibliometrix)

# File paths
file_path <- "C:/Users/Juhi/Downloads/Selected_Articles_385.xlsx"
output_folder <- "C:/Users/Juhi/Downloads/Bibliometric_Results"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Import data
raw <- read_excel(file_path)

cat("\nFile imported successfully.\n")
cat("Number of rows:", nrow(raw), "\n")
cat("Number of columns:", ncol(raw), "\n\n")
cat("Column names:\n")
print(names(raw))

# Check the Scopus fields used below
required_columns <- c(
  "Authors",
  "Author full names",
  "Author(s) ID",
  "Title",
  "Year",
  "Source title",
  "Volume",
  "Issue",
  "Art. No.",
  "Page start",
  "Page end",
  "Cited by",
  "DOI",
  "Link",
  "Author Keywords",
  "Publisher",
  "Document Type",
  "Publication Stage",
  "Open Access",
  "Source",
  "EID"
)

missing_columns <- setdiff(required_columns, names(raw))

if (length(missing_columns) > 0) {
  cat("\nMissing columns:\n")
  print(missing_columns)
  stop("Please check the Excel column names.")
}

cat("\nAll required columns found.\n")

# Standardise fields
data <- raw %>%
  mutate(
    Authors = as.character(Authors),
    `Author full names` = as.character(`Author full names`),
    `Author(s) ID` = as.character(`Author(s) ID`),
    Title = as.character(Title),
    Year = as.numeric(Year),
    `Source title` = as.character(`Source title`),
    `Cited by` = as.numeric(`Cited by`),
    DOI = as.character(DOI),
    `Author Keywords` = as.character(`Author Keywords`),
    Publisher = as.character(Publisher),
    `Document Type` = as.character(`Document Type`),
    `Publication Stage` = as.character(`Publication Stage`),
    `Open Access` = as.character(`Open Access`),
    Source = as.character(Source),
    EID = as.character(EID)
  )

# Remove records with neither title nor keywords
data <- data %>%
  filter(
    !is.na(Title) |
      !is.na(`Author Keywords`)
  )

cat("\nRecords after removing empty records:", nrow(data), "\n")

# Clean author keywords
data <- data %>%
  mutate(
    Keywords_Clean = `Author Keywords`,
    Keywords_Clean = ifelse(
      is.na(Keywords_Clean),
      "",
      Keywords_Clean
    ),
    Keywords_Clean = str_replace_all(
      Keywords_Clean,
      "\\|",
      ";"
    ),
    Keywords_Clean = str_replace_all(
      Keywords_Clean,
      ",",
      ";"
    ),
    Keywords_Clean = str_squish(Keywords_Clean),
    Keywords_Clean = str_to_lower(Keywords_Clean),
    Keywords_Clean = str_replace_all(
      Keywords_Clean,
      "\\s*;\\s*",
      ";"
    ),
    Keywords_Clean = str_replace_all(
      Keywords_Clean,
      ";+",
      ";"
    ),
    Keywords_Clean = str_remove_all(
      Keywords_Clean,
      "^;|;$"
    )
  )

keyword_records <- data %>%
  filter(
    !is.na(Keywords_Clean),
    Keywords_Clean != ""
  )

cat("\nKeyword coverage\n")
cat("Records with author keywords:", nrow(keyword_records), "\n")
cat("Records without author keywords:",
    nrow(data) - nrow(keyword_records), "\n")

# Prepare bibliometrix data
M <- data.frame(
  AU = data$Authors,
  AF = data$`Author full names`,
  TI = data$Title,
  PY = data$Year,
  SO = data$`Source title`,
  DE = data$Keywords_Clean,
  TC = data$`Cited by`,
  DI = data$DOI,
  PU = data$Publisher,
  DT = data$`Document Type`,
  LA = "English",
  stringsAsFactors = FALSE
)

M_keyword <- M %>%
  filter(
    !is.na(DE),
    DE != ""
  )

cat("\nBibliometric dataset\n")
cat("Total records:", nrow(M), "\n")
cat("Records with author keywords:", nrow(M_keyword), "\n")

write_xlsx(
  M_keyword,
  file.path(
    output_folder,
    "Clean_Bibliometrix_Dataset.xlsx"
  )
)

# Keyword frequencies
keyword_list <- str_split(
  M_keyword$DE,
  pattern = ";"
)

keyword_vector <- unlist(keyword_list)
keyword_vector <- str_squish(keyword_vector)
keyword_vector <- keyword_vector[keyword_vector != ""]

keyword_frequency <- as.data.frame(
  table(keyword_vector),
  stringsAsFactors = FALSE
)

names(keyword_frequency) <- c(
  "Keyword",
  "Frequency"
)

keyword_frequency <- keyword_frequency %>%
  arrange(desc(Frequency))

cat("\nTop 30 author keywords\n")
print(head(keyword_frequency, 30))

write_xlsx(
  keyword_frequency,
  file.path(
    output_folder,
    "Keyword_Frequency.xlsx"
  )
)

# Keyword co-occurrence matrix
NetMatrix <- biblioNetwork(
  M_keyword,
  analysis = "co-occurrences",
  network = "keywords",
  sep = ";"
)

cat("\nCo-occurrence matrix dimensions:",
    nrow(NetMatrix), "x", ncol(NetMatrix), "\n")

write.csv(
  as.matrix(NetMatrix),
  file.path(
    output_folder,
    "Keyword_Cooccurrence_Matrix.csv"
  ),
  row.names = TRUE
)

# Primary keyword threshold
min_frequency <- 3

selected_keywords <- keyword_frequency %>%
  filter(Frequency >= min_frequency)

cat("\nMinimum keyword frequency:", min_frequency, "\n")
cat("Keywords retained:", nrow(selected_keywords), "\n")

selected_names <- selected_keywords$Keyword

network_matrix <- NetMatrix[
  intersect(
    rownames(NetMatrix),
    selected_names
  ),
  intersect(
    colnames(NetMatrix),
    selected_names
  ),
  drop = FALSE
]

cat("Network contains", nrow(network_matrix), "keywords.\n")

# Community detection
g <- graph_from_adjacency_matrix(
  network_matrix,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g <- delete_vertices(
  g,
  V(g)[degree(g) == 0]
)

cat("\nNetwork information\n")
cat("Number of keywords:", vcount(g), "\n")
cat("Number of keyword connections:", ecount(g), "\n")

set.seed(1234)

louvain_result <- cluster_louvain(
  g,
  weights = E(g)$weight
)

cluster_membership <- membership(louvain_result)

number_of_clusters <- length(
  unique(cluster_membership)
)

primary_modularity <- modularity(louvain_result)

cat("\nClustering result\n")
cat("Number of clusters identified:", number_of_clusters, "\n")
cat("Modularity:", round(primary_modularity, 4), "\n")

# Cluster membership
cluster_table <- data.frame(
  Keyword = names(cluster_membership),
  Cluster = as.integer(cluster_membership),
  Frequency = selected_keywords$Frequency[
    match(
      names(cluster_membership),
      selected_keywords$Keyword
    )
  ],
  stringsAsFactors = FALSE
) %>%
  arrange(
    Cluster,
    desc(Frequency)
  )

cat("\nKeywords by cluster\n")

for (cl in sort(unique(cluster_table$Cluster))) {
  cat("\nCluster", cl, "\n")

  print(
    cluster_table %>%
      filter(Cluster == cl) %>%
      arrange(desc(Frequency))
  )
}

write_xlsx(
  cluster_table,
  file.path(
    output_folder,
    "Keyword_Cluster_Membership.xlsx"
  )
)

# Cluster summary
cluster_summary <- cluster_table %>%
  group_by(Cluster) %>%
  summarise(
    Number_of_keywords = n(),
    Total_keyword_frequency = sum(
      Frequency,
      na.rm = TRUE
    ),
    Top_keywords = paste(
      head(
        Keyword[
          order(Frequency, decreasing = TRUE)
        ],
        10
      ),
      collapse = "; "
    ),
    .groups = "drop"
  )

print(cluster_summary)

write_xlsx(
  cluster_summary,
  file.path(
    output_folder,
    "Cluster_Summary.xlsx"
  )
)

# Keyword network
png(
  filename = file.path(
    output_folder,
    "Keyword_Cooccurrence_Network.png"
  ),
  width = 3000,
  height = 2400,
  res = 300
)

set.seed(1234)

plot(
  louvain_result,
  g,
  vertex.size =
    5 + 2 * log(
      selected_keywords$Frequency[
        match(
          V(g)$name,
          selected_keywords$Keyword
        )
      ] + 1
    ),
  vertex.label.cex = 0.7,
  vertex.label.color = "black",
  vertex.frame.color = "grey30",
  edge.width =
    0.5 + 3 *
    E(g)$weight /
    max(E(g)$weight),
  edge.color = "grey70",
  layout = layout_with_fr(g),
  main = paste0(
    "Author Keyword Co-occurrence Network\n",
    "Minimum keyword frequency = ",
    min_frequency,
    " | Clusters = ",
    number_of_clusters
  )
)

dev.off()

# Thematic map
cat("\nCreating thematic map...\n")

set.seed(1234)

thematic_results <- thematicMap(
  M_keyword,
  field = "DE",
  n = 250,
  minfreq = min_frequency,
  stemming = FALSE,
  size = 0.5,
  n.labels = 5,
  repel = TRUE
)

png(
  filename = file.path(
    output_folder,
    "Thematic_Map.png"
  ),
  width = 3000,
  height = 2400,
  res = 300
)

plot(
  thematic_results,
  main = "Thematic Map of Author Keywords"
)

dev.off()

if (!is.null(thematic_results$map)) {
  thematic_map_data <- as.data.frame(
    thematic_results$map
  )

  write_xlsx(
    thematic_map_data,
    file.path(
      output_folder,
      "Thematic_Map_Data.xlsx"
    )
  )
}

# Sensitivity analysis: minimum frequency = 2
cat("\nSensitivity analysis\n")

min_frequency_sensitivity <- 2

selected_keywords_2 <- keyword_frequency %>%
  filter(Frequency >= min_frequency_sensitivity)

selected_names_2 <- selected_keywords_2$Keyword

network_matrix_2 <- NetMatrix[
  intersect(
    rownames(NetMatrix),
    selected_names_2
  ),
  intersect(
    colnames(NetMatrix),
    selected_names_2
  ),
  drop = FALSE
]

g2 <- graph_from_adjacency_matrix(
  network_matrix_2,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

g2 <- delete_vertices(
  g2,
  V(g2)[degree(g2) == 0]
)

set.seed(1234)

louvain_result_2 <- cluster_louvain(
  g2,
  weights = E(g2)$weight
)

membership_2 <- membership(louvain_result_2)

clusters_2 <- length(
  unique(membership_2)
)

modularity_2 <- modularity(
  louvain_result_2
)

cat("Clusters at minimum frequency 2:", clusters_2, "\n")
cat("Modularity:", round(modularity_2, 4), "\n")

sensitivity_results <- data.frame(
  Analysis = c(
    "Primary analysis",
    "Sensitivity analysis"
  ),
  Minimum_keyword_frequency = c(
    3,
    2
  ),
  Number_of_keywords = c(
    vcount(g),
    vcount(g2)
  ),
  Number_of_clusters = c(
    number_of_clusters,
    clusters_2
  ),
  Modularity = c(
    primary_modularity,
    modularity_2
  )
)

write_xlsx(
  sensitivity_results,
  file.path(
    output_folder,
    "Sensitivity_Analysis.xlsx"
  )
)

print(sensitivity_results)

# Analysis log
sink(
  file.path(
    output_folder,
    "Analysis_Log.txt"
  )
)

cat("BIBLIOMETRIC ANALYSIS LOG\n\n")

cat(
  "Input file:\n",
  file_path,
  "\n\n"
)

cat(
  "Total records in input:",
  nrow(raw),
  "\n"
)

cat(
  "Records analysed:",
  nrow(M_keyword),
  "\n"
)

cat(
  "Keyword threshold:",
  min_frequency,
  "\n"
)

cat(
  "Primary clusters:",
  number_of_clusters,
  "\n"
)

cat(
  "Primary modularity:",
  primary_modularity,
  "\n"
)

cat(
  "Sensitivity clusters:",
  clusters_2,
  "\n"
)

cat(
  "Sensitivity modularity:",
  modularity_2,
  "\n\n"
)

cat("R version:\n")
print(R.version.string)

cat("\nBibliometrix version:\n")
print(packageVersion("bibliometrix"))

cat("\nigraph version:\n")
print(packageVersion("igraph"))

cat("\nAnalysis completed:\n")
print(Sys.time())

sink()

cat("\nAnalysis finished.\n")
cat("Input records:", nrow(raw), "\n")
cat("Records with keywords:", nrow(M_keyword), "\n")
cat("Primary clusters:", number_of_clusters, "\n")
cat("Primary modularity:", round(primary_modularity, 4), "\n")
cat("Sensitivity clusters:", clusters_2, "\n")
cat("Results saved to:", output_folder, "\n")
cat("\nInspect cluster membership before assigning thematic labels.\n")
