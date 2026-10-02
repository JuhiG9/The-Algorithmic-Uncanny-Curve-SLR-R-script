# =============================================================================
# Bibliometric analysis for:
# "The Algorithmic Uncanny Curve: A Systematic Literature Review of
#  Anthropomorphic AI, Consumer Trust, and Affective Resistance in Agentic
#  Marketing Ecosystems"
#
# Reproduces the bibliometrix workflow described in Section 4.1 (Bibliometric
# analysis) and Section 4.2 (Figures 3 and 5, plus the robustness check):
#   1. Import the Scopus export and restrict it to the 174 retained articles
#   2. Descriptive bibliometrics (production, sources, authors, countries...)
#   3. Keyword co-occurrence network            -> Figure 5
#   4. Co-citation network                      -> intellectual structure
#   5. Thematic map (centrality x density)      -> Figure 3
#   6. Robustness check of cluster settings     -> "network clusters are
#                                                   sensitive to settings"
#   7. Tabulation of the study-level coding     -> Table 3 (Supplementary S1)
#
# INPUTS (put in the working directory or edit the paths below)
#   scopus_export.csv     Scopus export of the 561 records (or the 174 final
#                         ones) - export ALL fields incl. "Cited references"
#   retained_174.csv      Screening log with one column of Scopus EIDs (or DOIs)
#                         for the 174 retained articles
#   S1_coding_sheet.xlsx  Supplementary File S1 (study-level coding)
#
# Tested logic against bibliometrix >= 4.x. Column names in your own files
# may differ - the places to check are marked  <<< EDIT.
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("bibliometrix", "igraph", "dplyr", "tidyr", "ggplot2",
          "readr", "readxl", "writexl", "stringr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(2026)                                   # reproducible layouts/clusters
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/figures", showWarnings = FALSE)
dir.create("outputs/tables",  showWarnings = FALSE)

save_plot <- function(plot_expr, file, w = 11, h = 7, res = 300) {
  png(file.path("outputs/figures", file), width = w, height = h,
      units = "in", res = res)
  on.exit(dev.off())
  plot_expr()
}

# ---- 1. Import and restrict to the final corpus (n = 174) -------------------
M_all <- convert2df(file     = "scopus_export.csv",   # <<< EDIT
                    dbsource = "scopus",
                    format   = "csv")
cat("Records imported:", nrow(M_all), "\n")

retained <- read_csv("retained_174.csv", show_col_types = FALSE)   # <<< EDIT
# Match on the Scopus EID (bibliometrix keeps it in column "UT" for Scopus).
# If your log uses DOIs, replace the two lines below with: M_all$DI / retained$DOI
key_log <- retained$EID                                            # <<< EDIT
M <- M_all[M_all$UT %in% key_log, ]

cat("Retained articles in corpus:", nrow(M), "(paper reports 174)\n")
stopifnot(nrow(M) > 0)
if (nrow(M) != 174) warning("Corpus size is not 174 - check the ID matching.")

# ---- 1b. Keyword cleaning (synonyms / noise terms) --------------------------
# Merge variants BEFORE building networks, otherwise 'chatbot', 'chatbots',
# 'chat-bot' show up as separate nodes. Extend this list after inspecting
# the keyword frequency table in step 2.
synonyms <- c(
  "CHATBOT;CHATBOTS;CHAT-BOT;CHAT BOTS",
  "ARTIFICIAL INTELLIGENCE;AI",
  "ANTHROPOMORPHISM;ANTHROPOMORPHIC;ANTHROPOMORPHIZATION",
  "VIRTUAL INFLUENCER;VIRTUAL INFLUENCERS;AI INFLUENCER",
  "UNCANNY VALLEY;UNCANNY VALLEY EFFECT",
  "GENERATIVE AI;GENERATIVE ARTIFICIAL INTELLIGENCE",
  "CONSUMER TRUST;TRUST"
)
# Generic terms that carry no thematic information (also the search terms
# themselves tend to dominate every cluster)
remove_terms <- c("ARTICLE", "HUMAN", "HUMANS", "HUMAN EXPERIMENT",
                  "CONTROLLED STUDY", "MALE", "FEMALE", "ADULT",
                  "QUESTIONNAIRE", "PRIORITY JOURNAL")

# ---- 2. Descriptive bibliometrics -------------------------------------------
res <- biblioAnalysis(M, sep = ";")
S   <- summary(res, k = 15, pause = FALSE)

# Main information (timespan, sources, docs, avg. citations, collaboration...)
write_xlsx(list(MainInformation = S$MainInformation,
                AnnualProduction = S$AnnualProduction,
                MostProdAuthors = S$MostProdAuthors,
                MostCitedPapers = S$MostCitedPapers,
                MostProdCountries = S$MostProdCountries,
                TCperCountries = S$TCperCountries,
                MostRelSources = S$MostRelSources,
                MostRelKeywords = S$MostRelKeywords),
           "outputs/tables/descriptive_summary.xlsx")

save_plot(function() print(plot(res, k = 10, pause = FALSE)[[1]]),
          "descriptive_overview.png")

# Scientific production over time
ap <- as.data.frame(table(res$Years))
names(ap) <- c("Year", "Articles")
p_prod <- ggplot(ap, aes(as.numeric(as.character(Year)), Articles)) +
  geom_col(fill = "#2c5aa0") +
  scale_x_continuous(breaks = seq(2015, 2026, 1)) +
  labs(x = "Year", y = "Articles",
       title = "Annual scientific production (n = 174)") +
  theme_minimal(base_size = 12)
ggsave("outputs/figures/annual_production.png", p_prod, width = 8, height = 4.5, dpi = 300)

# Keyword frequency tables - inspect these to refine 'synonyms'/'remove_terms'
kw_author  <- tableTag(M, "DE")   # author keywords
kw_indexed <- tableTag(M, "ID")   # Keywords Plus / indexed keywords
write_xlsx(list(AuthorKeywords  = as.data.frame(head(kw_author, 100)),
                IndexedKeywords = as.data.frame(head(kw_indexed, 100))),
           "outputs/tables/keyword_frequencies.xlsx")

# Bradford / Lotka (optional, available in Biblioshiny "Sources"/"Authors")
bradford(M)$graph
lotka <- lotka(res)
print(lotka$AuthorProd)

# ---- 3. Keyword co-occurrence network (Figure 5) ----------------------------
# Author keywords if available for most records, otherwise indexed keywords.
kw_field <- if (mean(nzchar(M$DE)) > 0.6) "author_keywords" else "keywords"

NetMatrix_kw <- biblioNetwork(M,
                              analysis = "co-occurrences",
                              network  = kw_field,
                              sep      = ";")

# Optionally apply synonym/stop-word cleaning through the field tag instead:
#   M <- termExtraction(M, Field = "DE", remove.terms = remove_terms,
#                       synonyms = synonyms, verbose = FALSE)
# and then use network = "author_keywords" on the cleaned column.

save_plot(function() {
  net_kw <<- networkPlot(NetMatrix_kw,
                         normalize   = "association",   # association strength
                         n           = 50,              # top-50 keywords <<< EDIT
                         Title       = "Keyword co-occurrence network",
                         type        = "auto",
                         cluster     = "louvain",       # Biblioshiny default
                         size        = TRUE,
                         size.cex    = TRUE,            # node size = frequency
                         labelsize   = 1.1,
                         label.cex   = TRUE,
                         edgesize    = 3,
                         edges.min   = 2,               # drop weak links
                         remove.multiple = FALSE,
                         remove.isolates = TRUE,
                         halo        = FALSE,
                         curved      = FALSE,
                         verbose     = FALSE)
}, "Figure5_keyword_cooccurrence.png", w = 12, h = 8)

# Cluster membership table -> use it to label C1...Cn in the paper
kw_clusters <- data.frame(
  Keyword = V(net_kw$graph)$name,
  Cluster = net_kw$cluster_res$cluster,
  Degree  = igraph::degree(net_kw$graph)
) |> arrange(Cluster, desc(Degree))
write_xlsx(kw_clusters, "outputs/tables/keyword_network_clusters.xlsx")
print(table(kw_clusters$Cluster))

# ---- 4. Co-citation network (intellectual structure) ------------------------
# Needs the "Cited references" field (CR) in the Scopus export.
if (all(is.na(M$CR) | M$CR == "")) {
  warning("No cited references in the export - skipping co-citation analysis.")
} else {
  NetMatrix_cc <- biblioNetwork(M,
                                analysis = "co-citation",
                                network  = "references",
                                sep      = ";")

  save_plot(function() {
    net_cc <<- networkPlot(NetMatrix_cc,
                           normalize = NULL,
                           n         = 40,                 # top-40 cited refs <<< EDIT
                           Title     = "Co-citation network (cited references)",
                           type      = "auto",
                           cluster   = "louvain",
                           size.cex  = TRUE,
                           labelsize = 0.8,
                           edgesize  = 3,
                           remove.isolates = TRUE,
                           verbose   = FALSE)
  }, "co_citation_network.png", w = 12, h = 8)

  cc_clusters <- data.frame(
    Reference = V(net_cc$graph)$name,
    Cluster   = net_cc$cluster_res$cluster
  ) |> arrange(Cluster)
  write_xlsx(cc_clusters, "outputs/tables/co_citation_clusters.xlsx")

  # Most frequently cited references across the 174 articles
  cr_top <- citations(M, field = "article", sep = ";")
  write_xlsx(as.data.frame(head(cr_top$Cited, 50)),
             "outputs/tables/top_cited_references.xlsx")
}

# ---- 5. Thematic map (Figure 3) ---------------------------------------------
# Quadrants: Motor (high centrality / high density), Niche (low / high),
# Basic (high / low), Emerging-or-declining (low / low)  [Cobo et al., 2011;
# Aria & Cuccurullo, 2017].
# With only 174 documents, minfreq must be low (2-3) or very few themes appear.
tm <- thematicMap(M,
                  field      = "DE",        # author keywords ("ID" = Keywords Plus)
                  n          = 250,         # max. words used in the network
                  minfreq    = 3,           # min. occurrences per cluster <<< EDIT
                  stemming   = FALSE,
                  size       = 0.5,         # bubble size
                  n.labels   = 3,           # labels shown per bubble
                  repel      = TRUE,
                  remove.terms = tolower(remove_terms),
                  synonyms   = tolower(synonyms),
                  cluster    = "walktrap")  # bibliometrix default

ggsave("outputs/figures/Figure3_thematic_map.png", tm$map,
       width = 11, height = 7, dpi = 300)

# Table behind the map: centrality (Callon), density, rank, cluster label
write_xlsx(list(Clusters = tm$clusters,
                Words    = tm$words),
           "outputs/tables/thematic_map_clusters.xlsx")
print(tm$clusters[, c("name", "label", "Callon_Centrality",
                      "Callon_Density", "rank_centrality", "rank_density",
                      "freq")])

# Thematic evolution (optional): split the window at 2020 and 2023
# te <- thematicEvolution(M, field = "DE", years = c(2020, 2023),
#                         n = 250, minFreq = 2)
# plotThematicEvolution(te$Nodes, te$Edges)

# ---- 6. Robustness check of the clustering ----------------------------------
# The paper notes that with 174 documents "network clusters are sensitive to
# settings". This quantifies that: re-run the thematic map under alternative
# settings and compare the keyword partitions (adjusted Rand index).
grid <- expand.grid(field    = c("DE", "ID"),
                    minfreq  = c(2, 3, 5),
                    cluster  = c("walktrap", "louvain", "infomap"),
                    stringsAsFactors = FALSE)

fits <- lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  out <- try(thematicMap(M, field = g$field, n = 250, minfreq = g$minfreq,
                         stemming = FALSE, size = 0.5, n.labels = 3,
                         repel = TRUE, cluster = g$cluster), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)
  list(setting = paste(g$field, g$minfreq, g$cluster, sep = "|"),
       nclust  = length(unique(out$words$Cluster)),
       words   = out$words[, c("Words", "Cluster")])
})
fits <- Filter(Negate(is.null), fits)

robust_summary <- data.frame(
  Setting  = sapply(fits, `[[`, "setting"),
  Clusters = sapply(fits, `[[`, "nclust")
)
print(robust_summary)

# Pairwise agreement against the baseline (first successful DE|3|walktrap run)
base_id <- which(robust_summary$Setting == "DE|3|walktrap")
if (length(base_id) == 1) {
  base <- fits[[base_id]]$words
  robust_summary$ARI_vs_baseline <- sapply(fits, function(f) {
    common <- intersect(base$Words, f$words$Words)
    if (length(common) < 5) return(NA_real_)
    a <- base$Cluster[match(common, base$Words)]
    b <- f$words$Cluster[match(common, f$words$Words)]
    igraph::compare(as.integer(factor(a)), as.integer(factor(b)),
                    method = "adjusted.rand")
  })
}
write_xlsx(robust_summary, "outputs/tables/robustness_cluster_settings.xlsx")

# Same idea for the keyword co-occurrence network (Figure 5): compare
# community-detection algorithms on one fixed graph.
g_kw <- igraph::graph_from_adjacency_matrix(
  as.matrix(NetMatrix_kw[rownames(NetMatrix_kw) %in% V(net_kw$graph)$name,
                         colnames(NetMatrix_kw) %in% V(net_kw$graph)$name]),
  mode = "undirected", weighted = TRUE, diag = FALSE)

comm <- list(
  louvain     = cluster_louvain(g_kw),
  walktrap    = cluster_walktrap(g_kw),
  fast_greedy = cluster_fast_greedy(g_kw),
  infomap     = cluster_infomap(g_kw),
  label_prop  = cluster_label_prop(g_kw)
)
comm_summary <- data.frame(
  Algorithm  = names(comm),
  Clusters   = sapply(comm, function(x) length(x)),
  Modularity = sapply(comm, modularity),
  ARI_vs_louvain = sapply(comm, function(x)
    igraph::compare(membership(comm$louvain), membership(x),
                    method = "adjusted.rand"))
)
print(comm_summary)
write_xlsx(comm_summary, "outputs/tables/robustness_community_algorithms.xlsx")

# ---- 7. Study-level coding summary (Table 3) --------------------------------
# Supplementary File S1 holds one row per study. Adjust the column names to
# the real sheet. This is a tabulation of the manual coding, NOT a bibliometrix
# output, but it belongs to the same reproducibility package.
coding <- read_excel("S1_coding_sheet.xlsx")                       # <<< EDIT
# Example expected columns (logical TRUE/FALSE or 1/0 or "Yes"/"No"):
#   tests_nonlinearity, reports_valley, linear_positive, linear_negative,
#   null_mixed, two_or_three_conditions, conceptual_qualitative, agentic
yes <- function(x) sum(x %in% c(TRUE, 1, "1", "Yes", "yes", "Y", "TRUE"), na.rm = TRUE)

if (all(c("tests_nonlinearity", "reports_valley", "linear_positive",
          "linear_negative", "null_mixed", "two_or_three_conditions",
          "conceptual_qualitative", "agentic") %in% names(coding))) {
  table3 <- data.frame(
    Category = c("Tests non-linearity (>=4 realism levels or continuous)",
                 "Reports a valley-shaped (non-monotonic) effect",
                 "Reports linear positive effect",
                 "Reports linear negative effect",
                 "Reports null or mixed effects",
                 "Only two or three conditions (cannot detect a curve)",
                 "Conceptual or qualitative (no functional-form test)",
                 "Involves delegated autonomy or agentic commerce"),
    Studies  = c(yes(coding$tests_nonlinearity), yes(coding$reports_valley),
                 yes(coding$linear_positive),    yes(coding$linear_negative),
                 yes(coding$null_mixed),         yes(coding$two_or_three_conditions),
                 yes(coding$conceptual_qualitative), yes(coding$agentic))
  )
  table3$Percent <- round(100 * table3$Studies / nrow(coding), 1)
  print(table3)
  write_xlsx(table3, "outputs/tables/Table3_functional_form.xlsx")
} else {
  message("Table 3: rename the coding columns in step 7 to match S1.")
}

# ---- 8. Reproducibility log -------------------------------------------------
writeLines(capture.output(sessionInfo()), "outputs/sessionInfo.txt")
saveRDS(M, "outputs/bibliometrix_corpus_174.rds")
cat("\nDone. Figures in outputs/figures, tables in outputs/tables.\n")

# ---- Optional: run the same analysis interactively in Biblioshiny -----------
# bibliometrix::biblioshiny()
#   Data > Import raw file > Scopus (.csv) > load the 174-record file
#   Conceptual Structure > Co-occurrence Network   (field: author keywords,
#                          layout: auto, clustering: louvain, nodes: 50)
#   Conceptual Structure > Thematic Map            (min. cluster frequency: 3)
#   Intellectual Structure > Co-citation Network
