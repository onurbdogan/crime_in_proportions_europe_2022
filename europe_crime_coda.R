library(tidyverse)
library(robCompositions)
library(readxl)
library(gplots)
library(ggplot2)

Sys.setenv(LANGUAGE="en")
options(scipen = 999)
cat("\014")

# ---------------------------------------------------------------------
#               PREPROCESSING - START
# ---------------------------------------------------------------------

# Step 1: Read the data from the Excel file
# This excel file was created by filtering and combining other excel files which are downloaded from UN website.
file_path <- "data/europe_crime_rate_2022.xlsx"  # Adjust the path if necessary
crime_data <- read_excel(file_path) %>%
  mutate(Country = ifelse(Country=="Netherlands (Kingdom of the)","Netherlands",Country))


# Step 2: Filter for the year 2022
# This file includes data for only 2022 but we apply filter additionally to be sure.
crime_data_2022 <- crime_data %>%
  filter(Year == 2022 & Sex == "Total" & Age == "Total") %>%
  # Türkiye, Vatican,Liechtenstein, Republic of Moldova and North Macedonia are removed due to NA values
  filter(!Iso3_code  %in% c("TUR","VAT","MKD","LIE", "MDA")  ) %>%
   select(c(Country,Category,VALUE))


# Step 3: Pivot wider for the 'Category' column
# replace 'spaces' with '_' in column names and removes ':'
# make country column rownames
# remove irrelevant corruption columns

#Extracted columns sexual violence due to dependent variable problem
# and Other_acts_of_sexual_violence column since it has a lot of na values
#sum of two columns is equal to "Corruption" column
#"Death_due_to_intentional_homicide_in_prison" column include lots of zero and NAs.

cols_to_rm <- c(
                "Sexual_violence",
                "Corruption_Bribery", "Corruption_Other_acts_of_corruption",
                "Death_due_to_intentional_homicide_in_prison",
                "Fraud_Cyber-related_(Cy)"
                )

crime_data_wide <- crime_data_2022 %>%
  pivot_wider(
    names_from = Category,
    values_from = VALUE) %>% 
  rename_with(~ gsub(" ", "_", .)) %>%
  rename_with(~ gsub(":", "", .)) %>%
  rename_with(~ gsub("Sexual_violence_", "", .)) %>%
  select(-any_of(cols_to_rm)) %>%
  select(-contains("Other_acts_of_sexual_violence")) %>%
  column_to_rownames(var = "Country") %>%
  mutate(across(everything(), ~ as.numeric(.)))


# Combine the UK rows by summing all columns
uk_combined <- crime_data_wide %>%
  filter(rownames(.) %in% c("United Kingdom (England and Wales)", 
                            "United Kingdom (Northern Ireland)", 
                            "United Kingdom (Scotland)")) %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE))) 

#repace NaN with NA
uk_combined[uk_combined=="NaN"] <- NA

# Set the row name to "UK"
rownames(uk_combined) <- "United Kingdom"

# Remove the original UK rows from the data frame
crime_data_wide <- crime_data_wide[!rownames(crime_data_wide) %in% c("United Kingdom (England and Wales)", 
                                                                     "United Kingdom (Northern Ireland)", 
                                                                     "United Kingdom (Scotland)"), ]

# Combine the UK row into the main data frame
crime_data_wide <- rbind(crime_data_wide, uk_combined)
rm(uk_combined)

country_codes <- unique(crime_data %>% select(c(Iso3_code,Country)))
country_codes <- rbind(country_codes, c("GBR","United Kingdom"))


if("Death_due_to_intentional_homicide_in_prison" %in% colnames(crime_data_wide)) {
  # Assign mean for NA values in Death_due_to_intentional_homicide_in_prison
  mean_deaths <- mean(crime_data_wide$Death_due_to_intentional_homicide_in_prison, na.rm=T)
  crime_data_wide <- crime_data_wide %>%
    mutate(Death_due_to_intentional_homicide_in_prison =
             ifelse(is.na(Death_due_to_intentional_homicide_in_prison),
                    mean_deaths,Death_due_to_intentional_homicide_in_prison))
}


# These values were obtained by interpolating the previously available data.
# interpolated_na_values.xlsx shows results.
fill_values <- data.frame(
  Country = c("Albania", "France", "Ireland", "Italy", "Poland", "United Kingdom",
              "Ireland", "Latvia", "Hungary", "France", "Hungary", "France", 
              "Netherlands", "Poland"),
  Variable = c("Persons_convicted_for_intentional_homicide", 
               "Persons_convicted_for_intentional_homicide", 
               "Persons_convicted_for_intentional_homicide", 
               "Persons_convicted_for_intentional_homicide",
               "Persons_convicted_for_intentional_homicide", 
               "Persons_convicted_for_intentional_homicide", 
               "Smuggling_of_migrants", 
               "Smuggling_of_migrants", 
               "Burglary", 
               "Theft_of_a_motorized_vehicle", 
               "Serious_assault", 
               "Kidnapping", 
               "Kidnapping", 
               "Kidnapping"),
  Value = c(3.355180, 0.629761, 0.219159, 0.374271, 0.444837, 0.526850, 
            0.829857, 0.4269127, 294.482569, 123.427168, 163.093699, 
            5.848379, 1.810645, 0.908473)
)

# Update the values in the crime_data_wide dataset
for (i in 1:nrow(fill_values)) {
  country <- fill_values$Country[i]
  variable <- fill_values$Variable[i]
  value <- fill_values$Value[i]
  
  # Update the value in the dataset
  if (country %in% rownames(crime_data_wide) && variable %in% colnames(crime_data_wide)) {
    crime_data_wide[country, variable] <- value
  }
}

crime_data_wide %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

# Define a small value (epsilon)
epsilon <- 0.001
crime_data_wide[crime_data_wide == 0] <- epsilon

which(is.na(crime_data_wide),arr.ind = TRUE)


#geometricmean function was not defined in the function
geometricmean<-gm
## the result is in the original space but scaled so that each row's mean is equal to 1
impute_and_average_impCoda <- function(data, init = "KNN", method = "ltsReg", n_iter = 10) {
  imputed_results <- list()
  for (i in 1:n_iter) {
    set.seed(123)
    imputed_results[[i]] <- impCoda(data, init = init, method = method)$xImp
  }
    averaged_result <- Reduce("+", imputed_results) / n_iter
  
  return(averaged_result)
}
data <- impute_and_average_impCoda(crime_data_wide, init = "KNN", method = "ltsReg", n_iter = 100)

# converting na filled compositional data into original space
unscaled_data <- data
for (i in 1:nrow(data)) {
  unscaled_data[i,] <- data[i,] * (crime_data_wide[i,1] / data[i,1])
}

colnames(unscaled_data) <- colnames(crime_data_wide)
rownames(unscaled_data) <- rownames(crime_data_wide)
unscaled_data<- unscaled_data %>%
  rownames_to_column(var = "Country") %>%
  left_join(country_codes, by = "Country") %>%
  # Keep Iso3_code and all other columns, remove
  select(Iso3_code, everything(), -Country) %>% 
  column_to_rownames(var = "Iso3_code")


colnames(data) <- colnames(crime_data_wide)
rownames(data) <- rownames(crime_data_wide)
# Replace the Country column with Iso3_code in data
data<- data %>%
  rownames_to_column(var = "Country") %>%
  left_join(country_codes, by = "Country") %>%
  # Keep Iso3_code and all other columns, remove
  select(Iso3_code, everything(), -Country) %>% 
  column_to_rownames(var = "Iso3_code")


saveRDS(data, "data/raw_data.rds")
write.csv(data, "data/raw_data.csv")

# ---------------------------------------------------------------------
#               PREPROCESSING - END
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
#               PART-1: CLUSTERING OBSERVATIONS (R-Mode) - START
# ---------------------------------------------------------------------

# Load necessary libraries
library(cluster)
library(factoextra)
library(mclust)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(Rtsne)
library(ggrepel) # For better label placement
library(dendextend)
cat("\014")

clustering_data <- pivotCoord(data)
rownames(clustering_data) <- rownames(data)

# Determine Optimal Clusters
# Elbow Method (for K-means)
fviz_nbclust(clustering_data, kmeans, method = "wss") +
  ggtitle("Elbow Method for K-means")

# Silhouette Method (for K-means)
fviz_nbclust(clustering_data, kmeans, method = "silhouette") +
  ggtitle("Silhouette Method for K-means")

# Gap Statistic (for K-means)
set.seed(123)
gap_stat <- clusGap(clustering_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# k is determined as 3.

# K-means Clustering
set.seed(123)
k <- 3  
kmeans_result <- kmeans(clustering_data, centers = k, nstart = 10)

# Hierarchical Clustering
dist_matrix <- dist(clustering_data, method = "euclidean")
# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")
# Cut the dendrogram into k clusters
hclust_clusters <- cutree(hclust_result, k = k)
# Visualize the dendrogram
fviz_dend(hclust_result, k = k, rect = TRUE) +
  ggtitle("Dendrogram of Hierarchical Clustering")

# Gaussian Mixture Models (GMM)
gmm_result <- Mclust(clustering_data, G = 1:10)  # Test up to 10 clusters
# Optimal number of clusters based on BIC
summary(gmm_result)  # Shows the best model and number of clusters

# Compare Clustering Results

# Evaluate Silhouette Scores for each method
kmeans_silhouette <- silhouette(kmeans_result$cluster, dist_matrix)
hclust_silhouette <- silhouette(hclust_clusters, dist_matrix)
gmm_silhouette <- silhouette(gmm_result$classification, dist_matrix)

# Visualize Silhouette Scores
fviz_silhouette(kmeans_silhouette) + ggtitle("Silhouette Plot for K-means")
fviz_silhouette(hclust_silhouette) + ggtitle("Silhouette Plot for Hierarchical Clustering")
fviz_silhouette(gmm_silhouette) + ggtitle("Silhouette Plot for GMM")

# K-means is preferred over other methods


# Visualize All Methods 

# Add cluster labels to the data for visualization
clustering_data$KMeans_Cluster <- as.factor(kmeans_result$cluster)
clustering_data$HClust_Cluster <- as.factor(hclust_clusters)
clustering_data$GMM_Cluster <- as.factor(gmm_result$classification)

world <- ne_countries(scale = "medium", type="map_units", returnclass = "sf",continent = "europe")
country_coords <- world %>%
  filter(iso_a3_eh %in% rownames(clustering_data)) %>%
  select(iso_a3_eh, name, geometry)

clustering_results <- clustering_data %>% 
  rownames_to_column(var="Iso3_code") %>%
  select(c("Iso3_code","KMeans_Cluster","HClust_Cluster","GMM_Cluster"))

# Join coordinates with clustering results
map_data <- country_coords %>%
  inner_join(clustering_results, by = c("iso_a3_eh" = "Iso3_code")) 


# Ensure clustering results are treated as factors
map_data <- map_data %>%
  mutate(
    KMeans_Cluster = as.factor(KMeans_Cluster),
    HClust_Cluster = as.factor(HClust_Cluster),
    GMM_Cluster = as.factor(GMM_Cluster)
  )


plot_map <- function(cluster_column, title) {
  ggplot(data = map_data) + 
    # Fill map regions based on the cluster column
    geom_sf(aes(fill = !!sym(cluster_column)), color = "black") +
    coord_sf(ylim = c(35, 71)) +
    # Add ISO3 codes as labels for countries
    geom_text(
      aes(label = iso_a3_eh, geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      color = "black"
    ) +
    # Use the custom color palette (red, green, blue)
    scale_fill_manual(
      name = "Cluster",
      values = c("1" = "#F8766D", "2" = "#00BA38", "3" = "#619CFF")
    ) +
    theme_minimal() +
    ggtitle(title) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5), # Center the title
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
    )
}

# Plot for K-means Clustering
plot_map("KMeans_Cluster", "K-means Clustering Results: Countries")
# create vector image
pdf("figures/countries_map.pdf")
plot_map("KMeans_Cluster", "")
dev.off()

# Apply tsne method
# Load necessary libraries
library(Rtsne)

# define clustering_data again since it includes clustering result labels
clustering_data <- pivotCoord(data)
rownames(clustering_data) <- rownames(data)

# Define the range of hyperparameters to tune
perplexity_values <- seq(5, 10, by = 1)  # Perplexity values to test
learning_rate_values <- c(10, 100, 200, 500, 1000)  # Learning rates to test

# Function to compute t-SNE and return the KL divergence (cost)
compute_tsne <- function(data, perplexity, learning_rate) {
  set.seed(123)  # For reproducibility
  tsne_result <- Rtsne(
    data,
    dims = 2,
    perplexity = perplexity,
    eta = learning_rate,
    verbose = FALSE,
    max_iter = 500
  )
  list(Y = tsne_result$Y, cost = tsne_result$itercosts[length(tsne_result$itercosts)])
}

# Initialize a data frame to store results
results <- data.frame(
  Perplexity = numeric(),
  LearningRate = numeric(),
  KL_Divergence = numeric()
)

# Perform grid search over hyperparameters
for (perplexity in perplexity_values) {
  for (learning_rate in learning_rate_values) {
    cat("Testing perplexity =", perplexity, "and learning_rate =", learning_rate, "\n")
    tsne_result <- compute_tsne(clustering_data, perplexity, learning_rate)
    results <- rbind(
      results,
      data.frame(
        Perplexity = perplexity,
        LearningRate = learning_rate,
        KL_Divergence = tsne_result$cost
      )
    )
  }
}

# Find the best hyperparameters (lowest KL divergence)
best_params <- results %>% arrange(KL_Divergence) %>% slice(1)

# Print the best parameters
cat("Best Parameters:\n")
print(best_params)

# Visualize the grid search results
ggplot(results, aes(x = Perplexity, y = KL_Divergence, color = factor(LearningRate))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "t-SNE Hyperparameter Tuning",
    x = "Perplexity",
    y = "KL Divergence",
    color = "Learning Rate"
  )

# Run t-SNE with the best parameters
final_tsne <- Rtsne(
  clustering_data,
  dims = 2,
  perplexity = best_params$Perplexity,
  eta = best_params$LearningRate,
  verbose = TRUE,
  max_iter = 500
)

tsne_results <- final_tsne

# Step 3: Create a data frame for visualization
tsne_df <- as.data.frame(tsne_results$Y)
colnames(tsne_df) <- c("Dim1", "Dim2")
tsne_df$Country <- rownames(data)  # Add country names

# Step 4: Visualize t-SNE results
ggplot(tsne_df, aes(x = Dim1, y = Dim2, label = Country)) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = Country), hjust = 1.2, vjust = 1.2, size = 3) +
  theme_minimal() +
  ggtitle("t-SNE Visualization with Pivot Coordinates") +
  xlab("t-SNE Dimension 1") +
  ylab("t-SNE Dimension 2")


# Assuming tsne_results$Y contains the t-SNE 2D embedding
tsne_df <- as.data.frame(tsne_results$Y)
colnames(tsne_df) <- c("Dim1", "Dim2")  # Name the dimensions
tsne_df$Iso3_code <- clustering_results$Iso3_code  # Add country codes
tsne_df$KMeans_Cluster <- as.factor(clustering_results$KMeans_Cluster)  # Add KMeans clusters
tsne_df$HClust_Cluster <- as.factor(clustering_results$HClust_Cluster)  # Add Hierarchical clusters
tsne_df$GMM_Cluster <- as.factor(clustering_results$GMM_Cluster)  # Add GMM clusters


# Function to plot t-SNE results colored by cluster
plot_tsne_clusters <- function(cluster_column, title) {
  ggplot(tsne_df, aes(x = Dim1, y = Dim2, color = cluster_column, label = Iso3_code)) +
    geom_point(size = 3) +
    geom_text(aes(label = Iso3_code), hjust = 1.2, vjust = 1.2, size = 3) +
    theme_minimal() +
    #    ggtitle(title) +
    xlab("t-SNE Dimension 1") +
    ylab("t-SNE Dimension 2") +
    scale_color_brewer(palette = "Set1")  # Use a color palette for clusters
}

# Plot KMeans clusters
plot_kmeans <- plot_tsne_clusters(tsne_df$KMeans_Cluster, "t-SNE Visualization with KMeans Clusters")
# Display the plots
print(plot_kmeans)

# create vector image
pdf("figures/tsne_with_kmeans.pdf",width=9.67,height=4.76)
print(plot_kmeans)
dev.off()


# ---------------------------------------------------------------------
#               PART-2: CLUSTERING VARIABLES (Q-mode) & PCA
# ---------------------------------------------------------------------

set.seed(123)
method <- "ward.D2"
d <- as.dist(variation(data))
clust <- hclust(d, method)
clust$labels <- gsub("_", " ", clust$labels)
clust_variable <- cutree(clust,3)
cluster_heights <- clust$height
plot(clust,  xlab = "", sub = "", main="")

dend <- clust %>% as.dendrogram
dend %>% plot
# create vector image
pdf("figures/cluster_variables.pdf",width=16,height=8.35)
par(mar = c(0,2,0,15))
dend %>% plot(horiz = TRUE)
dend %>% rect.dendrogram(k=3, border = "#880019", lty = 5, lwd = 2,horiz = TRUE)
text(8.6,0.8,"Cluster 3",col = "#880019", cex = 1)
text(8.6,6,"Cluster 2",col = "#880019", cex = 1)
text(8.6,12.5,"Cluster 1",col = "#880019", cex = 1)
dev.off()


first_clust_n <- names(clust_variable[clust_variable == 1])
first_clust <- data %>% select(all_of(first_clust_n))

pca_robust_fc<- pcaCoDa(first_clust, method = "robust")
pca_classical_fc<- pcaCoDa(first_clust, method = "classical")

summary(pca_robust_fc)
summary(pca_classical_fc)

par(mfrow=c(1,2), mar = c(4,4,2,2))
biplot(pca_classical_fc, ylabs = gsub("_", " ", colnames(first_clust)) ,col=c("grey40","red"))
biplot(pca_robust_fc, ylabs = gsub("_", " ", colnames(first_clust)) ,col=c("grey40","red"))

# create vector image
pdf("figures/pca_cluster1_variables.pdf",width=16,height=8.35)
# Set up the plotting area
par(mfrow = c(1, 2),mar = c(4,4,2,2))
biplot(pca_classical_fc, ylabs = gsub("_", " ", colnames(first_clust)) ,col=c("grey40","red"))
biplot(pca_robust_fc, ylabs = gsub("_", " ", colnames(first_clust)) ,col=c("grey40","red"))
# Close the PDF device
dev.off()


second_clust_n <- names(clust_variable[clust_variable == 2])
second_clust <- data %>% select(all_of(second_clust_n))

pca_robust_sc<- pcaCoDa(second_clust, method = "robust")
pca_classical_sc<- pcaCoDa(second_clust, method = "classical")

summary(pca_robust_sc)
summary(pca_classical_sc)

par(mfrow=c(1,2), mar = c(4,4,2,2))
biplot(pca_classical_sc, ylabs = 1:9 ,col=c("grey40","red"))
biplot(pca_robust_sc, ylabs = 1:9 ,col=c("grey40","red"))


# create vector image
pdf("figures/pca_cluster2_variables.pdf",width=16,height=8.35)
# Set up the plotting area
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
# Create the biplots
biplot(pca_classical_sc, ylabs = 1:9, col = c("grey40", "red"))
biplot(pca_robust_sc, ylabs = 1:9, col = c("grey40", "red"))
# Close the PDF device
dev.off()
