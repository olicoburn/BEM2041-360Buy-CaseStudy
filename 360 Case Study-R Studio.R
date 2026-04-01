# Importing the necessary libraries
library(tidyverse)
library(readxl)
library(factoextra)
library(cluster)
library(ggplot2)
library(gt)


# Reading the data
buy360 <- read_excel("C:/Users/olive/OneDrive/year 2/360 Case Study/360buy_SurveyData.xlsx")

# Inspecting the data
View(buy360)
summary(buy360)
colSums(is.na(buy360))

# Selecting the 5 preference variables 
buy360_vars <- buy360 %>%
  select(CusChoice, ConstUp, ReplacReminder, ProdReturn, ProInsuCov)

# Normalizing the variables
m <- apply(buy360_vars, 2, mean)
s <- apply(buy360_vars, 2, sd)
buy360_scaled <- scale(buy360_vars, m, s)

# Finding the optimal number of clusters through the Within Sum of Squares method
fviz_nbclust(buy360_scaled, kmeans, method = "wss") +
  labs(
    title    = "Optimal Number of Clusters",
    subtitle = "Based on K-Means Clustering using the Within Sum of Squares Method",
    x        = "Number of Clusters k",
    y        = "Total Within Sum of Square"
  ) +
  theme_minimal()

ggsave("elbow_plot.png", width = 8, height = 5, dpi = 150)


#  Dendrogram — Hierarchical Cluster Analysis

distance <- dist(buy360_scaled, method = "euclidean")
hc_buy360 <- hclust(distance, method = "ward.D2")
res <- hcut(buy360_scaled, k = 3, stand = TRUE)

# Visualise the dendrogram
fviz_dend(res, 
          rect     = TRUE, 
          cex      = 0.3,
          lwd      = 0.3,
          k_colors = c("#1a9641", "#2c7bb6", "#74c476"),
          main     = "360buy Customer Segmentation Dendrogram",
          xlab     = "Customer Observations",
          ylab     = "Height",
          sub      = "Based on Euclidean Distance and Ward.D2 Linkage Method",
          type     = "rectangle")
ggsave("dendrogram.png", width = 14, height = 7, dpi = 150)


#  Final K-Means Clustering Model
set.seed(123)
k <- kmeans(buy360_scaled, centers = 3, nstart = 25)
str(k)

k$size
buy360_final <- cbind(buy360, cluster = k$cluster)
buy360_final$cluster <- as.factor(buy360_final$cluster)

# Calculating segment proportions
proportions <- table(buy360_final$cluster) /
  length(buy360_final$cluster) * 100
round(proportions, 1)


fviz_nbclust(buy360_scaled, kmeans, method = "silhouette")


#  Segment Profile Tables
# Finding the mean, standard deviations and median of the segments

# MEAN VALUES
segment_means <- buy360_final %>%
  group_by(cluster) %>%
  summarise_all(mean)

segment_means <- round(segment_means[ , -1], digits = 2)
cluster <- c(1:3)
Final <- data.frame(cluster, segment_means)
Final

# Displaying as a formatted table
Final %>%
  gt() %>%
  tab_header(title = md("**Mean Values for 360buy Customer Clusters**"))


# STANDARD DEVIATION
segment_sd <- buy360_final %>%
  group_by(cluster) %>%
  summarise_all(sd)
segment_sd <- round(segment_sd[ , -1], digits = 2)

SD_Final <- data.frame(cluster, segment_sd)
SD_Final

# Displaying as a formatted table
SD_Final %>%
  gt() %>%
  tab_header(title = md("**Standard Deviation of 360buy Customer Clusters**"))

# MEDIAN VALUES
segment_medians <- buy360_final %>%
  group_by(cluster) %>%
  summarise_all(median)
segment_medians <- round(segment_medians[ , -1], digits = 2)

Median_Final <- data.frame(cluster, segment_medians)
Median_Final

# Displaying as a formatted table
Median_Final %>%
  gt() %>%
  tab_header(title = md("**Median Values for 360buy Customer Clusters**"))


# Pie Chart — Relative Size of 360buy Market Segments

# Define segment names
cluster_names <- c("Cluster 1: Autonomous Innovators",
                   "Cluster 2: Guided Loyalists",
                   "Cluster 3: Disengaged Minimalists")

# Segment proportions
percentages <- as.numeric(round(proportions, 1))

prop_data <- data.frame(
  clusterSizes = percentages,
  clusters     = cluster_names
)

pie_chart <- ggplot(prop_data, aes(x = "", y = clusterSizes, fill = clusters)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#1a9641", "#2c7bb6", "#74c476")) +
  geom_text(aes(label = paste0(round(percentages, 1), "%")),
            position = position_stack(vjust = 0.5),
            fontface = "bold",
            size     = 5,
            colour   = "white") +
  labs(title    = "Relative Size of 360buy Market Segments",
       subtitle = "Based on K-Means Clustering using Euclidean Distance and Ward.D2 Method",
       fill     = "Segments") +
  theme_minimal() +
  theme(
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text     = element_blank(),
    panel.grid    = element_blank(),
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.title  = element_text(face = "bold")
  )
print(pie_chart)


# Income Levels by Cluster
income_cluster <- buy360_final %>%
  group_by(LevIncome, cluster) %>%
  summarise(count = n(), .groups = "drop")


income_chart <- ggplot(income_cluster,
                       aes(x = as.factor(LevIncome), y = count, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust    = -0.5,
            size     = 3,
            fontface = "bold") +
  scale_fill_manual(values = c("#1a9641", "#2c7bb6", "#74c476"),
                    labels  = cluster_names) +
  scale_x_discrete(labels = c("1" = "Below £50k",
                              "2" = "£51-100k",
                              "3" = "£101-125k",
                              "4" = "£126-150k",
                              "5" = "Above £150k")) +
  labs(title    = "Income Levels by Cluster",
       subtitle = "Based on K-Means Clustering using Euclidean Distance and Ward.D2 Method",
       x        = "Household Income",
       y        = "Count",
       fill     = "Segments") +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    axis.text.x   = element_text(angle = 15, hjust = 1),
    legend.title  = element_text(face = "bold")
  )

# Display it
print(income_chart)


# Mean Cluster Characteristics with Standard Deviations

long_combined <- buy360_final %>%
  select(cluster, CusChoice, ConstUp, 
         ReplacReminder, ProInsuCov, ProdReturn) %>%
  pivot_longer(-cluster, 
               names_to  = "variable", 
               values_to = "value") %>%
  group_by(cluster, variable) %>%
  summarise(mean = mean(value),
            sd   = sd(value),
            .groups = "drop") %>%
  mutate(
    variable = recode(variable,
                      "CusChoice"      = "Platform Choice",
                      "ConstUp"        = "Constant Updates",
                      "ReplacReminder" = "Replace Reminder",
                      "ProInsuCov"     = "Insurance Cover",
                      "ProdReturn"     = "Product Return"),
    cluster = recode(cluster,
                     "1" = "Autonomous Innovators",
                     "2" = "Guided Loyalists",
                     "3" = "Disengaged Minimalists")
  )

# Plot
mean_chart <- ggplot(long_combined,
                     aes(x = cluster, y = mean, fill = variable)) +
  geom_bar(stat     = "identity",
           position = position_dodge(0.6),
           width    = 0.6) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position  = position_dodge(0.6),
                width     = 0.2,
                colour    = "black",
                linetype  = "dashed",
                linewidth = 0.4) +
  geom_text(aes(label = round(mean, 2)),
            position = position_dodge(0.6),
            vjust    = -1.8,
            size     = 2.5,
            fontface = "bold") +
  scale_fill_manual(values = c("#1a9641", "#2c7bb6", "#74c476",
                               "#a6d96a", "#4575b4")) +
  scale_y_continuous(breaks = seq(0, 7, by = 1),
                     limits = c(0, 8)) +
  labs(title    = "Mean Cluster Characteristics with Standard Deviations",
       subtitle = "Based on K-Means Clustering using Euclidean Distance and Ward.D2 Method",
       x        = "Segment",
       y        = "Mean Score (1-7)",
       fill     = "Variable") +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    axis.text.x   = element_text(angle = 10, hjust = 1, size = 9),
    legend.title  = element_text(face = "bold")
  )

print(mean_chart)

#  Account Ownership by Cluster

account_data <- buy360_final %>%
  group_by(cluster) %>%
  summarise(Pct_Account = round(mean(CusAcct) * 100, 1))

account_data$cluster <- recode(account_data$cluster,
                               "1" = "Autonomous Innovators",
                               "2" = "Guided Loyalists",
                               "3" = "Disengaged Minimalists")

account_chart <- ggplot(account_data, 
                        aes(x = cluster, y = Pct_Account, fill = cluster)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Pct_Account, "%")),
            vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#1a9641", "#2c7bb6", "#74c476")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title    = "360buy Account Ownership by Segment",
       subtitle = "Based on K-Means Clustering using Euclidean Distance and Ward.D2 Method",
       x        = "Segment",
       y        = "Percentage with Account (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(size = 9, colour = "grey40"))

print(account_chart)


