# packages installation
required_packages <- c("ggplot2", "readxl", "ggcorrplot", "MASS", "ade4", "factoextra", "ggrepel", "car", "pROC")
install_missing_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}
install_missing_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

# data loading
data <- read_excel("agrume1.xlsx")
summary(data)


##### 2. Analyses descripives #####

yield <- data$`rendement moyen par mu`
fruit_ratio <- data$`proportion du rendement des fruits commerciaux`

# 2.1.1 rendement moyen par mu distribution
ggplot(data, aes(x = yield)) +
  geom_histogram(binwidth = 300, fill = "skyblue", color = "black") +
  ggtitle("Distribution Du Rendement Moyen Par Mu") +
  xlab("Rendement moyen par mu") +
  ylab("fréquence") +
  theme_minimal()

# 2.1.2 rendement moyen par an
ggplot(data, aes(x = yield, fill = as.factor(année))) +
  geom_density(alpha = 0.5) +
  ggtitle("Rendement Moyen Par Mu Pour Différentes Années") +
  xlab("Rendement moyen par mu") +
  ylab("densité") +
  labs(fill = "année") +
  theme_minimal()

# Statistiques descriptives
summary_stats <- data.frame(
  Mean = mean(yield, na.rm = TRUE),
  Median = median(yield, na.rm = TRUE),
  SD = sd(yield, na.rm = TRUE),
  Quantiles = quantile(yield, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
  Variance = var(yield, na.rm = TRUE)
)
print(summary_stats)

# 2.2 proportion du rendement des fruits commerciaux
fruit_ratio <- data$`proportion du rendement des fruits commerciaux`

# distribution
ggplot(data, aes(x = fruit_ratio)) +
  geom_histogram(binwidth = 0.01, fill = "lightgreen", color = "black") +
  ggtitle("Distribution du Proportion du Rendement des Fruits Commerciaux") +
  xlab("Proportion des fruits commerciaux") +
  ylab("frequence") +
  theme_minimal()

# statistique descriptive
mean_value <- mean(fruit_ratio, na.rm = TRUE)
median_value <- median(fruit_ratio, na.rm = TRUE)
sd_value <- sd(fruit_ratio, na.rm = TRUE)
variance_value <- var(fruit_ratio, na.rm = TRUE)
quantiles <- quantile(fruit_ratio, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
range_values <- range(fruit_ratio, na.rm = TRUE)

fruit_ratio_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Variance", "Q1", "Q2 (Median)", "Q3", "Min", "Max"),
  Value = c(
    mean_value,
    median_value,
    sd_value,
    variance_value,
    quantiles[1],
    quantiles[2],
    quantiles[3],
    range_values[1],
    range_values[2]
  )
)
print(fruit_ratio_stats)

# 2.3 Analyse des Corrélations
# retirer les variables qui n'ont pas de sens
filtered_data <- data[, !colnames(data) %in% c(
  "rendement total (kilogrammes)",
  "superficie (mus)",
  "rendement des fruits commerciaux (kilogrammes)",
  "rendement des fruits de seconde qualité (kilogrammes)",
  "densité de plantation (plants par mu)"
)]

# Extraire les variables sous-jacentes 
selected_columns <- c(
  "quantité annuelle de consommation de pesticides (litres)",
  "consommation moyenne de pesticides par mu",
  "volume d'irrigation (mètres cubes)",
  "volume moyen d'irrigation par mu",
  "quantité d'engrais utilisée (kilogrammes)",
  "quantité moyenne d'engrais utilisée par mu (kilogrammes)",
  "quantité d'engrais organique utilisée (kilogrammes)",
  "quantité moyenne d'engrais organique utilisée par mu (kilogrammes)"
)

filtered_data <- data[selected_columns]

# Renommer les colonnes pour des noms simplifiés
colnames(filtered_data) <- paste0("Variable ", seq_along(selected_columns))

# Calculer la matrice de corrélation
cor_matrix <- cor(filtered_data, use = "complete.obs")

# Carte thermique du coefficient de corrélation
ggcorrplot(
  cor_matrix,
  hc.order = FALSE,  
  type = "full",     
  lab = TRUE,        
  lab_size = 2,     
  colors = c("blue", "white", "red"), 
  title = "Symmetric Correlation Matrix for Selected Variables",
  tl.cex = 9,       
  tl.srt = 90       
)


# 2.4  Tests d’Hypothèses Complémentaires
# `utilisation de la technique de plantation en basse tige ?   (0 non, 1 oui)` -> `rendement moyen par mu`
ggplot(data, aes(x = as.factor(`utilisation de la technique de plantation en basse tige ?   (0 non, 1 oui)`), y = yield)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Impact de la Technique de Plantation sur le Rendement Moyen Par Mu") +
  xlab("Utilisation de la technique (0 = non, 1 = oui)") +
  ylab("Rendement moyen par mu") +
  theme_minimal()
dev.off()

t_test_result <- t.test(
  yield ~ data$`utilisation de la technique de plantation en basse tige ?   (0 non, 1 oui)`,
  alternative = "two.sided"
)
print(t_test_result)

# `utilisation de la technique de plantation en basse tige ?   (0 non, 1 oui)` -> `proportion du rendement des fruits commerciaux`
t_test_result2 <- t.test(
  data$`proportion du rendement des fruits commerciaux` ~ data$`utilisation de la technique de plantation en basse tige ?   (0 non, 1 oui)`,
  alternative = "two.sided"
)
print(t_test_result2)


##### 3. Amélioration de la Normalité des Données #####

# 3.1 Transformation Box-Cox
# Filtrer les données
data_filtered <- subset(data, `rendement moyen par mu` > 2000 & année != 2016)
y <- data_filtered$`rendement moyen par mu`

# Transformation de Box-Cox
boxcox_result <- boxcox(lm(y ~ 1), lambda = seq(-2, 2, by = 0.1))

# Obtenir le lambda optimal
lambda_opt <- boxcox_result$x[which.max(boxcox_result$y)]
y_transformed <- ((y^lambda_opt) - 1) / lambda_opt

# Configurer une disposition de 2 lignes et 2 colonnes pour les graphiques
par(mfrow = c(2, 2))

# Tracer le Q-Q plot des données originales
qqnorm(y, main = "Avant Box-Cox (Q-Q Plot)")
qqline(y)

# Tracer le Q-Q plot des données transformées par Box-Cox
qqnorm(y_transformed, main = "Après Box-Cox (Q-Q Plot)")
qqline(y_transformed)

# Tracer l'histogramme des données originales
hist(y, breaks = 20, col = "lightblue", border = "black",
     main = "Distribution des données originales",
     xlab = "Données originales", probability = TRUE)
lines(density(y), col = "blue", lwd = 2)  # Ajouter une courbe de densité

# Tracer l'histogramme des données transformées par Box-Cox
hist(y_transformed, breaks = 20, col = "lightgreen", border = "black",
     main = "Distribution des données transformées (Box-Cox)",
     xlab = "Données transformées (Box-Cox)", probability = TRUE)
lines(density(y_transformed), col = "darkgreen", lwd = 2)  # Ajouter une courbe de densité


# Effectuer le test de Shapiro-Wilk pour vérifier la normalité
shapiro_original <- shapiro.test(y)  # Test pour les données originales
shapiro_transformed <- shapiro.test(y_transformed)  # Test pour les données transformées

# Afficher les résultats des tests
cat("Résultat du test de Shapiro-Wilk pour les données originales:\n")
print(shapiro_original)

cat("\nRésultat du test de Shapiro-Wilk pour les données transformées:\n")
print(shapiro_transformed)

#### 4. Analyse en Composantes Principales ####
# scaling
numeric_columns <- sapply(data_filtered, is.numeric)
X <- data_filtered[, numeric_columns]
X_scaled <- scale(X)

# la matrice de covariance, eigenvalues
# examiner s'il existe "NA" ou "infty" 
any(is.na(X_scaled))     # NA
any(is.infinite(X_scaled)) # infty

# remplacer NA par la moyenne
X_scaled[is.na(X_scaled)] <- mean(X_scaled, na.rm = TRUE)

cov_matrix <- cov(X_scaled)
pca_results <- eigen(cov_matrix)
pca_results$values

# Scree plot
barplot(pca_results$values, main = "Scree Plot", xlab = "Principal Components", ylab = "Eigenvalues")
abline(h = mean(pca_results$values), col = "red", lty = 2)  # critère de Kaiser

# Proportion cumulative de la variance.
explained_variance <- pca_results$values / sum(pca_results$values)
cumulative_variance <- cumsum(explained_variance)
print(cumulative_variance)


ACP <- dudi.pca(X_scaled, scannf = FALSE, nf = 5)
# Scree Plot
fviz_eig(ACP, addlabels = TRUE, ylim = c(0, 50))
pca_var <- get_pca_var(ACP)
variable_numbers <- 1:nrow(pca_var$coord)

fviz_pca_var(
  ACP,
  col.var = "contrib",
  repel = FALSE,    
  label = "none"    
) +
  geom_text_repel(
    aes(
      x = pca_var$coord[, 1],
      y = pca_var$coord[, 2],
      label = variable_numbers
    ),
    size = 4,       
    box.padding = 0.3,  
    point.padding = 0.2,
    segment.color = "gray" 
  )

# Dim 1 2
fviz_pca_ind(
  ACP,
  col.ind = "cos2",  
  label = "none",    
  geom = "point"       
) +
  theme_classic() +  
  xlab("Dim 1 (Principal Component 1)") +
  ylab("Dim 2 (Principal Component 2)") +
  theme(
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Dim 3 4
fviz_pca_ind(
  ACP,
  axes = c(3, 4),    
  col.ind = "cos2",   
  label = "none",   
  geom = "point"     
) +
  theme_classic() +   
  xlab("Dim 3 (Principal Component 3)") +
  ylab("Dim 4 (Principal Component 4)") +
  theme(
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )


contributions <- pca_var$contrib[, 1:5]  # Contribution
print(contributions)


for (i in 1:5) {
  cat(paste("Composante principale", i, "main contribution：\n"))
  top_vars <- names(sort(contributions[, i], decreasing = TRUE)[1:5])  
  print(top_vars)
  cat("\n")
}


#### 5. Modélisation par Régression Linéaire Multiple ####

# Preprocess the data
data_regression <- data_filtered[, !names(data_filtered) %in% c(
  "rendement des fruits commerciaux (kilogrammes)", 
  "rendement des fruits de seconde qualité (kilogrammes)",
  "rendement total (kilogrammes)",
  "densité de plantation (plants par mu)",
  "proportion du rendement des fruits commerciaux"
)]

initial_model <- lm(`rendement moyen par mu` ~ ., data = data_regression)

# La transformée de Box-Cox
boxcox_results <- boxcox(initial_model, lambda = seq(-2, 2, 0.1), plotit = FALSE)  
lambda_optimal <- boxcox_results$x[which.max(boxcox_results$y)]
cat("lambda optimal pour Box-Cox:", lambda_optimal, "\n")

if (lambda_optimal == 0) {
  data_regression$Y_transformed <- log(data_regression$`rendement moyen par mu`)
} else {
  data_regression$Y_transformed <- (data_regression$`rendement moyen par mu`^lambda_optimal - 1) / lambda_optimal
}

data_regression <- data_regression[, !names(data_regression) %in% c("rendement moyen par mu")]

data_regression$année <- as.factor(data_regression$année)
data_regression$`type de verger` <- as.factor(data_regression$`type de verger`)
data_regression$année <- as.numeric(as.character(data_regression$année))

# Split the preprocessed data into training and testing sets
set.seed(42)  # Set a seed for reproducibility
train_indices <- sample(1:nrow(data_regression), size = 0.7 * nrow(data_regression))
train_data <- data_regression[train_indices, ]
test_data <- data_regression[-train_indices, ]

# Construire le modèle de régression linéaire multiple sur le training set
full_model_train <- lm(Y_transformed ~ ., data = train_data)

summary(full_model_train)

# Step-wise pour sélectionner le meilleur modèle sur le training set
best_model_train <- step(full_model_train, direction = "both")
summary(best_model_train)

# Utiliser ANOVA pour tester si best_model diffère significativement avec full_model (training set)
anova(full_model_train, best_model_train)

# Examiner la colinéarité (training set)
vif(best_model_train)

# Visualiser les résidus (training set)
par(mfrow = c(2, 2))
plot(best_model_train)

# Calculate performance metrics for the training set
predicted_train <- predict(best_model_train, newdata = train_data)
# Calculate R-squared for the training set
tss_train <- sum((train_data$Y_transformed - mean(train_data$Y_transformed))^2)
ssr_train <- sum((train_data$Y_transformed - predicted_train)^2)
r_squared_train <- 1 - (ssr_train / tss_train)
cat("Training Set R-squared:", r_squared_train, "
")

# Test the model on the testing set
predicted_test <- predict(best_model_train, newdata = test_data)
predicted_test[is.na(predicted_test)] <- mean(predicted_test, na.rm = TRUE)
# Calculate R-squared for the testing set
tss_test <- sum((test_data$Y_transformed - mean(test_data$Y_transformed))^2)
ssr_test <- sum((test_data$Y_transformed - predicted_test)^2)
r_squared_test <- 1 - (ssr_test / tss_test)
cat("Test Set R-squared:", r_squared_test, "
")

# Calculate R-squared for full_model on the training set
predicted_full_train <- predict(full_model_train, newdata = train_data)
tss_full_train <- sum((train_data$Y_transformed - mean(train_data$Y_transformed))^2)
ssr_full_train <- sum((train_data$Y_transformed - predicted_full_train)^2)
r_squared_full_train <- 1 - (ssr_full_train / tss_full_train)
cat("Training Set R-squared for Full Model:", r_squared_full_train, "
")

# Calculate R-squared for full_model on the testing set
predicted_full_test <- predict(full_model_train, newdata = test_data)
predicted_full_test[is.na(predicted_full_test)] <- mean(predicted_full_test, na.rm = TRUE)
tss_full_test <- sum((na.omit(test_data$Y_transformed) - mean(na.omit(test_data$Y_transformed)))^2)
ssr_full_test <- sum((na.omit(test_data$Y_transformed) - predicted_full_test)^2)
r_squared_full_test <- 1 - (ssr_full_test / tss_full_test)
cat("Test Set R-squared for Full Model:", r_squared_full_test, "
")


#### 6. Analyse par Régression Logistique ####
# Pre-processing the data
data$`qualité de production` <- ifelse(data$`proportion du rendement des fruits commerciaux` > 0.9, "élevée", "faible")
data$`qualité de production` <- as.factor(data$`qualité de production`)
summary(train_data$`qualité de production`)

data_logistic <- subset(data, select = -c(`proportion du rendement des fruits commerciaux`, 
                                          `rendement moyen par mu`, 
                                          `rendement total (kilogrammes)`,
                                          `rendement des fruits commerciaux (kilogrammes)`,
                                          `rendement des fruits de seconde qualité (kilogrammes)`,
                                          `densité de plantation (plants par mu)`))

# Handle missing values
data_logistic$`espacement de plantation (mètres)`[is.na(data_logistic$`espacement de plantation (mètres)`)] <- 3.5

data_logistic$`type de verger` <- as.factor(data_logistic$`type de verger`)
data_logistic$année <- as.numeric(as.character(data_logistic$année))

# Split the data into training and testing sets
set.seed(42)  # Set a seed for reproducibility
train_indices <- sample(1:nrow(data_logistic), size = 0.7 * nrow(data_logistic))
train_data <- data_logistic[train_indices, ]
test_data <- data_logistic[-train_indices, ]

# Build the logistic regression model on the training set
logistic_model <- glm(`qualité de production` ~ ., data = train_data, family = binomial)

# Perform step-wise variable selection
optimized_model <- step(logistic_model, direction = "both")

anova(optimized_model, logistic_model)

# Evaluate the optimized model on the training set
train_predicted_probs <- predict(optimized_model, newdata = train_data, type = "response")
train_predicted_classes <- ifelse(train_predicted_probs >= 0.5, "faible", "élevée")

# Confusion matrix and accuracy for the training set
train_confusion_matrix <- table(Predicted = train_predicted_classes, Actual = train_data$`qualité de production`)
train_accuracy <- mean(train_predicted_classes == train_data$`qualité de production`)

# Print results for training set
cat("\nConfusion Matrix for Training Set:\n")
print(train_confusion_matrix)
cat("Training Set Accuracy:", train_accuracy, "\n")


# Evaluate the optimized model on the test set
predicted_probs <- predict(optimized_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs >= 0.5, "faible", "élevée")

# Confusion matrix and accuracy
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$`qualité de production`)
accuracy <- mean(predicted_classes == test_data$`qualité de production`)


# Print results
print(confusion_matrix)
cat("Test Set Accuracy:", accuracy, "\n")

# Check multicollinearity in the optimized model
vif_values <- vif(optimized_model)
print(vif_values)

roc_curve <- roc(test_data$`qualité de production`, predicted_probs, levels = c("faible", "élevée"))
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
cat("AUC:", auc(roc_curve), "\n")