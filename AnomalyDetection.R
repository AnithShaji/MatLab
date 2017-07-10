
# Anomaly detection
# install.packages("pacman")
# Install packages for Anomaly detection
pacman::p_load(ggplot2, grid, gridExtra, robustbase)
help("grid.arrange")

# Load Data 
# Read data of endangered plant
data = read.csv("EndangeredSpecies.csv")   # Load data

# Check the stracture
str(data)

# Transform variables to factors
data$Region = as.factor(data$Region)

# Univariate outliers

# Boxplot for each variable separately
# Rafflesia
u1 <- qplot(data = data, y = Rafflesia, x = 1, 
            geom = "boxplot", outlier.color = "#FFCA28",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Rafflesia") +
  geom_text(aes(label = ifelse(Rafflesia %in%
                                 boxplot.stats(Rafflesia)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u1

# Coneflower
u2 <- qplot(data = data, y = Coneflower, x = 1, 
            geom = "boxplot", outlier.color = "#DB4437",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Coneflower") +
  geom_text(aes(label = ifelse(Coneflower %in%
                                 boxplot.stats(Coneflower)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u2

# Purple-loosestrife
u3 <- qplot(data = data, y = Purple.loosestrife, x = 1, 
            geom = "boxplot", outlier.color = "#9C27B0",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Purple-loosestrife") +
  geom_text(aes(label = ifelse(Purple.loosestrife %in%
                                 boxplot.stats(Purple.loosestrife)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u3

# Norway maple
u4 <- qplot(data = data, y = Norway.maple, x = 1, 
            geom = "boxplot", outlier.color = "#2196F3",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Norway maple") +
  geom_text(aes(label = ifelse(Norway.maple %in%
                                 boxplot.stats(Norway.maple)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u4

# Common ivy
u5 <- qplot(data = data, y = Common.ivy, x = 1, 
            geom = "boxplot", outlier.color = "#399540",
            xlim = c(0, 2), xlab = NULL, ylab = NULL,
            main = "Common ivy") +
  geom_text(aes(label = ifelse(Common.ivy %in%
                                 boxplot.stats(Common.ivy)$out, 
                               as.character(state.name), "")), hjust = 1.5)
u5


# Plot boxplots together
grid.arrange(u1, u2, u3, u4, u5, nrow = 2,
             top = "Boxplots: Univariate outliers")


# Bivariate outliers
b1 <- qplot(data = data, x = Rafflesia, y = Common.ivy,
            main = "Rafflesia vs. Common ivy") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((Rafflesia > 0 |Common.ivy < 100),
                               as.character(state.name), "")), hjust = 1.5)
b1

b2 <- qplot(data = data, x = Purple.loosestrife, y = Norway.maple,
            main = "Purple-loosestrife vs. Norway.maple") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((Purple.loosestrife > 0 | Norway.maple <20),
                               as.character(state.name), "")), hjust = 1.5)
b2

b3 <- qplot(data = data, x = Norway.maple, y = Coneflower,
            main = "Norway maple vs. Coneflower ") +
  stat_ellipse(level = .99, color = "#E38040") +
  geom_text(aes(label = ifelse((Norway.maple > 1| Coneflower <100),
                               as.character(state.name), "")), hjust = 1.5)
b3

# Plot together
grid.arrange(b1, b2, b3, nrow = 1, top = "Bivariate outliers")



# Clean up 
rm(list = ls())


