# install.packages(c("tidyverse","broom","ggpubr","lmtest","car"))
library(tidyverse)
library(broom)
library(ggpubr)
library(lmtest)   # bptest, dwtest

# Load dataset -----------------------------------------------------------
auto <- read_csv("auto-mpg.csv")  # file from Kaggle
names(auto)

df <- auto %>%
  dplyr::select(mpg, weight) %>%
  dplyr::filter(is.finite(mpg), is.finite(weight))

## try with and without log2
df$mpg <- log2(df$mpg)

# Exploratory plots ------------------------------------------------------

# Scatter
p_scatter_loess <- ggplot(df, aes(weight, mpg )) +
  geom_point(alpha=0.5) +
  labs(title="EDA: Car Weight vs Fuel Efficiency (mpg)",
       x="Car Weight (lbs)", y="Miles per Gallon") +
  theme_minimal()

# Histograms
p_hist_weight <- ggplot(df, aes(weight)) +
  geom_histogram(bins=20, fill="steelblue", color="white") +
  labs(title="Distribution of Car Weight") +
  theme_minimal()

p_hist_mpg <- ggplot(df, aes( mpg )) +
  geom_histogram(bins=20, fill="darkorange", color="white") +
  labs(title="Distribution of MPG") +
  theme_minimal()

# Correlation
pearson  <- cor(df$weight, df$mpg, method="pearson")
spearman <- cor(df$weight, df$mpg, method="spearman")

p_scatter_lm <- ggplot(df, aes(weight, mpg )) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=FALSE, color="red", linetype="dashed") +
  annotate("text", x=Inf, y=Inf, hjust=1.1, vjust=1.5,
           label=sprintf("Pearson r = %.2f\nSpearman ρ = %.2f", pearson, spearman)) +
  labs(title="Correlation Overlay (dashed = linear fit)",
       x="Car Weight (lbs)", y="Miles per Gallon") +
  theme_minimal()

ggarrange(p_scatter_loess, p_scatter_lm, p_hist_weight, p_hist_mpg,
          ncol=2, nrow=2)


# Fit linear model -------------------------------------------------------
m <- lm(mpg ~ weight, data=df)
summary(m)


alpha <- summary(m)$coefficients[1,1]
beta <- summary(m)$coefficients[1,2]

message(alpha, " + ", beta, " X", " + error")

df_mod <- augment(m)
## explore df_mod

# Explained vs unexplained -----------------------------------------------
set.seed(1)
samp <- df_mod %>% slice_sample(n=80)

ggplot(samp, aes(weight, mpg)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=FALSE, color="red") +
  geom_segment(aes(xend=weight, y=.fitted, yend=mpg),
               color="gray40", alpha=0.7) +
  labs(title="Explained (ŷ = Xβ) + Unexplained (ε)",
       subtitle="Red = regression line, Gray = residuals",
       x="Car Weight (lbs)", y="Miles per Gallon") +
  theme_minimal()


# Assumption checks ------------------------------------------------------

# 3a) Residuals vs Fitted (linearity + homoscedasticity)
ggplot(df_mod, aes(.fitted, .resid)) +
  geom_hline(yintercept=0, linetype=2, color="red") +
  geom_point(alpha=0.6) +
  labs(title="Residuals vs Fitted",
       x="Fitted values (ŷ)", y="Residuals (ε)") +
  theme_minimal()

bptest(m)   # homoscedasticity (expect p > 0.05)


set.seed(123)
shapiro.test(sample(residuals(m), size = min(5000, nrow(df)))) # normality



