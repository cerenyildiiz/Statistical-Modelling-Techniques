# Step 1: Load and inspect the data.
data(mtcars)
head(mtcars)
str(mtcars)
summary(mtcars)


$$
  \begin{aligned}
Y_i &= \beta_0 + \beta_1 X_{i1} + \cdots + \beta_k X_{ik} + \varepsilon_i, \\
\varepsilon_i &\sim \mathcal{N}(0,\sigma^2), \quad i=1,\ldots,n.
\end{aligned}
,\varepsilon -> error \ term
$$
  
# Step 2: Check for missing or duplicate observations.
 any(is.na(mtcars))   
any(duplicated(mtcars))
#No missing or duplicated observations were found.
## Model selection and justification
#Before building the full model, let???s explore correlations and select meaningful predictors.
cor(mtcars[, sapply(mtcars, is.numeric)])
# Observations:**
  
 # `mpg` has a strong negative correlation with `wt` (???0.87) and `hp` (???0.78).

# `disp` and `wt` are also correlated, meaning one of them might be dropped to avoid multicollinearity.

#We select the following variables for the model:
  
 # -   **wt (Weight)** ??? heavier cars consume more fuel.

#-   **hp (Horsepower)** ??? higher horsepower reduces efficiency.

#-   **am (Transmission)** ??? manual vs automatic transmission

#-   **disp (Displacement)** ??? optional, measures engine size.

#These predictors are justified based on mechanical and statistical reasoning.
library(ggplot2)
mtcars2 <- transform(mtcars, am = factor(am, labels = c("Automatic","Manual")))

# Scatter plot: MPG vs Weight
ggplot(mtcars2, aes(wt, mpg, color = am)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  labs(title = "Fuel Efficiency vs Vehicle Weight",
       x = "Weight (1000 lbs)", y = "MPG", color = "Transmission") +
  theme_minimal(base_size = 13)

#  Kernal Density Plot: MPG by transmission 
ggplot(mtcars2, aes(mpg, fill = am)) +
  geom_density(alpha = 0.5, color = "black") +
  labs(title = "MPG Density by Transmission Type",
       x = "Miles per Gallon (MPG)", y = "Density", fill = "Transmission") +
  theme_minimal(base_size = 13)

# **Model Fitting in R**
data(mtcars)
model_mtc <- lm(mpg ~ wt + hp + disp + factor(am), data = mtcars)
summary(model_mtc)

### **Interpretation of the Results**
|# Predictor | Interpretation | Significance |
# |------------------------|------------------------|------------------------|
# | Intercept (34.00) | Estimated mpg when other predictors = 0 | Baseline |
# | wt (-2.88) | For every 1000 lb increase, mpg decreases by \~2.88 | \*\*\* Significant |
# | hp (-0.038) | Each unit increase in horsepower decreases mpg by \~0.038 | \*\* Significant | # | disp (-0.018) | Engine displacement has a small negative effect | Not significant |
# | amManual (+2.08) | Manual cars have \~2 mpg higher efficiency than automatic | \* Significant |
  
  
#  **Model fit:**
  
 # R?? = 0.84, Adj. R?? = 0.81 ??? The model explains about 81% of the variation in mpg.\
# F-test p \< 0.001 ??? The model is significant.\

# **Model Diagnostics and Evaluation**
  
#  a)  Residual Diagnostics
  par(mfrow=c(2,2))
plot(model_mtc)
par(mfrow=c(1,1))


#**Interpretation:**
  
 # -   Residuals are randomly scattered around zero (linearity OK).

#-   Normal Q-Q plot ??? straight line (normality OK).

#-   Scale-location: variance roughly constant (homoskedasticity OK).

#-   No influential outliers (Cook???s distance \< 0.5).
install.packages("car")
library(car)
vif(model_mtc)


#-   VIF \> 10 ??? severe multicollinearity

#-   5 \< VIF \< 10 ??? possible multicollinearity

#-   VIF \< 5 ??? no multicollinearity

#c)  Simplified model
model_best <- lm(mpg ~ wt + hp + am, data = mtcars)
summary(model_best)


#Simpler model, slightly higher explanatory power.
anova(model_best, model_mtc)
#The difference is not statistically significant ??? simpler model is preferred.
# **Residual Analysis**
res_std <- rstandard(model_best)
par(mfrow = c(1,1), mar = c(4.5, 4.5, 3, 1))
qqnorm(res_std,
       main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 1,
       cex = 1.1)
qqline(res_std, col = "red", lwd = 2)

# Residuals vs Fitted

par(mfrow = c(1,2))
plot(model_best$fitted.values,
     residuals(model_best),
     pch = 19,
     col = "darkblue",
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)

# Normal Q-Q Plot

res_std <- rstandard(model_best)
qqnorm(res_std,
       main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles",
       ylab = "Observed Quantiles",
       pch = 1, cex = 1.1)
qqline(res_std, col = "red", lwd = 2)
par(mfrow = c(1,1))

# Residual density 

res <- resid(model_best)
dens <- density(res)
plot(dens,
     main = "Density of Residuals",
     xlab = "Residuals",
     ylab = "Density",
     lwd  = 2)
abline(v = 0, col = "red", lwd = 2)
rug(res, col = "gray40")
sigma_hat <- summary(model_best)$sigma
x <- seq(min(res), max(res), length.out = 400)
lines(x, dnorm(x, mean = 0, sd = sigma_hat),
      col = "steelblue", lwd = 2, lty = 2)
legend("topright",
       legend = c("Kernel Density", "N(0, ??????)"),
       lwd = 2, col = c("black", "steelblue"),
       lty = c(1, 2), bty = "n")

# If the density curve is symmetrical and concentrated around 0, the residuals are approximately symmetric and the fit with the normal curve is good. If the right or left tail is prominent, or there are double peaks, it indicates a deviation from normality and should be further evaluated using a Q-Q plot and the Shapiro???Wilk test.

res <- resid(model_best)
shapiro.test(res)

# **Hypotheses:**
  
#  -   H???: residuals are normally distributed.(Null)

# -   H???: residuals are not normally distributed.(Alternative)

# **Decision:**\
# p-value = 0.4142 \> 0.05 ??? H??? not rejected.

# **Conclusion:**\
# Residuals are approximately normal.\
# This conclusion aligns with Q???Q plot and density plot visual checks.

# In this case, the assumption that the error terms are normally distributed is met. When evaluated together with the Q???Q plot and the density plot, it can be said that the model residuals are quite consistent with a normal distribution.

# **Outlier Detection**
model <- lm(mpg ~ wt + hp + am + disp, data = mtcars)

summary(model)




# **1. Standardized Residuals (Model-based Outliers)**
# Compute standardized residuals
std_resid <- rstandard(model)

# Identify observations with |residual| > 3
out_std <- which(abs(std_resid) > 3)
print(out_std)

if(length(out_std) == 0){
  cat("No outliers were found based on standardized residuals (|r| > 3).\n\n")
} else {
  cat("Observations with |r| > 3 (potential outliers):", out_std, "\n\n")
}



#Interpretation: Standardized residuals greater than ??3 indicate points where the model???s prediction error is unusually large. If no such points exist, it means the regression model fits all cases reasonably well in terms of residual behavior.
# **2. IQR Rule (Univariate Outlier Detection)**
# Select numeric variables only.
num_cols <- mtcars[, sapply(mtcars, is.numeric)]

# Compute Q1, Q3, and IQR for each variable.
Q1  <- apply(num_cols, 2, quantile, probs = 0.25)
Q3  <- apply(num_cols, 2, quantile, probs = 0.75)
IQRv <- Q3 - Q1

# Identify outliers beyond 1.5 * IQR
out_iqr <- lapply(names(num_cols), function(col){
  x <- num_cols[[col]]
  which(x < (Q1[col] - 1.5 * IQRv[col]) | x > (Q3[col] + 1.5 * IQRv[col]))
})
names(out_iqr) <- names(num_cols)
print(out_iqr)


# **3. Mahalanobis Distance (Multivariate Outlier Detection)**
# Predictor matrix (exclude dependent variable).
X <- model.matrix(model)[ , -1, drop = FALSE]  # remove intercept

# Compute Mahalanobis distances.
D2 <- mahalanobis(X, center = colMeans(X), cov = cov(X))

# Critical chi-square cutoffs.
k <- ncol(X)
cut95 <- qchisq(0.95, df = k)
cut99 <- qchisq(0.99, df = k)

cat("95% cutoff:", round(cut95, 2), "\n99% cutoff:", round(cut99, 2), "\n")

# Identify multivariate outliers.
out_md_95 <- which(D2 > cut95)
out_md_99 <- which(D2 > cut99)

cat("95% threshold outliers:", out_md_95, "\n")
cat("99% threshold outliers:", out_md_99, "\n")
Mahalanobis distance measures how far each observation lies from the
multivariate center of the predictors.

#Points above the 95% cutoff are possible multivariate outliers.

#Points above the 99% cutoff are strong outliers.

#(Optional) Visualization
# Plot Mahalanobis distances.
plot(D2, pch = 19, col = "steelblue",
     main = "Mahalanobis Distance for mtcars",
     xlab = "Observation Index", ylab = expression(D^2))
abline(h = cut95, col = "orange", lty = 2, lwd = 2)
abline(h = cut99, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("95% cutoff", "99% cutoff"),
       col = c("orange", "red"), lty = 2, bty = "n")
### **MSE (Mean Squared Error)**

#1.  **Definition:**\

#-   MSE is the mean of squared residuals.Smaller values indicate better fit.

#-   Large errors are penalized more since they are squared.

#-   The unit is the square of the dependent variable (e.g., mpg??).
model1 <- lm(mpg ~ wt + hp, data = mtcars)
model2 <- lm(mpg ~ wt + hp + am + disp, data = mtcars)


pred1 <- predict(model1)
pred2 <- predict(model2)

mse1 <- mean((mtcars$mpg - pred1)^2)
mse2 <- mean((mtcars$mpg - pred2)^2)

rmse1 <- sqrt(mse1)
rmse2 <- sqrt(mse2)


# AIC and BIC (Model comparison)
# Model set used in AIC/BIC comparison
m0 <- lm(mpg ~ 1, data = mtcars)  # intercept-only
m1 <- lm(mpg ~ wt + hp, data = mtcars)
m2 <- lm(mpg ~ wt + hp + factor(am), data = mtcars)
mF <- lm(mpg ~ wt + hp + factor(am) + disp, data = mtcars)

AIC(m0, m1, m2, mF)
BIC(m0, m1, m2, mF)

# **Rule: Smaller AIC/BIC values indicate a better balance between model complexity and fit. Helps avoid overfitting by penalizing unnecessary variables.**
  
 # RMSE is lower, which means that **model2** performs slightly better in terms of prediction; however, since the Adjusted R?? value remains the same, adding additional variables to model2 does not provide a meaningful improvement, so according to the **parsimony principle**, the simpler model (model1) is preferred ??? RMSE, being the square root of MSE, expresses the average prediction error in the same units as the dependent variable and is therefore easier to interpret, showing how many units of error the model makes on average; moreover, model evaluation should also consider **AIC** and **BIC**, where smaller values indicate a better model because these criteria account for both the amount of error and the model???s complexity, thereby helping to prevent **overfitting**.

#[**NOTE:**]{.underline} I realize that I'm using very simple data. My goal here is to establish a basis with the help of my notes from last year's regression analysis course.

# ***Some necessary information***
  
# |     |                                              |     |
# |-----|----------------------------------------------|-----|
# |     |                                              |     |
# |     | **VIF\>10, Severe multicollinearity exists** |     |
# |     |                                              |     |
# |     | **10\>VIF\>5, Possible multicollinearity**   |     |
# |     |                                              |     |
# |     | **5\>VIF, No multicollinearity**             |     |
# |     |                                              |     |
  
# -   In multiple linear regression, several assumptions must be satisfied to ensure that the model provides valid and reliable results.

# a)  **Linearity**
  
#  b)  **Homoscedasticity**
  
 # c)  **Normality**
  
  #d)  **No multicollinearity etc.**


