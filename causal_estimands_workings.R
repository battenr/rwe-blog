df %>% view()
sample(c(1,2,3), size = 10, replace = TRUE)

?sample

# ATE 

ps.mod <- glm(sprayed ~ eyes + group, 
              family = binomial(link = "logit"), 
              data = df)

mean(df.ps$weight_atu)

df.ps <- df %>% 
  dplyr::mutate(
    ps = predict(ps.mod, type = "response"), 
    weight_ate = dplyr::case_when(
      sprayed == 1 ~ 1/ps, 
      sprayed == 0 ~ 1/(1-ps)
    ),
    weight_att = dplyr::case_when(
      sprayed == 1 ~ 1, 
      sprayed == 0 ~ ps/(1-ps)
    ),
    weight_atu = dplyr::case_when(
      sprayed == 1 ~ (1-ps)/ps,
      sprayed == 0 ~ 1
    )
  )

outcome.mod <- glm(height ~ sprayed,
                   family = gaussian(link = "identity"),
                   weights = weight_ate, 
                   data = df.ps)

coef(outcome.mod)[2]

# Load required library
library(boot)

outcome_model <- function(df, indices){
  df <- df[indices, ]
  
  fit <- glm(height ~ sprayed, data = df, family = gaussian(link = "identity"))
  return(coef(fit)[2])
}

set.seed(1234)
results <- boot(data = df.ps, statistic = outcome_model(df.ps), R = 100)

library(boot)

outcome_model <- function(df, indices) {
  df <- df[indices,]
  fit <- glm(height ~ sprayed, data = df, family = gaussian(link = "identity"))
  return(coef(fit)[2])
}

set.seed(1234)
results <- boot(data = df.ps, statistic = outcome_model, R = 100)

library(sandwich)

sandwich_se <- diag(sandwich::vcovHC(mod.outcome, type = "HC"))^0.5

results

?boot

results$

boot.ci(results)


boot::boo

# Define our data
data <- mtcars

# Define our model function
logistic_model <- function(data, indices) {
  # Fit the model
  fit <- glm(am ~ mpg, data = data[indices,], family = binomial)
  return(coef(fit))
}

# Perform bootstrapping
set.seed(1234)  # For reproducibility
results <- boot(data = data, statistic = logistic_model, R = 1000)

# Print results
print(results)




outcome.mod$coefficients

mean(df.ps$weight_ate)
mean(df.ps$weight_att)
