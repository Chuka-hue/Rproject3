# Install libraries with install.packages("package.name") if required
library(tidyverse) # for data wrangling and visualization
library(ggpubr) # theme_pubr()
library(broom) # for tidy model output
library(dfidx) # for indexed data frames
library(mlogit) # for multinomial logit

# Read the data
data <- read.csv("cloud.csv",
                 stringsAsFactors = T) # automatically converts strings to factors

# Convert to tibble
data <- as_tibble(data)
summary(data)

# Define the reference levels 
data$cloud_storage <- relevel(data$cloud_storage, ref = '30gb')
data$price <- relevel(data$price, ref = 'p6')

# Turn the "price" into numeric class
data <- data %>% 
  mutate(price_n = gsub("p", "", price), # remove character 'p' from
         price_n = as.numeric(price_n))

mean(data$price_n)

# Number of choices
filter(data, choice == "1" & cloud_storage  == "30gb")
filter(data, choice == "1" & cloud_services == "email")

# Multinomial Conjoint Model
m_data <- dfidx(data, # the data set to use
                choice = "choice", # variable that contains choice
                idx = list(c("choice_id", "respondent_id"), # the two indexes (choice set and consumer) that define unique obs
                "alternative_id")) # the levels of the alternatives

# Print
m_data
glimpse(m_data)

# Estimate the model1
set.seed(123) 
model1 <- mlogit(choice ~ 0 + cloud_storage + customer_support + cloud_services + price, 
                data = m_data) 

summary(model1)$CoefTable

# Estimate the model2
set.seed(123) 
model2 <- mlogit(choice ~ 0 + cloud_storage + customer_support + cloud_services + price_n, 
                 data = m_data) 

summary(model2)$CoefTable

# Likelihood-ratio test
lrtest(model1, model2)

# Predict the choice probabilities
head(predict(model2, m_data), 1)

predicted_propabilities <- predict(model2, m_data) %>% 
  as_tibble()
predicted_propabilities

# The maximum choice probabilities.
predicted_alternative <-
  predicted_propabilities %>% 
  rowid_to_column("choice_id") %>% 
  pivot_longer(!choice_id, names_to = "choice", values_to = "prob") %>% 
  group_by(choice_id) %>% 
  slice(which.max(prob)) %>% 
  ungroup() %>% 
  select(choice) %>% 
  as_vector

predicted_alternative

selected_alternative <- 
  data %>% 
  filter(choice > 0) %>% 
  select(alternative_id) %>% 
  as_vector()

selected_alternative

table(selected_alternative, predicted_alternative)

# Function to predict market share
predict.share <- function(model, d) {
  temp <- model.matrix(update(model$formula, 0 ~ .), data = d)[, -1] # generate dummy matrix
  u <- temp %*% model$coef[colnames(temp)] # calculate utilities
  probs <- t(exp(u) / sum(exp(u))) # calculate probabilities
  colnames(probs) <- paste("alternative", colnames(probs))
  return(probs)
}

# Create a data object with hypothetical five alternatives
cloud_storage <- c("30gb", "30gb", "30gb", "5000gb", "5000gb")
customer_support <- c("no", "no", "yes", "yes", "no")
cloud_services <- c("email", "email, video", "email", "email", "email, video, productivity")
price_n <- c(6, 12, 12, 18, 18)

d_base <- data.frame(cloud_storage, customer_support, cloud_services, price_n)

d_base <- cbind(d_base, as.vector(predict.share(model2, d_base)))

d_base

# Change the column name
colnames(d_base)[5] <- "predicted_share"

d_base

# make a copy of the d_base data
d_new <- d_base

# update d_new data so that the value of row 2 (Galaxy) and column "ram" is now "r4gb"
d_new[5, "cloud_services"] <- "email, video"

# re-run the custom function and attach its output
d_new$predicted_share <- as.vector(predict.share(model2, d_new))

# print
d_new

# Willingness to Pay (customer support)
- coef(model2)["customer_supportyes"] / coef(model2)["price_n"]

# Willingness to Pay (cloud_storage)
- coef(model2)["cloud_storage2000gb"] / coef(model2)["price_n"]

# Willingness to Pay (cloud_storage)
- (coef(model2)["cloud_storage5000gb"] - coef(model2)["cloud_storage2000gb"]) / coef(model2)["price_n"]
