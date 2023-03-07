# Install needed packages
install.packages("tidyverse", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages('pROC', dependencies = TRUE)

# Load libraries
library(tidyverse)
library(caret)
library(ROSE)
library(pROC)
library(ROCit)

# Read data
set.seed(-1)
data <- readr::read_csv("./data/final_data.csv")

data%>%
  group_by(bad_client) %>%
  summarize(records=n()) %>% 
  ggplot(aes(x=as.factor(bad_client), y=records, fill=c(as.factor(bad_client)))) +
  geom_bar(stat="identity") +
  labs(fill="cliente", x=element_blank(), y="registros", title="Conjunto original") +
  theme_minimal()

good_clients <- data %>% filter(bad_client==0)
good_clients <- sample_n(good_clients, 3500)
bad_clients <- data %>% filter(bad_client==1)
data <- rbind(good_clients, bad_clients)
data <- data[sample(1:nrow(data)), ]

data%>%
  group_by(bad_client) %>%
  summarize(records=n()) %>% 
  ggplot(aes(x=as.factor(bad_client), y=records, fill=c(as.factor(bad_client)))) +
  geom_bar(stat="identity") +
  labs(fill="cliente", x=element_blank(), y="registros", title="Conjunto inicial") +
  theme_minimal()

# Training
train <- data %>% dplyr::sample_frac(0.8)
test  <- dplyr::anti_join(data, train, by = "id")
table(train$bad_client)

model <- stats::glm(
  bad_client ~
    status_severity_month_0 +
    log_income +
    age +
    months_employed +
    own_property
  , 
  data = train, 
  family = binomial
)
summary(model)

# Prediction
probabilities <- model %>% stats::predict(test, type = "response")
test <- test %>% 
  mutate(pred=ifelse(probabilities > 0.5, 1, 0))

# Performance
cm <- confusionMatrix(as.factor(test$pred), as.factor(test$bad_client), positive = "1", mode="everything")
print(cm)
categories <- test$bad_client
ROCit_obj <- rocit(score=probabilities,class=categories)
plot(ROCit_obj)
ksplot(ROCit_obj)
auc <- ROCit_obj$AUC
gini <- 2*auc - 1

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Real",y = "Predicción") +
  theme_minimal() +
  theme(legend.position="none") 