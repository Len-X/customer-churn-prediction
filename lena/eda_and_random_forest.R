# 0 uncomment and execute if needed
# install.packages("tidymodels")
# install.packages("rlang")
# install.packages("here")
# install.packages("ranger")

# 1 Load data
library(tidyverse)
library(tidymodels)
library(recipes)
library(magrittr)
library(ggplot2)

df = read.csv(here::here("ML_Project/data/churn-bigml-80.csv"))

df %>% glimpse()

df %<>% 
    rename_all(function(.name){
                   .name %>% 
                       tolower() #%>% 
#                        str_replace(".", "_")
    })

df %>% glimpse()
df %>% str(max.level=1)


## Data exploration

dim(df)
# There are 2666 observations and 20 variables in the data set

names(df)
summary(df)
table(df$churn)
# There are 2278 False churns (customers didn't leave/stayed with company) 
# and 388 True churns (customers actually left the company)
# This indicates that we are dealing with unbalanced data (low number of Trues and high number of Falses)

# Checking for missing values
sum(is.na(df))
# There are no missing values

# Plotting the data for quick investigation

plot(df$churn)

plot(df$churn, df$account.length, xlab="Churn", ylab="Account Length")
# boxplot indicates almost equal distribution in respect to the account length.

plot(df$churn, df$customer.service.calls, xlab="Churn", ylab="Number of Customer Service calls")
# boxplot indicates that there is a dependency between a customer Churn and a number of
# customer service calls. Higher number of CS calls could lead to a higher chance of customer churn.

plot(df$churn, df$total.day.charge, xlab="Churn", ylab="Total day charge")
# boxplot indicates that there may be a dependency between a customer Churn and the amount of total day charge.

plot(df$churn, df$total.eve.charge, xlab="Churn", ylab="Total evening charge")
# boxplot indicates almost equal distribution in respect to the total evening charge (slightly more total 
# evening charge for confirmed churn).

plot(df$churn, df$total.night.charge, xlab="Churn", ylab="Total night charge")
# boxplot indicates almost equal distribution in respect to the total night charge.

plot(df$churn, df$total.intl.charge, xlab="Churn", ylab="Total international charge")
# boxplot indicates that there is a slight dependency between a customer Churn and Total international charge.

sort(table(df$state), decreasing = TRUE)
# Top three states with highest number of customers: West Virginia (88), Minnesota (70) and New York (68).
# States with lowest number of customers: California (24), Louisiana (35) and Pennsylvania (36).

# States with the highest numbers of churn customers
new_df <- df %>% filter(churn == "True")
sort(table(new_df$state), decreasing = TRUE)

p1 <- new_df %>% 
    ggplot(aes(factor(churn), fill = state))+
    geom_jitter(stat="count", aes(colour = state))+
    scale_fill_manual(values = rainbow(51))+
    theme_bw()+
    xlab("Churn")+
    ylab("Count")+
    ggtitle("Customer Churn by State")+
    theme(plot.title = element_text(hjust = 0.5))
p1




# 2 Random forest: splitting data

set.seed(0)

churn_split = initial_split(df, prop=0.8)
churn_train = training(churn_split)
churn_test  = testing (churn_split)

# 10-fold Cross-Validation
churn_cv  <- vfold_cv(churn_train, strata=churn, v=10)

{ # define recipe and model, and train model
    churn_recipe <- df %>% 
        recipe(churn ~ .)

    rf_model <- rand_forest() %>% 
        set_args(mtry = tune()) %>% 
        set_engine("ranger", importance = "impurity") %>% 
        set_mode("classification")

    rf_workflow <- workflow() %>% 
        add_recipe(churn_recipe) %>% 
        add_model(rf_model)

    rf_grid  <- expand.grid(mtry=3:10)

    rf_tune_results <- rf_workflow %>% 
        tune_grid(resamples = churn_cv,
                  grid      = rf_grid,
                  metrics   = metric_set(accuracy, roc_auc))

}

{ # plot the penalty plot
    rf_plot  <- 
        rf_tune_results %>% 
        collect_metrics() %>% 
#         filter(.metric == "accuracy") %>% 
        ggplot(aes(x = mtry, y = mean)) +
        geom_point() +
        geom_line() +
        facet_wrap(~.metric) +
#         ylab("AUC the ROC Curve") +
        theme_bw()

    rf_plot

}

{ # choosing the best parameter and building the final model
    param_final  <- rf_tune_results %>% 
        select_best(metric = "roc_auc")

    rf_fit  <- rf_workflow %>% 
        finalize_workflow(param_final) %>% 
        last_fit(churn_split)

}

{ # testing
    test_performance <- rf_fit %>% 
        collect_metrics(); test_performance

    test_predictions <- rf_fit %>% 
        collect_predictions(); test_predictions

    test_predictions %>% conf_mat(truth = churn, estimate = .pred_class)

    test_predictions %>% 
        ggplot() +
        geom_density(aes(x = .pred_True, fill = churn), alpha=0.5)



}

