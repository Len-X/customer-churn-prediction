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

df = read.csv(here::here("data/churn-bigml-80.csv"))

df %>% glimpse()

df %<>% 
    rename_all(function(.name){
                   .name %>% 
                       tolower() #%>% 
#                        str_replace(".", "_")
    })

df %>% glimpse()
df %>% str(max.level=1)

set.seed(0)

# 2 Random forest: splitting data

churn_split = initial_split(df, prop=0.8)
churn_train = training(churn_split)
churn_test  = testing (churn_split)

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

