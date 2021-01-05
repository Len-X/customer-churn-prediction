# install.packages("kernlab")

# 1 Load data
library(tidyverse)
library(tidymodels)
library(recipes)
library(magrittr)
library(ggplot2)
library(gridExtra)

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

show_engines("svm_poly")

set.seed(0)

# 2 SVM

churn_split = initial_split(df, prop=0.8)
churn_train = training(churn_split)
churn_test  = testing (churn_split)

churn_cv  <- vfold_cv(churn_train, strata=churn, v=10)

{ # define recipe and model, and train model
    svm_recipe <- df %>% 
        recipe(churn ~ .)  %>% 
        step_zv(all_predictors()) %>% 
        step_lincomb(all_numeric())


    svm_model <-
        svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
        set_mode("classification") %>%
        set_engine("kernlab")

    svm_workflow <- workflow() %>% 
        add_recipe(svm_recipe) %>% 
        add_model(svm_model)

    ctrl <- control_grid(verbose = TRUE, save_pred = TRUE)

    svm_tune_results <- svm_workflow %>% 
      tune_grid(resamples = churn_cv,
        metrics = metric_set(accuracy, roc_auc),
        control = ctrl
      )

}
svm_tune_results %>% 
        collect_metrics()

{ # plot the penalty plot
    svm_plot_cost  <- 
        svm_tune_results %>% 
        collect_metrics() %>% 
#         filter(.metric == "roc_auc") %>% 
        ggplot(aes(x = cost, y = mean)) +
        geom_point() +
        geom_line() +
        facet_wrap(~.metric) +
#         ylab("AUC the ROC Curve") +
        theme_bw() +
        ggtitle("Tuning regarding to Cost")


    svm_plot_sigma  <- 
        svm_tune_results %>% 
        collect_metrics() %>% 
#         filter(.metric == "roc_auc") %>% 
        ggplot(aes(x = rbf_sigma, y = mean)) +
        geom_point() +
        geom_line() +
        facet_wrap(~.metric) +
#         ylab("AUC the ROC Curve") +
        theme_bw() +
        ggtitle("Tuning regarding to Sigma")

    grid.arrange(svm_plot_cost, svm_plot_sigma, nrow = 2)

}


{ # choosing the best parameter and building the final model
    param_final  <- svm_tune_results %>% 
        select_best(metric = "roc_auc")

    svm_fit  <- svm_workflow %>% 
        finalize_workflow(param_final) %>% 
        last_fit(churn_split)

}


{ # testing
    test_performance <- svm_fit %>% 
        collect_metrics(); test_performance

    test_predictions <- svm_fit %>% 
        collect_predictions(); test_predictions

    test_predictions %>% conf_mat(truth = churn, estimate = .pred_class)

    test_predictions %>% 
        ggplot() +
        geom_density(aes(x = .pred_True, fill = churn), alpha=0.5)



}
