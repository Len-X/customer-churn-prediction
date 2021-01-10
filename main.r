# 0. Uncomment and execute if needed ===========================================
# install.packages("tidymodels")
# install.packages("rlang")
# install.packages("here")
# install.packages("ranger")
# install.packages("kernlab")

tosave = TRUE # if you want to save all your plots in `pics` folder

# 1. Load libraries and data set ===============================================
{
    library(tidyverse)
    library(tidymodels)
    library(recipes)
    library(magrittr)
    library(ggplot2)
    library(gridExtra)
    library(Boruta)
    library(ggcorrplot)

    df = read.csv(here::here("data/churn-bigml-80.csv")) %>% 
        # make all feature names in lower case
        rename_all(function(.name){ .name %>% tolower() })

    test_df = read.csv(here::here("data/churn-bigml-20.csv")) %>% 
        # make all feature names in lower case
        rename_all(function(.name){ .name %>% tolower() })
}

# 2. Data introduction =========================================================

{ # Let's make sure that proportion of entries for each class is the same
  # for training/validation data frame and for testing one
  quantity_stats <- function(ldf) ldf %>% 
    group_by(churn) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    ungroup() %>% 

    mutate(n.prop = n / sum(n))
    
    df      %>% quantity_stats()
    test_df %>% quantity_stats()

    # Here n.prop the same for both data frames
    # There are 2278 False churns (customers didn't leave/stayed with company) 
    # and 388 True churns (customers actually left the company)
    # This indicates that we are dealing with slightly unbalanced data 
    # (low number of Trues and high number of Falses)

}

df %>% glimpse() # There are 2666 observations and 20 variables in the data set

df %>% summarise_all(~sum(is.na(.))) # missingness per feature: no missingness

# 3. Feature selection =========================================================
# Before conducting any type of analysis or model building 
# it would be great to exclude useless features.
# To do that we will use Boruta algorithm and Correlation step function

{ # Boruta algorithm
  # Boruta is a feature ranking and selection algorithm based on random 
  # forests algorithm.
  # 
  # The advantage with Boruta is that it clearly decides if a variable is 
  # important or not and helps to select variables that are statistically 
  # significant. Besides, you can adjust the strictness of the algorithm by 
  # adjusting the p values that defaults to 0.01 and the maxRuns.

    set.seed(3)
    boruta.df <- Boruta(churn ~ ., data=df, doTrace=2, maxRuns=50)
    print(boruta.df)

    { # Calculate stats for features
        imp.history = boruta.df$ImpHistory
        lz <- lapply(1:ncol(imp.history), 
                     function(i) imp.history[is.finite(imp.history[,i]),i])
        names(lz) <- colnames(boruta.df$ImpHistory)
        Labels    <- sort(sapply(lz, median))
        rm(imp.history)
    }

    
    { # Plot feature importance whisker plot
        if(tosave){
#             png(filename="pics/feature_importance.png", width=1280,height=720)
            svg(filename="pics/feature_importance.svg", width=16, height=9)

            ticks_cex = 2
            title_cex = 3
        }else{

            ticks_cex = 1.0
            title_cex = 1.5
        }

        par(mar = c(15, 4, 5, 4))
        plot(boruta.df, xlab = "", xaxt = "n", ylab = "", yaxt = "n")
        grid(NULL, NULL, lty = 7, col = "gray") 


        axis(side = 1, labels = FALSE, at = 1:ncol(boruta.df$ImpHistory))
        axis(side = 2, las = 2, mgp = c(3, 0.75, 0), cex.axis = ticks_cex)

        text(x = 1:ncol(boruta.df$ImpHistory),
             y = par("usr")[3] - 5,
             labels = names(Labels),
             xpd = NA,
             srt = 45,
             adj = 1.0,
             cex = ticks_cex)
        mtext("Variable Importance", side = 3, line = 1, cex = title_cex)

        if(tosave) dev.off()
    }
    # The whisker plot above shows us that only 13 features are important.
    # So we can drop the rest features.
    # 
    # The columns in green are confirmed the ones in red are not. There 
    # are couple of blue bars representing ShadowMax and ShadowMin. They 
    # are not actual features, but are used by the boruta algorithm to 
    # decide if a variable is important or not.
    #
    # There are 13 confirmed features out of 20. Features "international.plan"
    # and "customer.service.calls" have the highest importance.


    { # drop unimportant features

        confirmed       = boruta.df$finalDecision == "Confirmed"
        names_to_select = boruta.df$finalDecision[confirmed] %>%
            unlist() %>% 
            names()

        df = df %>% 
            select(churn, all_of(names_to_select))
    }

    df %>% glimpse()

}

{ # Draw correlation matrix

    model.matrix(~0+., data=df) %>% 
      cor(use="pairwise.complete.obs") %>% 
      ggcorrplot(show.diag = F, type="full", 
                 lab=FALSE, lab_size=1, tl.cex=10, tl.srt=45,
      ) +
    ggtitle("Correlation Matrix") +
    theme(plot.title = element_text(size = 20, hjust = 0.5))
    if(tosave)ggsave("pics/correlation_matrix.svg")

}

df %>% glimpse()

{ # Remove highly correlated features 
  # Here we use package recipe becaue in the future it could be 
  # used as a part of model's recipe.

    df = df %>% mutate(churn = as.character(churn)) 

    # I am still thinking about replacing dots with underscore
#     names(tdf) <- gsub("\\.", "_", names(tdf)) 

    correlation_recipe = df  %>% 
        recipe(churn ~ .) %>% 
        # transforming categorical variables to numbers
        step_dummy(c(international.plan, voice.mail.plan), one_hot=T) %>% 
        # removing predictor variables with correlation more than 0.9
        step_corr(threshold=0.9, all_predictors()) 

    narrow_df = correlation_recipe %>% 
        prep() %>% 
        juice()


}

narrow_df  %>% glimpse() # now we have only 9 variables.
                         # minutes and charge variables had a high
                         # correlation, so they are were removed.

wide_df = df

# 4. Explanatory Data Analysis =================================================
# To find out if there is a skewness we will draw density plots target variable
# against every predictor variable

{ # To automate work we will create two functions
  # draw_dist for continuous variables 
  # draw_hist for discrete variables

    draw_dist <- function(tdf){
        p = tdf %>% 
            ggplot(aes(variable, fill=churn)) +
            geom_density(alpha=.3) +
            theme_bw() + 
            ggtitle("Variable density") +
            theme(plot.title = element_text(size = 20, hjust = 0.5))
    }

    draw_hist <- function(tdf){
        p = tdf %>% 
            ggplot(aes(variable, fill=churn)) +
            geom_histogram(stat="count", position="dodge", alpha=0.8) + 
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            theme_bw() + 
            ggtitle("variable histogram") +
            theme(plot.title = element_text(size = 20, hjust = 0.5))
    }

    gchurn = df$churn
}

{ # Draw continuous variables
    continuous_df = df %>% select(-churn, -international.plan, - voice.mail.plan)

    continuous_plots = continuous_df %>% 
        map(function(x) draw_dist(data.frame(variable=x, churn=gchurn)))

    for(name in names(continuous_df)){
        print(continuous_plots[[name]] + labs(title = paste("Density of ", gsub("\\.", " ", name))))
        plot_name = gsub("\\.", "_", name) 
        plot_name = glue::glue("pics/", plot_name, ".svg")

        if(tosave) ggsave(plot_name)
    }
}


{ # Draw discrete variables
    discrete_df = df %>% select(international.plan, voice.mail.plan)

    discrete_plots = discrete_df %>% 
        map(function(x) draw_hist(data.frame(variable=x, churn=gchurn)))

    for(name in names(discrete_df)){
        print(discrete_plots[[name]] + labs(title = paste("Histogram of ", gsub("\\.", " ", name))))
        plot_name = gsub("\\.", "_", name) 
        plot_name = glue::glue("pics/", plot_name, ".svg")

        if(tosave) ggsave(plot_name)
    }
}

# 5. Model building ============================================================
# As were said to split the data set into (60/20/20=training/validation/testing)
# Testing set has been already split, then we need to split our data frame into
# Training and Validation. 60% of original data frame corresponds to 75 % of
# out narrow_df or wide_df data frame. (0.8 * 0.75 = 0.6)

set.seed(5)
narrow_churn_split = initial_split(narrow_df, prop=0.75)
narrow_churn_train = training(narrow_churn_split)
narrow_churn_valid = testing (narrow_churn_split)
narrow_churn_cv    = vfold_cv(narrow_churn_train, strata=churn, v=10)

set.seed(137) # just a fine number
wide_churn_split = initial_split(wide_df, prop=0.75)
wide_churn_train = training(wide_churn_split)
wide_churn_valid = testing (wide_churn_split)
wide_churn_cv    = vfold_cv(wide_churn_train, strata=churn, v=10)

{ # to show why we won't tune random forest by mtry
    {
        rf_recipe <- narrow_df %>% 
            recipe(churn ~ .)

        rf_model <- rand_forest() %>% 
            set_args(mtry = tune()) %>% 
            set_engine("ranger", importance = "impurity") %>% 
            set_mode("classification")

        rf_workflow <- workflow() %>% 
            add_recipe(rf_recipe) %>% 
            add_model(rf_model)

        ctrl <- control_grid(verbose = TRUE, save_pred = TRUE)

        doParallel::registerDoParallel()

        rf_tune_results <- rf_workflow %>% 
            tune_grid(resamples = narrow_churn_cv,
                      metrics   = metric_set(bal_accuracy, recall, roc_auc),
                      control   = ctrl
            )
    }



    { # plot the penalty plot
        rf_plot_mtry  <- 
            rf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = mtry, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() +
            ggtitle("Random Forest: tuning by mtry parameter") +
            theme(plot.title = element_text(size = 20, hjust = 0.5))

        rf_plot_mtry # just to show the plot
        if(tosave) ggsave("pics/_random_forest_penalty_plot.svg", 
                          plot=rf_plot_mtry)
        # bal_accuracy parameter clearly demonstrates that
        # mtry more than 3 = floor(sqrt(9)) does not bring 
        # a huge difference

    }
}

RandomForest <- function(df         ,
                         churn_split, 
                         churn_train, 
                         churn_valid, 
                         churn_cv   , 
                         forest_name){
    ### LOCAL TESTING
    #df          = narrow_df
    #churn_split = narrow_churn_split
    #churn_train = narrow_churn_train
    #churn_valid = narrow_churn_valid
    #churn_cv    = narrow_churn_cv
    #forest_name = "narrow"
    ### LOCAL TESTING
    { # define recipe and a random forest model, and tune the model
        rf_recipe <- df %>% 
            recipe(churn ~ .)

        m = df %>% ncol() %>% sqrt() %>% floor()

        rf_model <- rand_forest() %>% 
            set_args(mtry = m, trees = tune(), min_n = tune()) %>% 
            set_engine("ranger", importance = "impurity") %>% 
            set_mode("classification")

        rf_workflow <- workflow() %>% 
            add_recipe(rf_recipe) %>% 
            add_model(rf_model)

        # the number of trees will be tuned from 125 to 500.
        # the minimum number of data points in a node 
        # will be tuned from 2 to 8.
        rf_grid <- grid_regular(trees(range = c(125, 500)),
                                min_n(range = c(2, 8)),
                                levels = 6
        )
        ctrl <- control_grid(verbose = TRUE, save_pred = TRUE)

        doParallel::registerDoParallel()

        rf_tune_results <- rf_workflow %>% 
            tune_grid(resamples = churn_cv,
                      grid      = rf_grid,
                      metrics   = metric_set(bal_accuracy, recall, roc_auc),
                      control   = ctrl
            )

    }

    { # plot the penalty plot
        plot_title = paste(str_to_title(forest_name),
                           "Random Forest penalty plot", 
                           collapse=" ")
        rf_plot_trees  <- 
            rf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = trees, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() +
            ggtitle(plot_title) +
            theme(plot.title = element_text(size = 20, hjust = 0.5))

        rf_plot_min_n  <- 
            rf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = min_n, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw()

        rf_penalty_plot = gridExtra::grid.arrange(rf_plot_trees, rf_plot_min_n, nrow = 2) 
        rf_penalty_plot # just to show the plot
        plot_path = glue::glue("pics/", forest_name ,"_random_forest_penalty_plot.svg")
        if(tosave) ggsave(plot_path, plot=rf_penalty_plot)

    }
    # We will choose our hyperparameter by ROC AUC.

    { # choosing the best parameter and building the final model
        param_final  <- rf_tune_results %>% 
            select_best(metric = "roc_auc")

        rf_fit  <- rf_workflow %>% 
            finalize_workflow(param_final) %>% 
            last_fit(churn_split)

        print(rf_fit$.workflow) # hyperparameters of the chosen model

    }

    { # ROC and AUC

        { # calculate AUC
            roc_obj = rf_fit %>% 
                collect_predictions() %>% 
                pROC::roc(churn, .pred_True)
            auc_metric = pROC::auc(roc_obj)[[1]]

        }

        { # draw ROC
            rf_auc <- rf_fit %>% collect_predictions() %>% 
                roc_curve(churn, .pred_False) %>% 
                mutate(model = "Random Forest")

            plot_title = paste(str_to_title(forest_name),
                               "Random Forest: AUC", 
                               round(auc_metric, 3),
                               collapse=" ")

            rf_roc_plot <- autoplot(rf_auc) + 
                ggtitle(plot_title) + 
                theme(plot.title = element_text(size = 20, hjust = 0.5))

            rf_roc_plot
            plot_path = glue::glue("pics/", 
                                   forest_name ,
                                   "_random_forest_roc_plot.svg")
            if(tosave) ggsave(plot_path, plot=rf_roc_plot)
        }

    }

    { # Draw Distribution 
        validation_predictions <- rf_fit %>% collect_predictions()

        plot_title = paste(str_to_title(forest_name),
                           "Random Forest, Validation Distribution", 
                           collapse=" ")

        rf_val_dist = validation_predictions %>% 
            ggplot() +
            geom_density(aes(x = .pred_True, fill = churn), alpha=0.5) +
            theme_bw() +
            ggtitle(plot_title) + 
            theme(plot.title = element_text(size = 20, hjust = 0.5))
        rf_val_dist

        plot_path = glue::glue("pics/", 
                               forest_name, 
                               "_random_forest_validation_distribution.svg")
        if(tosave) ggsave(plot_path, plot=rf_val_dist)

        
    }

    { # Validation Metrics
        rf_conf_mat      = validation_predictions %>% conf_mat    (truth = churn, estimate = .pred_class)

        rf_recall        = validation_predictions %>% recall      (truth = churn, estimate = .pred_class, event_level="second")
        rf_accuracy      = validation_predictions %>% accuracy    (truth = churn, estimate = .pred_class)
        rf_fbal_accuracy = validation_predictions %>% bal_accuracy(truth = churn, estimate = .pred_class)
        rf_kap           = validation_predictions %>% kap         (truth = churn, estimate = .pred_class)

        rf_metrics = bind_rows(rf_recall       ,
                               rf_accuracy     ,
                               rf_fbal_accuracy,
                               rf_kap          
        )
    }

    rf_workflow = rf_workflow %>% finalize_workflow(param_final)
    return(list(workflow     = rf_workflow    ,
                penalty_plot = rf_penalty_plot,
                roc_plot     = rf_roc_plot    ,
                valid_dist   = rf_val_dist    ,
                conf_mat     = rf_conf_mat    ,
                metrics      = rf_metrics
        )
    )


}

{ # to show why we will use svm_rbf over svm_poly

    { # define recipe and model, and train model
        svm_recipe <- narrow_df %>% 
            recipe(churn ~ .)  %>% 
            step_zv(all_predictors()) %>% 
            step_lincomb(all_numeric())  %>% 
            step_normalize(all_numeric())

        svm_model_poly <-
            svm_poly(cost = tune(), scale_factor = tune()) %>%
            set_mode("classification") %>%
            set_engine("kernlab")

        svm_poly_workflow <- workflow() %>% 
            add_recipe(svm_recipe) %>% 
            add_model(svm_model_poly)

        svm_model_rbf <-
            svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
            set_mode("classification") %>%
            set_engine("kernlab")

        svm_rbf_workflow <- workflow() %>% 
            add_recipe(svm_recipe) %>% 
            add_model(svm_model_rbf)

        ctrl <- control_grid(verbose = TRUE, save_pred = TRUE)

        doParallel::registerDoParallel()

        svm_poly_tune_results <- svm_poly_workflow %>% 
            tune_grid(resamples = narrow_churn_cv,
                      metrics   = metric_set(bal_accuracy, recall, roc_auc),
                      control = ctrl
          )

        svm_rbf_tune_results <- svm_rbf_workflow %>% 
            tune_grid(resamples = narrow_churn_cv,
                      metrics   = metric_set(bal_accuracy, recall, roc_auc),
                      control = ctrl
          )

    }

    { # plot the penalty plot for poly method
        svm_plot_cost  <- 
            svm_poly_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = cost, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() +
            ggtitle("Polynomial SVM: penalty plot") +
            theme(plot.title = element_text(size = 20, hjust = 0.5))

        # bal_accuracy parameter clearly demonstrates that
        # mtry more than 3 = floor(sqrt(9)) does not bring 
        # a huge difference


        svm_plot_scale  <- 
            svm_poly_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = scale_factor, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() 

        svm_poly_plot = grid.arrange(svm_plot_cost, svm_plot_scale, nrow = 2)

        svm_poly_plot # just to show the plot
        if(tosave) ggsave("pics/svm_poly_penalty_plot.svg", 
                          plot=svm_poly_plot)

    }

    { # plot the penalty plot for rbf method
        svm_plot_cost  <- 
            svm_rbf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = cost, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() +
            ggtitle("Radial Basis Function SVM: penalty plot") +
            theme(plot.title = element_text(size = 20, hjust = 0.5))


        svm_plot_sigma  <- 
            svm_rbf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = rbf_sigma, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() 

        svm_rbf_plot = grid.arrange(svm_plot_cost, svm_plot_sigma, nrow = 2)

        svm_rbf_plot # just to show the plot
        if(tosave) ggsave("pics/svm_rbf_penalty_plot.svg", 
                          plot=svm_rbf_plot)

    }
}

SupportVectorMachine <- function(df         ,
                                 churn_split, 
                                 churn_train, 
                                 churn_valid, 
                                 churn_cv   , 
                                 svm_name)
{
    ### LOCAL TESTING
    #df          = wide_df
    #churn_split = wide_churn_split
    #churn_train = wide_churn_train
    #churn_valid = wide_churn_valid
    #churn_cv    = wide_churn_cv
    #svm_name    = "wide"
    ### LOCAL TESTING

    { # define recipe and model, and train model
        svm_recipe <- df %>% 
            recipe(churn ~ .)  %>% 
            step_zv(all_numeric()) %>% 
            step_lincomb(all_numeric()) %>% 
            step_normalize(all_numeric())


        svm_model <-
            svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
            set_mode("classification") %>%
            set_engine("kernlab")

        svm_workflow <- workflow() %>% 
            add_recipe(svm_recipe) %>% 
            add_model(svm_model)

        svm_grid <- grid_regular(cost(range = c(0, 4)),
                                 rbf_sigma(range = c(-5, 1)),
                                 levels = 6
        )
        ctrl <- control_grid(verbose = TRUE, save_pred = TRUE)

        doParallel::registerDoParallel()

        svm_tune_results <- svm_workflow %>% 
            tune_grid(resamples = churn_cv,
                      grid      = svm_grid,
                      metrics   = metric_set(bal_accuracy, recall, roc_auc),
                      control = ctrl
          )

    }

    { # plot the penalty plot

        plot_title = paste(str_to_title(svm_name),
                           "SVM penalty plot", 
                           collapse=" ")
        svm_plot_cost  <- 
            svm_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = cost, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() +
            ggtitle(plot_title) +
            theme(plot.title = element_text(size = 20, hjust = 0.5))

        svm_plot_sigma  <- 
            svm_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = rbf_sigma, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            theme_bw() 

        svm_penalty_plot = grid.arrange(svm_plot_cost, svm_plot_sigma, nrow = 2)
        plot_path = glue::glue("pics/", svm_name ,"_svm_penalty_plot.svg")
        if(tosave) ggsave(plot_path, plot=svm_penalty_plot)

    }

    { # choosing the best parameter and building the final model
        param_final  <- svm_tune_results %>% 
            select_best(metric = "roc_auc")

        svm_fit  <- svm_workflow %>% 
            finalize_workflow(param_final) %>% 
            last_fit(churn_split)

        print(svm_fit$.workflow) # hyperparameters of the chosen model

    }

    { # ROC and AUC

        { # calculate AUC
            roc_obj = svm_fit %>% 
                collect_predictions() %>% 
                pROC::roc(churn, .pred_True)
            auc_metric = pROC::auc(roc_obj)[[1]]

        }

        { # draw ROC
            svm_auc <- svm_fit %>% collect_predictions() %>% 
                roc_curve(churn, .pred_False) %>% 
                mutate(model = "Support Vector Machine")

            plot_title = paste(str_to_title(svm_name),
                               "SVM: AUC", 
                               round(auc_metric, 3),
                               collapse=" ")

            svm_roc_plot <- autoplot(svm_auc) + 
                ggtitle(plot_title) + 
                theme(plot.title = element_text(size = 20, hjust = 0.5))

            svm_roc_plot
            plot_path = glue::glue("pics/", 
                                   svm_name ,
                                   "_svm_roc_plot.svg")
            if(tosave) ggsave(plot_path, plot=svm_roc_plot)
        }

    }

    { # Draw Distribution 
        validation_predictions <- svm_fit %>% collect_predictions()

        plot_title = paste(str_to_title(svm_name),
                           "SVM, Validation Distribution", 
                           collapse=" ")

        svm_val_dist = validation_predictions %>% 
            ggplot() +
            geom_density(aes(x = .pred_True, fill = churn), alpha=0.5) +
            theme_bw() +
            ggtitle(plot_title) + 
            theme(plot.title = element_text(size = 20, hjust = 0.5))

        plot_path = glue::glue("pics/", 
                               svm_name, 
                               "_svm_validation_distribution.svg")
        if(tosave) ggsave(plot_path, plot=svm_val_dist)

        
    }

    { # Validation Metrics
        svm_conf_mat      = validation_predictions %>% conf_mat    (truth = churn, estimate = .pred_class)

        svm_recall        = validation_predictions %>% recall      (truth = churn, estimate = .pred_class, event_level="second")
        svm_accuracy      = validation_predictions %>% accuracy    (truth = churn, estimate = .pred_class)
        svm_fbal_accuracy = validation_predictions %>% bal_accuracy(truth = churn, estimate = .pred_class)
        svm_kap           = validation_predictions %>% kap         (truth = churn, estimate = .pred_class)

        svm_metrics = bind_rows(svm_recall       ,
                                svm_accuracy     ,
                                svm_fbal_accuracy,
                                svm_kap          
        )
    }

    svm_workflow = svm_workflow %>% finalize_workflow(param_final)
    return(list(workflow     = svm_workflow    ,
                penalty_plot = svm_penalty_plot,
                roc_plot     = svm_roc_plot    ,
                valid_dist   = svm_val_dist    ,
                conf_mat     = svm_conf_mat    ,
                metrics      = svm_metrics
        )
    )

}


# Narrow Models ---------------------------------------------------------------


narrow_forest = RandomForest(narrow_df         ,
                             narrow_churn_split,
                             narrow_churn_train,
                             narrow_churn_valid,
                             narrow_churn_cv   ,
                             "narrow"
)


narrow_forest$penalty_plot %>% print() # idk how to make it print
narrow_forest$roc_plot     %>% print()
narrow_forest$valid_dist   %>% print()
narrow_forest$conf_mat 
narrow_forest$metrics 
narrow_forest$workflow  


narrow_svm = SupportVectorMachine(narrow_df         ,
                                  narrow_churn_split,
                                  narrow_churn_train,
                                  narrow_churn_valid,
                                  narrow_churn_cv   ,
                                  "narrow"
)


narrow_svm$penalty_plot %>% print() # idk how to make it print
narrow_svm$roc_plot     %>% print()
narrow_svm$valid_dist   %>% print()
narrow_svm$conf_mat 
narrow_svm$metrics 
narrow_svm$workflow  

# Wide Models -----------------------------------------------------------------


wide_forest = RandomForest(wide_df         ,
                           wide_churn_split,
                           wide_churn_train,
                           wide_churn_valid,
                           wide_churn_cv   ,
                           "wide"
)


wide_forest$penalty_plot %>% print() # idk how to make it print
wide_forest$roc_plot     %>% print()
wide_forest$valid_dist   %>% print()
wide_forest$conf_mat 
wide_forest$metrics 


wide_svm = SupportVectorMachine(wide_df         ,
                                wide_churn_split,
                                wide_churn_train,
                                wide_churn_valid,
                                wide_churn_cv   ,
                                "wide"
)

wide_svm$penalty_plot %>% print() # idk how to make it print
wide_svm$roc_plot     %>% print()
wide_svm$valid_dist   %>% print()
wide_svm$conf_mat 
wide_svm$metrics %>% select(-.estimator)

# Compare Wide and Narrow SVM models -------------------------------------------

wide_svm_metrics = wide_svm$metrics %>% 
    select(-.estimator) %>% 
    rename(metric   = .metric, 
           wide_svm = .estimate)

narrow_svm_metrics = narrow_svm$metrics %>% 
    select(-.estimator) %>% 
    rename(metric     = .metric, 
           narrow_svm = .estimate)

left_join(wide_svm_metrics, narrow_svm_metrics, by="metric") %>% 
    mutate(diff = narrow_svm - wide_svm)


# Compare Wide and Narrow RF models --------------------------------------------

wide_forest_metrics = wide_forest$metrics %>% 
    select(-.estimator) %>% 
    rename(metric  = .metric, 
           wide_rf = .estimate)

narrow_forest_metrics = narrow_forest$metrics %>% 
    select(-.estimator) %>% 
    rename(metric    = .metric, 
           narrow_rf = .estimate)

left_join(wide_forest_metrics, narrow_forest_metrics, by="metric") %>% 

# Compare RF and SVM -----------------------------------------------------------
left_join(narrow_svm_metrics, narrow_forest_metrics, by="metric")  %>% 
    mutate(diff = narrow_rf - narrow_svm)
# so we choose Narrow Random Forest Model

# 6. Model Testing =============================================================


{ # to make test data frame in the same format as it was done outside of
  # the recipe in the RandomForest function
    correlation_recipe = test_df  %>% 
        select(churn, all_of(names_to_select)) %>% 
        recipe(churn ~ .) %>% 
        step_dummy(c(international.plan, voice.mail.plan), one_hot=T)  

    narrow_test_df = correlation_recipe %>% 
        prep() %>% 
        juice()
}

{ # final fit testing data
    ffit = narrow_forest$workflow %>% 
        parsnip::fit(narrow_df) # here we will use all the training data 
                                # to refit it, as hparameters were already found

        rf_final_fit = pull_workflow_fit(ffit)

        predicted = predict(rf_final_fit,new_data = narrow_test_df)
        testing_prediction = bind_cols(narrow_test_df, predicted)
}


{ # calculate final testing metrics
    rf_conf_mat      = testing_prediction %>% conf_mat    (truth = churn, estimate = .pred_class)

    rf_recall        = testing_prediction %>% recall      (truth = churn, estimate = .pred_class, event_level="second")
    rf_accuracy      = testing_prediction %>% accuracy    (truth = churn, estimate = .pred_class)
    rf_fbal_accuracy = testing_prediction %>% bal_accuracy(truth = churn, estimate = .pred_class)
    rf_kap           = testing_prediction %>% kap         (truth = churn, estimate = .pred_class)


    rf_metrics = bind_rows(rf_recall       ,
                           rf_accuracy     ,
                           rf_fbal_accuracy,
                           rf_kap          ) %>% 
        select(-.estimator)

    rf_metrics
}




