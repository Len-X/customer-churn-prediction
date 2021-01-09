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



{ # Narrow Models --------------------------------------------------------------
    set.seed(5)
    churn_split = initial_split(narrow_df, prop=0.75)
    churn_train = training(churn_split)
    churn_valid = testing (churn_split)

    churn_cv  <- vfold_cv(churn_train, strata=churn, v=10)

    { # define recipe and a random forest model, and tune the model
        rf_recipe <- narrow_df %>% 
            recipe(churn ~ .)

        rf_model <- rand_forest() %>% 
            # the number of trees will be tuned from 125 to 1855
            # the number of variable will be tuned from 1 to 8
            # we expect that the best mtry will be 2 or 3
            set_args(mtry = tune(), trees = tune()) %>% 
            set_engine("ranger", importance = "impurity") %>% 
            set_mode("classification")

        rf_workflow <- workflow() %>% 
            add_recipe(rf_recipe) %>% 
            add_model(rf_model)

        ctrl <- control_grid(verbose = TRUE, save_pred = TRUE)

        rf_tune_results <- rf_workflow %>% 
            tune_grid(resamples = churn_cv,
                      metrics   = metric_set(accuracy, recall, roc_auc),
                      control   = ctrl
            )

    }

    { # plot the penalty plot
        rf_plot_trees  <- 
            rf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = trees, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() +
            ggtitle("Narrow Random Forest penalty plot") +
            theme(plot.title = element_text(size = 20, hjust = 0.5))

        rf_plot_mtry  <- 
            rf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = mtry, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw()

        p = gridExtra::grid.arrange(rf_plot_trees, rf_plot_mtry, nrow = 2) 
        p # just to show the plot
        if(tosave) ggsave("pics/narrow_random_forest_penalty_plot.svg", plot=p)

    }
    # On the plot you can see that the model is overfitted because 
    # recall equals to 1 when number of trees equals to 1517 or
    # mtry = 1. So we will choose our hyperparameter by ROC AUC.

    { # choosing the best parameter and building the final model
        param_final  <- rf_tune_results %>% 
            select_best(metric = "roc_auc")

        rf_fit  <- rf_workflow %>% 
            finalize_workflow(param_final) %>% 
            last_fit(churn_split)

        rf_fit$.workflow # hyperparameter of the chosen model

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

            rf_roc_plot <- autoplot(rf_auc) + 
                ggtitle(paste0(c("Narrow Random Forest: AUC", round(auc_metric, 3)), collapse=" ")) + 
                theme(plot.title = element_text(size = 20, hjust = 0.5))

            rf_roc_plot
            if(tosave) ggsave("pics/narrow_random_forest_roc_plot.svg", plot=rf_roc_plot)
        }

    }

    { # Draw Distribution 
        validation_predictions <- rf_fit %>% collect_predictions()

        rf_val_dist = validation_predictions %>% 
            ggplot() +
            geom_density(aes(x = .pred_True, fill = churn), alpha=0.5) +
            theme_bw() +
            ggtitle("Narrow Random Forest, Validation Distribution") + 
            theme(plot.title = element_text(size = 20, hjust = 0.5))
        rf_val_dist
        if(tosave) ggsave("pics/narrow_random_forest_validation_distribution.svg", plot=rf_val_dist)

        
    }

    { # Validation Metrics
        validation_predictions %>% conf_mat    (truth = churn, estimate = .pred_class)
        validation_predictions %>% recall      (truth = churn, estimate = .pred_class, event_level="second")
        validation_predictions %>% accuracy    (truth = churn, estimate = .pred_class)
        validation_predictions %>% bal_accuracy(truth = churn, estimate = .pred_class)
        validation_predictions %>% kap         (truth = churn, estimate = .pred_class)
    }

}
