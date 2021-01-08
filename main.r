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

# 4. Explanatory Data Analysis =================================================

df %>% 
    group_by(state) %>% 
    summarise(n       = n(),
              n_churn = sum(churn == "True"),
              .groups = "drop") %>% 
    ungroup() %>% 

    mutate(churn.prop = round(n_churn/n, 3)) %>% 
    arrange(desc(churn.prop))


round(1234.2222, 2)
