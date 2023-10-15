library(tidyverse)
library(tidymodels)
library(furrr)
options(tidymodels.dark = TRUE)
theme_set(theme_bw())
set.seed(0)

S <- matrix(rbeta(25, 0.5, 0.5), 5, 5)
X <- mvtnorm::rmvnorm(500, sigma = crossprod(S))
colnames(X) <- paste0("x", 1:5)
data_random <- as_tibble(X) |>
    mutate(y = 1 * x1 + 2 * x2 + 3 * x3 + 4 * x4 + 5 * x5 + rnorm(500, sd = 5))

data_iris <- iris |>
    as_tibble() |>
    rename(
        factor = Species,
        y = Petal.Width
    )

data_diamonds <- diamonds |>
    rename(
        factor1 = cut,
        factor2 = color,
        factor3 = clarity,
        yy = y,
        y = price
    )

data_concrete <- concrete |>
    rename(y = compressive_strength)

data_penguins <- penguins |>
    drop_na() |>
    rename(
        factor1 = species,
        factor2 = island,
        factor3 = sex,
        y = body_mass_g
    )

evaluate_model <- function(df) {
    data <- df |>
        mutate(across(
            -c(y, starts_with("factor")), \(x) {
                n <- length(x)
                index <- sample(n, size = 0.1 * n)
                x[index] <- NA
                x
            }
        ))

    split <- initial_validation_split(data, prop = c(0.8, 0.19))
    training <- training(split)
    validation <- validation_set(split)

    workflow <- workflow_set(
        preproc = list(
            mean = training |>
                recipe(y ~ .) |>
                step_impute_mean(all_numeric_predictors()),
            median = training |>
                recipe(y ~ .) |>
                step_impute_median(all_numeric_predictors()),
            linear = training |>
                recipe(y ~ .) |>
                step_impute_linear(all_numeric_predictors()),
            knn = training |>
                recipe(y ~ .) |>
                step_impute_knn(all_numeric_predictors()),
            bag = training |>
                recipe(y ~ .) |>
                step_impute_bag(all_numeric_predictors())
        ),
        models = list(impute = linear_reg())
    )

    workflow |>
        workflow_map(
            fn = "fit_resamples",
            validation,
            verbose = TRUE,
            seed = 0
        ) |>
        collect_metrics() |>
        select(-.config, -preproc, -model, -.estimator, -n, -std_err) |>
        pivot_wider(names_from = .metric, values_from = mean)
}

plan(multisession, workers = 12)
results <- future_map(
    1:2,
    \(x) evaluate_model(data_diamonds),
    .progress = TRUE
)

results |>
    imap(\(x, i) mutate(x, mc = i)) |>
    Reduce(f = rbind) |>
    summarise(
        rmse = mean(rmse),
        rsq = mean(rsq),
        .by = wflow_id
    )


penguins |>
    drop_na() |>
    select(-species, -island, -sex) |>
    rename(y = body_mass_g) %>%
    ggplot(aes(x = y, y = lm(y ~ ., .) |> predict())) +
    geom_point() +
    coord_obs_pred()