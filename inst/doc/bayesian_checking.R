## ---- echo=FALSE---------------------------------------------------------
# Should the full vignette be run or a fast version.
# The full version run all code and store the results to run faster later.
run_full_vignette <- TRUE # This is false to speed up checking etc

## ------------------------------------------------------------------------
library(tidytopics)
library(dplyr)
library(ggplot2)
data(sotu50)
sotu50

## ------------------------------------------------------------------------
top_keys <- top_terms(sotu50, "type_probability")
top_keys

## ------------------------------------------------------------------------
observed_mi <- mi(sotu50)
ggplot(data = observed_mi, aes(x = mi)) + geom_histogram(bins = 20)

## ------------------------------------------------------------------------
observed_imi <- imi(state = sotu50, w = top_keys)
observed_imi

## ------------------------------------------------------------------------
plt <- ggplot_imi_type(observed_imi, topic = 1)
plt

## ---- echo=FALSE---------------------------------------------------------
if(run_full_vignette){
  no_draws <- 25
  replicated_imi <- list()
  replicated_mi <- list()
  for(i in 1:no_draws){
    sim_tokens <- sample_tokens_given_topics(sotu50)
    replicated_imi[[i]] <- imi(sim_tokens, w = top_keys)
    replicated_mi[[i]] <- mi(sim_tokens)
  }
  replicated_imi <- bind_rows(replicated_imi)
  replicated_mi <- bind_rows(replicated_mi)
  save(replicated_imi, replicated_mi, file = "bayesian_checking_1.Rdata")
} else {
  load(file = "bayesian_checking_1.Rdata")
}


## ---- eval=FALSE---------------------------------------------------------
#  no_draws <- 25
#  replicated_imi <- list()
#  replicated_mi <- list()
#  for(i in 1:no_draws){
#    sim_tokens <- sample_tokens_given_topics(sotu50)
#    replicated_imi[[i]] <- imi(sim_tokens, w = top_keys)
#    replicated_mi[[i]] <- mi(sim_tokens)
#  }
#  replicated_imi <- bind_rows(replicated_imi)
#  replicated_mi <- bind_rows(replicated_mi)

## ------------------------------------------------------------------------
plt <- ggplot_imi_type(observed_imi, replicated_imi, topic = 1)
plt

## ------------------------------------------------------------------------
ggplot(data = filter(replicated_mi, topic == 1), aes(x = mi)) +
  geom_histogram(bins = 100) + 
  geom_vline(xintercept = filter(observed_mi, topic == 1)[["mi"]])

## ------------------------------------------------------------------------
mi_dev <- mi_deviance(observed_mi, replicated_mi)
mi_dev

## ------------------------------------------------------------------------
data(sotu)
doc_pres <- 
  transmute(sotu, doc, president, 
            random_group = sample(1:43, nrow(sotu), replace = TRUE))
sotu50 <- left_join(sotu50, y = doc_pres, by = "doc")

## ------------------------------------------------------------------------
observed_mi_president <- mi(sotu50, g = "president")
observed_mi_random_group <- mi(sotu50, g = "random_group")
observed_imi_president <- imi(sotu50, g = "president", w = top_keys)
observed_imi_random_group <- imi(sotu50, g = "random_group", w = top_keys)

## ---- echo=FALSE---------------------------------------------------------
if(run_full_vignette){
  no_draws <- 25
  replicated_mi_president <- list()
  replicated_mi_random_group <- list()
  replicated_imi_president <- list()
  replicated_imi_random_group <- list()
  for(i in 1:no_draws){
    sim_tokens <- sample_tokens_given_topics(sotu50)
    replicated_mi_president[[i]] <- 
      mi(sim_tokens, g = "president")
    replicated_imi_president[[i]] <- 
      imi(sim_tokens, g = "president", w = top_keys)
    replicated_mi_random_group[[i]] <- 
      mi(sim_tokens, g = "president")
    replicated_imi_random_group[[i]] <- 
      imi(sim_tokens, g = "president", w = top_keys)
  }
  replicated_mi_president <- bind_rows(replicated_mi_president)
  replicated_mi_random_group <- bind_rows(replicated_mi_president)
  replicated_imi_president <- bind_rows(replicated_imi_president)
  replicated_imi_random_group <- bind_rows(replicated_imi_random_group)
  save(replicated_mi_president, replicated_mi_random_group, replicated_imi_president, replicated_imi_random_group, file = "bayesian_checking_2.Rdata")
} else {
  load("bayesian_checking_2.Rdata")
}

## ---- eval=FALSE---------------------------------------------------------
#  no_draws <- 25
#  replicated_mi_president <- list()
#  replicated_mi_random_group <- list()
#  replicated_imi_president <- list()
#  replicated_imi_random_group <- list()
#  for(i in 1:no_draws){
#    sim_tokens <- sample_tokens_given_topics(sotu50)
#    replicated_mi_president[[i]] <-
#      mi(sim_tokens, g = "president")
#    replicated_imi_president[[i]] <-
#      imi(sim_tokens, g = "president", w = top_keys)
#    replicated_mi_random_group[[i]] <-
#      mi(sim_tokens, g = "president")
#    replicated_imi_random_group[[i]] <-
#      imi(sim_tokens, g = "president", w = top_keys)
#  }
#  replicated_mi_president <- bind_rows(replicated_mi_president)
#  replicated_mi_random_group <- bind_rows(replicated_mi_president)
#  replicated_imi_president <- bind_rows(replicated_imi_president)
#  replicated_imi_random_group <- bind_rows(replicated_imi_random_group)

## ------------------------------------------------------------------------
mi_dev_pres <- 
  mi_deviance(observed_mi_president, replicated_mi_president) %>%
  mutate(g = "president")
mi_dev_rg <- 
  mi_deviance(observed_mi_random_group, replicated_mi_random_group) %>%
  mutate(g = "random_group")
mi_dev <- 
  mi_deviance(observed_mi, replicated_mi) %>%
  mutate(g = "doc")  %>%
  bind_rows(mi_dev_pres, mi_dev_rg)

ggplot(data = mi_dev, aes(x = g, y = deviance)) + 
  geom_boxplot() + 
  coord_flip()

## ------------------------------------------------------------------------
ggplot_imi_type(observed_imi = observed_imi_president, replicated_imi = replicated_imi_president, topic = 1)

ggplot_imi_type(observed_imi = observed_imi_random_group, replicated_imi = replicated_imi_random_group, topic = 1)

## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()

