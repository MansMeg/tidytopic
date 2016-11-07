## ---- echo=FALSE---------------------------------------------------------
suppressPackageStartupMessages(library(tidytopics))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(dplyr))

## ------------------------------------------------------------------------
library(tidytopics)
library(tidytext)
library(dplyr)

data("sotu50")
sotu50
is.tidy_topic_state(sotu50)

## ------------------------------------------------------------------------
top_terms(sotu50)

## ------------------------------------------------------------------------
nw <- topic_type_matrix(sotu50)
nw
nd <- doc_topic_matrix(sotu50)
nd

## ------------------------------------------------------------------------
is.tidy_topic_matrix(nw)
is.tidy_topic_array(nw)

data("sotu")
nd <- left_join(nd, sotu[,c("doc" )], by = "doc")

is.tidy_topic_matrix(nd)
is.tidy_topic_array(nd)

## ------------------------------------------------------------------------
setwd("../data-raw")
nw <- as.sparseMatrix(nw)
nw[1:10, 1:5]
print(getwd())
print(dir(getwd()))
print(dir(getwd()))
file.exists("sotu50.txt.gz")

## ---- echo=FALSE---------------------------------------------------------
setwd("../data-raw")
my_sotu <- read_mallet_statefile("sotu50.txt.gz")
alpha <- read_mallet_statefile("sotu50.txt.gz", type = "alpha")
beta <- read_mallet_statefile("sotu50.txt.gz", type = "beta")

## ---- eval=FALSE---------------------------------------------------------
#  my_sotu <- read_mallet_statefile("sotu50.txt.gz")
#  alpha <- read_mallet_statefile("sotu50.txt.gz", type = "alpha")
#  beta <- read_mallet_statefile("sotu50.txt.gz", type = "beta")

## ---- eval=TRUE----------------------------------------------------------
my_sotu
alpha[1:5]
beta

## ---- eval=FALSE---------------------------------------------------------
#  write_mallet_statefile(state = my_sotu,
#                         alpha = alpha,
#                         beta = beta,
#                         state_file = "my_mallet_state.txt.gz")

## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()

