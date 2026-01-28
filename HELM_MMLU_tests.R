library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(digest)


dir_helm <- path.expand("~/helm_mmlu")
dir_runs <- file.path(dir_helm, "runs")
dir_v1   <- file.path(dir_runs, "v1.0.0")
dir_run <- list.files(dir_v1, full.names = TRUE)
dir_run <- dir_run[file.info(dir_run)$isdir]

spec_tbl <- function(dir_one) {
  fp <- file.path(dir_one, "run_spec.json")
  spec <- read_json(fp, simplifyVector = FALSE)
  rs <- if (!is.null(spec$run_spec)) spec$run_spec else spec
  scen <- rs$scenario_spec
  ad   <- rs$adapter_spec
  subj <- "unknown_subject"
  if (!is.null(scen$args$subject)) subj <- scen$args$subject
  ds_id <- paste0("mmlu:subject=", subj)
  mdl <- "unknown_model"
  if (!is.null(ad$model)) {
    mdl <- ad$model
  } else if (!is.null(ad$model_deployment)) {
    mdl <- ad$model_deployment
  }
  tibble(
    run_dir    = basename(dir_one),
    dataset_id = ds_id,
    model      = mdl
  )
}

spec_hash <- function(dir_one) {
  fp <- file.path(dir_one, "run_spec.json")
  spec <- read_json(fp, simplifyVector = FALSE)
  rs <- if (!is.null(spec$run_spec)) spec$run_spec else spec
  if (!is.null(rs$adapter_spec$model)) rs$adapter_spec$model <- NULL
  if (!is.null(rs$adapter_spec$model_deployment)) rs$adapter_spec$model_deployment <- NULL
  digest(rs, algo = "xxhash64")
}
run_tbl <- bind_rows(lapply(dir_run, spec_tbl))
run_tbl$cfg_hash <- vapply(dir_run, spec_hash, character(1))

metric_pool <- unique(c(
  "exact_match", "quasi_exact_match", "prefix_exact_match",
  "inference_runtime",
  "num_bytes"
))

metric_tbl <- function(stats, metric_pool) {
  nm <- vapply(
    stats,
    function(x) {
      if (is.null(x$name) || is.null(x$name$name) || length(x$name$name) == 0L) {
        NA_character_
      } else {
        as.character(x$name$name[1])
      }
    },
    character(1)
  )
  mu <- vapply(
    stats,
    function(x) {
      if (is.null(x$mean) || length(x$mean) == 0L) {
        NA_real_
      } else {
        as.numeric(x$mean[1])
      }
    },
    numeric(1)
  )
  out <- tibble(metric = nm, score = mu)
  out <- out[!is.na(out$metric) &
               !is.na(out$score) &
               out$metric %in% metric_pool, ]
  out
}

fp_stats <- vapply(
  run_tbl$run_dir,
  function(rd) file.path(dir_v1, rd, "stats.json"),
  character(1)
)
stats_raw <- lapply(fp_stats, function(fp) read_json(fp, simplifyVector = FALSE))

long_raw <- bind_rows(lapply(
  seq_along(stats_raw),
  function(i) {
    mt <- metric_tbl(stats_raw[[i]], metric_pool)
    mt$dataset_id <- run_tbl$dataset_id[i]
    mt$model      <- run_tbl$model[i]
    mt$cfg_hash   <- run_tbl$cfg_hash[i]
    mt
  }
))

setting <- select(
  ungroup(
    slice_max(
      group_by(
        count(long_raw, dataset_id, model, cfg_hash, name = "n_runs"),
        dataset_id, model
      ),
      n_runs,
      n = 1,
      with_ties = FALSE
    )
  ),
  dataset_id, model, cfg_hash
)

long <- summarise(
  group_by(
    select(
      inner_join(long_raw, setting, by = c("dataset_id", "model", "cfg_hash")),
      -cfg_hash
    ),
    dataset_id, model, metric
  ),
  score = mean(score, na.rm = TRUE),
  .groups = "drop"
)

lower_better <- c( "inference_runtime", "num_bytes")

long <- mutate(
  filter(
    mutate(
      filter(mutate(long, score = as.numeric(score)), is.finite(score)),
      !(metric == "num_bytes" & score <= 0)
    ),
    !is.na(score)
  ),
  score = if_else(metric %in% lower_better, -score, score)
)

### Metric and model sets

Phi <- list(
  Phi_acc = c("exact_match", "prefix_exact_match", "quasi_exact_match"),
  Phi_mix = c("quasi_exact_match", "exact_match", "inference_runtime")
)

A <- list(
  A_1 = c(
    "openai/gpt-4-0613",
    "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229",
    "anthropic/claude-3-sonnet-20240229",
    "qwen/qwen1.5-72b",
    "meta/llama-2-70b", "anthropic/claude-3-haiku-20240307", 
    "01-ai/yi-34b", "01-ai/yi-6b"
  ),
  A_2 = c(
    "openai/gpt-4-0613",
    "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229",
    "anthropic/claude-3-sonnet-20240229",
    "qwen/qwen1.5-72b",
    "meta/llama-2-70b", "mistralai/mistral-7b-v0.1"
  ),
  A_3 = c(
    "openai/gpt-4-0613",
    "qwen/qwen1.5-72b",
    "meta/llama-2-70b", "mistralai/mistral-7b-v0.1"
  ),
  A_4 = c(
    "openai/gpt-4-0613",
    "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229",
    "anthropic/claude-3-sonnet-20240229",
    "qwen/qwen1.5-72b"
  )
)

complete <- function(sub, models) {
  tmp <- summarise(
    group_by(sub, dataset_id, metric),
    n_models = n_distinct(model),
    .groups = "drop"
  )
  
  pull(
    filter(
      summarise(
        group_by(tmp, dataset_id),
        complete = all(n_models == length(models)),
        .groups = "drop"
      ),
      complete
    ),
    dataset_id
  )
}

rankings <- function(long_tbl, models, metrics, tie = c("alph", "reverse_alph")) {
  tie <- match.arg(tie)
  
  sub <- filter(long_tbl, metric %in% metrics, model %in% models)
  
  ds <- complete(sub, models)
  sub <- filter(sub, dataset_id %in% ds)
  
  if (tie == "alph") {
    orders <- summarise(
      group_by(arrange(sub, dataset_id, metric, desc(score), model), dataset_id, metric),
      order = list(model),
      .groups = "drop"
    )
  } else {
    orders <- summarise(
      group_by(arrange(sub, dataset_id, metric, desc(score), desc(model)), dataset_id, metric),
      order = list(model),
      .groups = "drop"
    )
  }
  
  profiles <- summarise(
    group_by(orders, dataset_id),
    profile = list(setNames(order, metric)),
    models  = list(sort(unique(models))),
    .groups = "drop"
  )
  
  list(orders = orders, profiles = profiles)
}

###Single-peakedness helpers

sp_order_axis <- function(order, axis) {
  m <- length(axis)
  stopifnot(length(order) == m)
  for (t in seq_len(m)) {
    top_t <- order[1:t]
    idx <- sort(match(top_t, axis))
    if (max(idx) - min(idx) + 1 != length(idx)) return(FALSE)
  }
  TRUE
}

sp_profile_axis <- function(profile, axis) {
  all(vapply(profile, function(ord) sp_order_axis(ord, axis), logical(1)))
}

perm_all <- function(x) {
  n <- length(x)
  if (n == 1L) return(matrix(x, nrow = 1L))
  
  res <- NULL
  for (i in seq_len(n)) {
    first <- x[i]
    rest  <- x[-i]
    sub   <- perm_all(rest)
    res <- rbind(res, cbind(first, sub))
  }
  res
}

sp_axis_find <- function(models, profile) {
  perms <- perm_all(models)
  for (i in seq_len(nrow(perms))) {
    axis <- perms[i, ]
    if (sp_profile_axis(profile, axis)) return(list(sp = TRUE, axis = axis))
  }
  list(sp = FALSE, axis = NULL)
}

sp_run <- function(profiles) {
  select(
    mutate(
      profiles,
      sp_info      = map2(models, profile, sp_axis_find),
      single_peaked = map_lgl(sp_info, "sp"),
      axis         = map(sp_info, "axis")
    ),
    dataset_id, single_peaked, axis
  )
}

### Group separability helpers

subsets <- function(S) {
  s <- length(S)
  if (s <= 1L) return(list())
  unlist(
    lapply(1:(s - 1L), function(k) {
      combn(S, k, simplify = FALSE)
    }),
    recursive = FALSE
  )
}

partition <- function(profile_named_orders) {
  models <- sort(unique(unlist(profile_named_orders)))
  S <- models
  for (E in subsets(S)) {
    if (separation_in_S(profile_named_orders, E, S)) return(TRUE)
  }
  FALSE
}

sep_for_metric <- function(ord, E, S) {
  pos   <- setNames(seq_along(ord), ord)
  Epos  <- pos[E]
  Scpos <- pos[setdiff(S, E)]
  
  if (length(Epos) == 0L || length(Scpos) == 0L) return(FALSE)
  
  (max(Epos) < min(Scpos)) || (max(Scpos) < min(Epos))
}

separation_in_S <- function(profile_named_orders, E, S) {
  all(vapply(
    profile_named_orders,
    function(ord) sep_for_metric(ord, E, S),
    logical(1)
  ))
}

restrict_profile <- function(profile_named_orders, S_sub) {
  lapply(
    profile_named_orders,
    function(ord) ord[ord %in% S_sub]
  )
}

gs_inner <- function(profile_named_orders, S) {
  if (length(S) <= 2L) return(TRUE)
  E_list <- subsets(S)
  for (E in E_list) {
    if (separation_in_S(profile_named_orders, E, S)) {
      S_minus_E <- setdiff(S, E)
      profile_E  <- restrict_profile(profile_named_orders, E)
      profile_Sc <- restrict_profile(profile_named_orders, S_minus_E)
      if (gs_inner(profile_E, E) &&
          gs_inner(profile_Sc, S_minus_E)) {
        return(TRUE)
      }
    }
  }
  
  FALSE
}

group_separable <- function(profile_named_orders) {
  models <- sort(unique(unlist(profile_named_orders)))
  gs_inner(profile_named_orders, models)
}


### Distance-restrictedness helpers

swap_distance <- function(order1, order2) {
  stopifnot(length(order1) == length(order2))
  stopifnot(setequal(order1, order2))
  pos2 <- setNames(seq_along(order2), order2)
  p <- unname(pos2[order1]) 
  m <- length(p)
  inv <- 0L
  for (i in 1:(m-1)) {
    for (j in (i+1):m) {
      if (p[i] > p[j]) inv <- inv + 1L
    }
  }
  inv
}


dr <- function(orders_df, p = 1L) {
  summarise(
    group_by(
      arrange(orders_df, dataset_id, metric),
      dataset_id
    ),
    metrics = list(metric),
    orders  = list(order),
    max_dS  = {
      ords <- order
      n <- length(ords)
      if (n <= 1) return(0L)
      dmax <- 0L
      for (i in 1:(n-1)) for (j in (i+1):n) {
        d <- swap_distance(ords[[i]], ords[[j]])
        if (d > dmax) dmax <- d
      }
      dmax
    },
    distance_restricted = (max_dS <= p),
    .groups = "drop"
  )
}


###Experiments

sp_tests <- tribble(
  ~A_set, ~Phi_set, ~tie,
  "A_1",  "Phi_acc", "alph",
  "A_1",  "Phi_acc", "reverse_alph",
  "A_3",  "Phi_mix", "alph",
  "A_3",  "Phi_mix", "reverse_alph",
  "A_2",  "Phi_mix", "alph",
  "A_2",  "Phi_mix", "reverse_alph"
)

gs_tests <- tribble(
  ~A_set, ~Phi_set, ~tie,
  "A_2",  "Phi_acc", "alph",
  "A_2",  "Phi_acc", "reverse_alph",
  "A_3",  "Phi_mix", "alph",
  "A_3",  "Phi_mix", "reverse_alph",
  "A_1",  "Phi_mix", "alph",
  "A_1",  "Phi_mix", "reverse_alph"
)

dr_tests <- tribble(
  ~A_set, ~Phi_set, ~tie,
  "A_4",  "Phi_acc", "alph",
  "A_4",  "Phi_acc", "reverse_alph",
  "A_3",  "Phi_acc", "alph",
  "A_3",  "Phi_acc", "reverse_alph",
  "A_2",  "Phi_mix", "alph",
  "A_2",  "Phi_mix", "reverse_alph"
)

design <- distinct(bind_rows(sp_tests, gs_tests, dr_tests), A_set, Phi_set, tie)

rankings_tbl <- bind_rows(lapply(
  seq_len(nrow(design)),
  function(i) {
    A_name   <- design$A_set[i]
    Phi_name <- design$Phi_set[i]
    tie_rule <- design$tie[i]
    
    out <- rankings(
      long,
      models  = A[[A_name]],
      metrics = Phi[[Phi_name]],
      tie     = tie_rule
    )
    
    tibble(
      A_set   = A_name,
      Phi_set = Phi_name,
      tie     = tie_rule,
      profiles = list(out$profiles),
      orders   = list(out$orders)
    )
  }
))

### Single-peakedness results

sp_all <- bind_rows(lapply(
  seq_len(nrow(rankings_tbl)),
  function(i) {
    A_name   <- rankings_tbl$A_set[i]
    Phi_name <- rankings_tbl$Phi_set[i]
    tie_rule <- rankings_tbl$tie[i]
    if (!any(sp_tests$A_set == A_name &
             sp_tests$Phi_set == Phi_name &
             sp_tests$tie == tie_rule)) {
      return(NULL)
    }
    prof <- rankings_tbl$profiles[[i]]  
    res <- sp_run(prof)         
    res$A_set   <- A_name
    res$Phi_set <- Phi_name
    res$tie     <- tie_rule
    res
  }
))

sp_summary <- summarise(
  group_by(sp_all, A_set, Phi_set, tie),
  n_datasets       = n(),
  n_single_peaked  = sum(single_peaked),
  .groups = "drop"
)
print(sp_summary)

### Group-separability results

gs_all <- bind_rows(lapply(
  seq_len(nrow(rankings_tbl)),
  function(i) {
    A_name   <- rankings_tbl$A_set[i]
    Phi_name <- rankings_tbl$Phi_set[i]
    tie_rule <- rankings_tbl$tie[i]
    if (!any(gs_tests$A_set == A_name &
             gs_tests$Phi_set == Phi_name &
             gs_tests$tie == tie_rule)) {
      return(NULL)
    }
    prof <- rankings_tbl$profiles[[i]]
    res <- select(
      mutate(
        prof,
        group_separable = map_lgl(profile, group_separable)
      ),
      dataset_id, group_separable
    )
    
    res$A_set   <- A_name
    res$Phi_set <- Phi_name
    res$tie     <- tie_rule
    res
  }
))

gs_summary <- summarise(
  group_by(gs_all, A_set, Phi_set, tie),
  n_datasets          = n(),
  n_group_separable   = sum(group_separable),
  .groups = "drop"
)
print(gs_summary)

### Distance-restrictedness results

dr_all <- bind_rows(lapply(
  seq_len(nrow(rankings_tbl)),
  function(i) {
    A_name   <- rankings_tbl$A_set[i]
    Phi_name <- rankings_tbl$Phi_set[i]
    tie_rule <- rankings_tbl$tie[i]
    
    if (!any(dr_tests$A_set == A_name &
             dr_tests$Phi_set == Phi_name &
             dr_tests$tie == tie_rule)) {
      return(NULL)
    }
    
    ords <- rankings_tbl$orders[[i]]    
    res  <- dr(ords, p = 1L)
    
    res$A_set   <- A_name
    res$Phi_set <- Phi_name
    res$tie     <- tie_rule
    res
  }
))

dr_summary <- summarise(
  group_by(dr_all, A_set, Phi_set, tie),
  n_datasets           = n(),
  n_distance_restricted = sum(distance_restricted),
  .groups = "drop"
)
print(dr_summary)






##### Aggregation across datasets

library(ddandrda)
library(relations)
library(Rgraphviz)

metric_pool_agg <- unique(c(
  "exact_match", "quasi_exact_match", "prefix_exact_match",
  "inference_runtime",
  "num_prompt_tokens", "num_completion_tokens", "num_output_tokens",
  "num_bytes",
  "logprob", "perplexity", "bits_per_byte",
  "logprob_per_byte"
))

long_raw_agg <- bind_rows(lapply(
  seq_along(stats_raw),
  function(i) {
    mt <- metric_tbl(stats_raw[[i]], metric_pool_agg)
    mt$dataset_id <- run_tbl$dataset_id[i]
    mt$model      <- run_tbl$model[i]
    mt$cfg_hash   <- run_tbl$cfg_hash[i]
    mt
  }
))

setting_agg <- select(
  ungroup(
    slice_max(
      group_by(
        count(long_raw_agg, dataset_id, model, cfg_hash, name = "n_runs"),
        dataset_id, model
      ),
      n_runs,
      n = 1,
      with_ties = FALSE
    )
  ),
  dataset_id, model, cfg_hash
)

long_agg <- summarise(
  group_by(
    select(
      inner_join(long_raw_agg, setting_agg, by = c("dataset_id", "model", "cfg_hash")),
      -cfg_hash
    ),
    dataset_id, model, metric
  ),
  score = mean(score, na.rm = TRUE),
  .groups = "drop"
)

lower_better_agg <- c(
  "inference_runtime",
  "num_prompt_tokens", "num_completion_tokens", "num_output_tokens",
  "num_bytes",
  "perplexity",
  "bits_per_byte"
)

long_agg <- mutate(
  filter(
    mutate(
      filter(mutate(long_agg, score = as.numeric(score)), is.finite(score)),
      !(metric == "num_bytes" & score <= 0)
    ),
    !is.na(score)
  ),
  score = if_else(metric %in% lower_better_agg, -score, score)
)

comp_agg <- select(
  filter(
    summarise(
      group_by(long_agg, dataset_id, model),
      nm = n_distinct(metric),
      .groups = "drop"
    ),
    nm == length(metric_pool_agg)
  ),
  dataset_id, model
)

long_complete_agg <- inner_join(
  long_agg,
  comp_agg,
  by = c("dataset_id", "model")
)

ranks <- select(
  ungroup(
    mutate(
      arrange(
        group_by(long_complete_agg, dataset_id, metric),
        desc(score), model,
        .by_group = TRUE
      ),
      rank = row_number()
    )
  ),
  dataset_id, metric, model, rank
)

ranks$rank <- (-1) * ranks$rank


rank_to_incidence <- function(v){
  n <- length(v)
  A <- array(0,c(n,n))
  for(k in (1:n)){
    for(l in (1:n)){
      A[k,l] <- (v[k] > v[l])
    }
  }
  return(A)
}

aggregate <- function(dat,models,metrics,modelnames){
  context <- array(0,c(length(unique(dat$dataset_id)),length(models)^2))
  t <- 1
  relations <- list()
  for(dataset in dataset_names){
    sum_ranks <-array(0,c(length(models),length(models)))
    for(metric in metrics){
      temp <-rep(0,length(models))
      for(k in seq_len(length(models))){
        i <- which(dat$dataset_id==dataset & dat$metric==metric & dat$model==models[k])
        temp[k] <- dat$rank[i]
      }
      sum_ranks <- sum_ranks + rank_to_incidence(temp)
    }
    majority_relation <- 1*(sum_ranks >= t(sum_ranks))
    context[t,] <- as.vector(majority_relation)
    relations[[t]] <- majority_relation
    t <- t+1
    
    if(! relation_is_transitive(as.relation(majority_relation))){print("warning: non-transitive majority relation occured")}
    if(! relation_is_acyclic(as.relation(majority_relation))){print("warning: cyclic majority relation occured")}
  }
  
  context <- cbind(context,1-context)
  depths=ddandrda::compute_tukeys_depth(context,context)
  idxs <- which(!duplicated(context))
  if(length(which(max(depths[idxs])==depths[idxs]))>1){print("warning: more than one relation with maximal depth")}
  aggregated_relation <- relations[[which.max(depths)]]
  colnames(aggregated_relation) <- rownames(aggregated_relation) <- modelnames
  return(list(relations=relations,context=context,depths=depths,aggregated_relation=aggregated_relation))
}

dat <- ranks

dataset_names <- unique(dat$dataset_id)

metrics_acc <- c("exact_match", "prefix_exact_match","quasi_exact_match")
metrics_mix <- c("exact_match","quasi_exact_match","inference_runtime")

models <- unique(dat$model)

models1 <- models[c(16,17,5,6,19,10,4,1,2)]
modelnames1 <- c(" GPT-4-0613","GPT-4-1106", "Claude-3-Opus", "Claude-3-Sonnet", "Qwen1.5", "Llama-2" , "Claude-3-Haiku", "Yi-34b", "Yi-6b")

models2 <- models[c(16,17,5,6,19,10,13)]
modelnames2 <- c("GPT-4-0613","GPT-4-1106", "Claude-3-Opus", "Claude-3-Sonnet", "Qwen1.5", "Llama-2", "Mistral-7b")

models3 <- models[c(16,19,10,13)]
modelnames3 <- c("GPT-4-0613", "Qwen1.5", "Llama-2", "Mistral-7b")

models4 <- models[c(16,17,5,6,19)]
modelnames4 <- c("GPT-4-0613","GPT-4-1106" ,"Claude-3-Opus", "Claude-3-Sonnet", "Qwen1.5")

### Aggregated results for single-peaked preferences

result1 <- aggregate(dat,models1,metrics_acc,modelnames1)
result2 <- aggregate(dat,models3,metrics_mix,modelnames3)

par(mfrow=c(1,2))
plot(as.relation(t(result1$aggregated_relation)),main="")
plot(as.relation(t(result2$aggregated_relation)),main="")

### Aggregated results for group-separable preferences

result3 <- aggregate(dat,models2,metrics_acc,modelnames2)
result4 <- aggregate(dat,models3,metrics_mix,modelnames3)

par(mfrow=c(1,2))
plot(as.relation(t(result3$aggregated_relation)),main="")
plot(as.relation(t(result4$aggregated_relation)),main="")


### Aggregated results for distance-restricted preferences

result5 <- aggregate(dat,models4,metrics_acc,modelnames4)
result6 <- aggregate(dat,models3,metrics_acc,modelnames3)

par(mfrow=c(1,2))
plot(as.relation(t(result5$aggregated_relation)),main="")
plot(as.relation(t(result6$aggregated_relation)),main="")

