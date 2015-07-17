H <- seq(0.0,1,0.05)
hypo_space = data.frame(hypothesis=H, priors=rep(1/length(H), length(H)))
#COIN_BIAS = seq(0,1,0.1)

flip_coins <- function(n, bias) {
  nums <- runif(n)
  sapply(nums, function(val) if (val > bias) FALSE else TRUE)
}

mle <- function(flips) {
  sum(flips) / length(flips)
}


lik<- function(bias, flips) {
  total_head <- sum(flips) 
  total_obv <- length(flips)
  n_choose_k <- choose(total_obv, total_head)
  n_choose_k * (bias ^ total_head) * ((1 - bias) ^ (total_obv - total_head))
}


post <- function(likelihood, priors) {
  likelihood * priors
}

post_normalize <- function(flips, hypothesis_space) {
  likelihoods <- sapply(hypothesis_space$hypothesis, lik, flips=flips)
  posteriors <- likelihoods * hypothesis_space$priors
  sum_posterior <- sum(posteriors)
  posteriors / sum_posterior
}


#ggplot(df, aes(x=x, y=y)) + geom_point(size=2)

gen_lik <- function() {
  flip_prob <- NULL
  hypo_prob <- NULL
  likelihood <- NULL
  for(flip_bias in seq(0,1, by=0.1)) {
    flips <- flip_coins(20, flip_bias)
    for (prob in hypo_space$hypothesis) {
      flip_prob <- c(flip_prob, as.character(flip_bias))
      hypo_prob <- c(hypo_prob, prob)
      likelihood <- c(likelihood, lik(prob, flips))
    }
  }
  data_frame <- data.frame(flip_prob=flip_prob, hypo_prob=hypo_prob, likelihood=likelihood)
  data_frame
}

plot_likelihoods<- function() {
  df <- gen_lik()
  # select a subset of data
  selected_probs <- sample(df$flip_prob, 5)
  df <- df[df$flip_prob %in% selected_probs, ]
  p <- ggplot(data=df, aes(x=hypo_prob, y=likelihood, colour=flip_prob)) + geom_point(shape=1) + facet_grid(. ~ flip_prob)
  p
}




