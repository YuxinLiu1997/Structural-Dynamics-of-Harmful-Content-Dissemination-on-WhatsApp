set.seed(42)

# Parameters
total_users <- 200000
k <- 1500  # number of users to sample
alpha <- 1.4
xmin <- 1
xmax <- 1000
share_rate <- 0.9  # 80% of user's groups are shared

# Power-law sampling function
rpowerlaw <- function(n, alpha, xmin=1, xmax=1000) {
  u <- runif(n)
  vals <- floor(xmin * (1 - u)^(-1 / (alpha - 1)))
  vals[vals > xmax] <- xmax
  return(vals)
}

# Step 1: Generate overlapping groups until all users are covered
group_sizes <- c()
group_members <- list()
user_covered <- rep(FALSE, total_users)

while (!all(user_covered)) {
  size <- rpowerlaw(1, alpha, xmin, xmax)
  members <- sample(1:total_users, size, replace = FALSE)
  
  group_sizes <- c(group_sizes, length(members))
  group_members[[length(group_members) + 1]] <- members
  user_covered[members] <- TRUE
}

num_groups <- length(group_sizes)
avg_group_size <- mean(group_sizes)

# Step 2: Build user-to-group mapping
user_to_groups <- vector("list", total_users)
for (g in 1:num_groups) {
  for (u in group_members[[g]]) {
    user_to_groups[[u]] <- c(user_to_groups[[u]], g)
  }
}

# Step 3: Sample k users
sampled_users <- sample(1:total_users, k)
shared_groups <- c()

for (u in sampled_users) {
  groups <- user_to_groups[[u]]
  if (length(groups) > 0) {
    n_share <- max(1, floor(length(groups) * share_rate))
    shared_groups <- c(shared_groups, sample(groups, n_share))
  }
}

distinct_shared_groups <- length(unique(shared_groups))

# Output
cat("Total users (unique):", total_users, "\n")
cat("Total groups:", num_groups, "\n")
cat("Average group size:", round(avg_group_size, 2), "\n")
cat("Sampled users:", k, "\n")
cat("Distinct groups hit by sampled users (after 80% sharing):", distinct_shared_groups, "\n")


