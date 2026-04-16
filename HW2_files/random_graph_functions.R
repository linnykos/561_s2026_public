generate_random_graph <- function(n,
                                  clique_fraction = 0.2,
                                  density_low = 0.1){
  # Check all the arguments are correct
  stopifnot(n %% 1 == 0, n >= 0, 
            clique_fraction >= 0, clique_fraction <= 1,
            density_low >= 0, density_low <= 1)
  
  # Generate an unsymmetric matrix
  adj_mat <- matrix(sample(x = c(0,1),
                           size = n^2,
                           prob = c(1-density_low, density_low),
                           replace = TRUE), 
                    nrow = n, ncol = n)
  
  # Symmetrize the matrix
  adj_mat <- adj_mat + t(adj_mat)
  adj_mat[adj_mat > 0] <- 1
  diag(adj_mat) <- 1
  
  # Form the clique
  clique_size <- ceiling(n * clique_fraction)
  adj_mat[1:clique_size, 1:clique_size] <- 1
  
  # Randomize the order of the nodes
  sample_idx <- sample(1:n)
  adj_mat <- adj_mat[sample_idx, sample_idx]
  
  # Compute the appropriate reverse order
  rev_order <- sapply(1:n, function(i){
    which(sample_idx == i)
  })
  # To see what happens, try: adj_mat[rev_order, rev_order]
  
  return(list(adj_mat = adj_mat,
              rev_order = rev_order))
}


pivot_longer_heatmap <- function(adj_mat){
  # Convert the matrix to a tibble (a type of data frame) for easier manipulation
  colnames(adj_mat) <- paste0("node:", 1:nrow(adj_mat))
  mat_df <- as_tibble(adj_mat)
  
  # Add row numbers as a new column, since pivot_longer() will melt all existing columns
  mat_df <- mat_df %>% mutate(Row = row_number())
  
  # Use pivot_longer() to convert from wide to long format
  mat_long <- mat_df %>%
    pivot_longer(cols = !Row, 
                 names_to = "Column", 
                 values_to = "Value") %>%
    mutate(Column = as.numeric(gsub("node:", "", Column))) %>%
    rename(X = Row, Y = Column)
  
  return(mat_long)
}