#DEFINE A N BY N MATRIX
criteria <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7")
m <- c(1, 2, 3, 4, 7, 6, 5,
       1/2, 1, 2, 3, 6, 5, 4,
       1/3, 1/2, 1, 2, 5, 4, 3,
       1/4, 1/3, 1/2, 1, 4,3, 2,
       1/7, 1/6, 1/5, 1/4, 1, 1/2, 1/3,
       1/6, 1/5, 1/4, 1/3, 2, 1, 1/2,
       1/5, 1/4, 1/3, 1/2, 3, 2, 1)
m <- matrix(m, nrow=7, byrow = TRUE, dimnames = list(criteria, criteria))
?matrix
mat <- as.data.frame(m)
write.csv(mat, "criteria.csv")
ahp<- function(comparison_matrix){
  #get the dimension of matrix
  n <- length(comparison_matrix) ** 0.5
  comparison_matrix_init <- comparison_matrix
  #get sum of each column
  colsum<- Matrix::colSums(comparison_matrix)
  
  #normalize each cell by the column sum
  comparison_matrix <- comparison_matrix / matrix(colsum, ncol=n, nrow = n, byrow = TRUE)
  
  #criteria weight; find the average across each row
  criteria_weight <- Matrix::rowMeans(comparison_matrix)
  
  #To calculate the consistency
  #multiply the comparison matrix by the criteria weight
  row_product <- comparison_matrix_init *  matrix(criteria_weight, ncol=n, nrow = n, byrow = TRUE)
  #calculate the sum for each row
  weighted_sum <- Matrix::rowSums(row_product)
  #divide the weighted sum by the criteria weight, and calculate the mean
  weighted_sum_per_criteria_weight <- (weighted_sum/criteria_weight)
  max_mean <- mean(weighted_sum_per_criteria_weight)
  #in calculating the consistency, the formula below is used
  consistency_index <- (max_mean - n)/(n-1)
  
  #random index table
  ri <- matrix(c(0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49))
  
  consistency_ratio <- consistency_index/ri[n]
  
  if (consistency_ratio < 0.1){
    print(paste("Matrix decision is consistent", consistency_ratio, sep = ","))
  }
  return(criteria_weight)
}

f <- ahp(m)
f


