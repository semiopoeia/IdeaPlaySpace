#bring in necessary libraries
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(MASS)
library(plotly)
#function to create multivariate mixture distributions
generate_mixture_samples <- function(n_samples, means, covariances, proportions) {
  if (abs(sum(proportions) - 1) > 1e-6) {
    stop("Proportions must sum to 1.")
  }  
n_components <- length(proportions)
  component_samples <- sample(1:n_components, size = n_samples, replace = TRUE, prob = proportions)
  samples <- matrix(0, nrow = n_samples, ncol = length(means[[1]]))
  start_idx <- 1
  for (i in 1:n_components) {
    n_component_samples <- sum(component_samples == i)
    if (n_component_samples > 0) {
      component_samples <- mvrnorm(n = n_component_samples, mu = means[[i]], Sigma = covariances[[i]])
      samples[start_idx:(start_idx + n_component_samples - 1), ] <- component_samples
      start_idx <- start_idx + n_component_samples
    }
  }
  samples <- samples[sample(1:n_samples), ]
  return(samples)
}


#plugging values into function to generate multivariate mixture of gaussian distributions
means <- list(c(1, 0, -1), c(0, -1, 1))
covariances <- list(diag(3), diag(3))
proportions <- c(0.3, 0.7)
n_samples <- 1000
samples <- generate_mixture_samples(n_samples, means, covariances, proportions)

#put value into data.frame
sampleDat <- as.data.frame(samples)
colnames(sampleDat) <- c("x", "y", "z")

#make a 3d scatter for the mixtures (trivariate, two classes)
plot <- plot_ly(samples_df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers", marker = list(size = 2, opacity = 0.6)) %>%
  layout(title = "3D Scatter Plot of Multivariate Mixture Samples",
         scene = list(xaxis = list(title = "X-axis"),
                      yaxis = list(title = "Y-axis"),
                      zaxis = list(title = "Z-axis")))
print(plot)

#make data to work with for model fitting
write.csv(samples_df, file = "C:/Users/pws5/mmg.csv")