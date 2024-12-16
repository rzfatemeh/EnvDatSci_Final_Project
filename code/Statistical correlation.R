# Fatemeh Rezaei
# APM595
# Graduate project-HW6
# October 26, 2024

#create a pdf tosave the plots
pdf("Graduate project Plots-HW6.pdf", height = 12, width = 8)
par(mfrow=c(3,1))

# Load necessary libraries
library(readr)
#install.packages("png")
#install.packages("grid")



# Define the path to the folder containing the CSV files
folder_path <- "C:/Users/TEK1/OneDrive - Syracuse University/syracuse university/4 semester/APM595 probability/HW/HW6/graduate project"

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store matrices
matrices_list <- list()

# Loop through each file and process
for (file in file_list) {
  # Read the file 
  data <- read_lines(file)
  
  # Split each line by spaces and convert to a numeric matrix
  matrix_data <- do.call(rbind, lapply(strsplit(data, " "), as.numeric))
  
  # Append the matrix to the list
  matrices_list[[file]] <- matrix_data
}

# accessing matrices_list
print(dim(matrices_list[[1]]))

# Initialize a list to store the averages of submatrices
submatrix_averages <- list()

# Process each matrix in matrices_list
for (i in seq_along(matrices_list)) {
  matrix_data <- matrices_list[[i]]
  num_rows <- nrow(matrix_data)
  submatrix_size <- floor(num_rows / 10)  # Calculate the number of rows per submatrix
  
  # create a vector to store the averages 
  matrix_sub_averages <- numeric(10)
  
  # Divide the matrix into 10 submatrices and calculate the average of each
  for (j in 1:10) {
    start_row <- (j - 1) * submatrix_size + 1
    end_row <- ifelse(j == 10, num_rows, j * submatrix_size)  # The last submatrix may be larger if rows are not evenly divisible
    submatrix <- matrix_data[start_row:end_row, ]
    matrix_sub_averages[j] <- mean(submatrix, na.rm = TRUE)
  }
  
  # Append this matrix's submatrix averages to the list
  submatrix_averages[[i]] <- matrix_sub_averages
}

# Flatten the list of averages into a single vector for plotting
all_submatrix_averages <- unlist(submatrix_averages)

# Plot a histogram of the submatrix averages with 5 bins
hist(all_submatrix_averages, breaks = 10, main = "Histogram of Submatrix Averages", xlab = "Average Value", ylab = "Frequency", col = "lightgreen")

#install.packages("dplyr")


#BOX PLOT
library(readr)
library(ggplot2)
library(dplyr)

# Define the path to the folder containing the CSV files
folder_path <- "C:/Users/TEK1/OneDrive - Syracuse University/syracuse university/4 semester/APM595 probability/HW/HW6/graduate project"

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store temperature data
temperature_data <- list()

# Loop through each file and read the temperature values
for (file in file_list) {
  # Read the file without headers
  data <- read_lines(file)
  
  # Split each line by spaces and convert to numeric values
  temp_values <- unlist(lapply(strsplit(data, " "), as.numeric))
  
  # Append the temperature values to the list with the file name as an identifier
  temperature_data[[basename(file)]] <- temp_values
}

# Convert the list to a data frame for ggplot
temperature_df <- do.call(rbind, lapply(names(temperature_data), function(name) {
  data.frame(Image = name, Temperature = temperature_data[[name]])
}))

# Calculate average temperatures for sorting
average_temps <- temperature_df %>%
  group_by(Image) %>%
  summarise(Average_Temperature = mean(Temperature, na.rm = TRUE))

# Reorder the factor levels based on the average temperatures
temperature_df$Image <- factor(temperature_df$Image, 
                               levels = average_temps$Image[order(average_temps$Average_Temperature)])
# Save boxplot as a temporary file and then read it back
#temp_boxplot_file <- tempfile(fileext = ".png")
ggplot(temperature_df, aes(x = Image, y = Temperature)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Temperature Values by Thermal Image (Sorted by Average)",
       x = "Thermal Image", y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
#ggsave(temp_boxplot_file, width = 8, height = 6) # Save the boxplot as a PNG file

# Read the saved boxplot back into the PDF
#img <- png::readPNG(temp_boxplot_file)
#grid::grid.raster(img)

# correlation
#install.packages("raster")

library(raster)
library(corrplot)

# Initialize a list to store image stacks
image_list <- list()

# Loop through each file and read the images into the list
for (i in 1:36) {
  img_path <- paste0("C:/Users/TEK1/OneDrive - Syracuse University/syracuse university/4 semester/APM595 probability/HW/HW6/5band images/", i, ".tif")
  
  # Check if the file exists
  if (file.exists(img_path)) {
    img <- stack(img_path)  # Load the image stack
    
    # Check if the image has 5 layers
    if (nlayers(img) == 5) {
      image_list[[i]] <- img  # Store the valid image stack in the list
    } else {
      cat("Image", i, "does not have 5 bands.\n")
    }
  } else {
    cat("File does not exist:", img_path, "\n")
  }
}

# Initialize the average correlation matrix
avg_correlation <- matrix(0, nrow = 5, ncol = 5)
valid_image_count <- 0  # Counter for valid images

# Calculate the average correlation across all valid images
for (img in image_list) {
  band_correlation <- cor(as.data.frame(img))
  avg_correlation <- avg_correlation + band_correlation
  valid_image_count <- valid_image_count + 1  # Increment the valid image count
}

# Define band names
band_names <- c("Red", "Green", "Blue", "NIR", "LWIR")

# Compute the average correlation matrix
if (valid_image_count > 0) {
  avg_correlation <- avg_correlation / valid_image_count  # Divide by the number of valid images
  
  # Plotting the average correlation matrix
  colnames(avg_correlation) <- band_names  # Set column names
  rownames(avg_correlation) <- band_names  # Set row names
  corrplot(avg_correlation, method = "color", title = "Average Correlation Matrix of Bands Across Images", tl.col = "black", tl.srt = 45)
} else {
  cat("No valid images to compute correlation.\n")
}


dev.off()




#line plot
# Line plot for each matrix
for (i in seq_along(submatrix_averages)) {
  plot(1:10, submatrix_averages[[i]], type = "b", col = "blue", 
       main = paste("Temperature Trend Across Submatrices - Matrix", i),
       xlab = "Submatrix", ylab = "Avg Temperature (°C)")
}

#density
# Plot density of all submatrix averages
ggplot(data.frame(AverageTemp = all_submatrix_averages), aes(x = AverageTemp)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Density Plot of Temperature Averages Across All Submatrices",
       x = "Average Temperature (°C)", y = "Density") +
  theme_minimal()


#heatmap
install.packages("tidyverse")

library(ggplot2)
library(tidyr)
library(dplyr)

for (i in seq_along(matrices_list)) {
  # Reshape the averages for the 10 submatrices into a 2D matrix
  avg_matrix <- matrix(submatrix_averages[[i]], nrow = 2, ncol = 5, byrow = TRUE)
  
  # Convert the matrix into a data frame for ggplot
  avg_df <- as.data.frame(avg_matrix)
  avg_df <- avg_df %>% mutate(Row = row_number()) %>% pivot_longer(-Row, names_to = "Column", values_to = "AverageTemp")
  
  # Convert Column to numeric for plotting
  avg_df$Column <- as.numeric(gsub("V", "", avg_df$Column))
  
  # Plot heatmap
  ggplot(avg_df, aes(x = Column, y = Row, fill = AverageTemp)) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = paste("Heatmap of Temperature Averages - Matrix", i),
         x = "Column", y = "Row", fill = "Avg Temp (°C)") +
    theme_minimal()
}




# correlation
install.packages("raster")

library(raster)
library(corrplot)

# Initialize a list to store image stacks
image_list <- list()

# Loop through each file and read the images into the list
for (i in 1:36) {
  img_path <- paste0("C:/Users/TEK1/OneDrive - Syracuse University/syracuse university/4 semester/APM595 probability/HW/HW6/5band images/", i, ".tif")
  
  # Check if the file exists
  if (file.exists(img_path)) {
    img <- stack(img_path)  # Load the image stack
    
    # Check if the image has 5 layers
    if (nlayers(img) == 5) {
      image_list[[i]] <- img  # Store the valid image stack in the list
    } else {
      cat("Image", i, "does not have 5 bands.\n")
    }
  } else {
    cat("File does not exist:", img_path, "\n")
  }
}

# Initialize the average correlation matrix
avg_correlation <- matrix(0, nrow = 5, ncol = 5)
valid_image_count <- 0  # Counter for valid images

# Calculate the average correlation across all valid images
for (img in image_list) {
  band_correlation <- cor(as.data.frame(img))
  avg_correlation <- avg_correlation + band_correlation
  valid_image_count <- valid_image_count + 1  # Increment the valid image count
}

# Define band names
band_names <- c("Red", "Green", "Blue", "NIR", "LWIR")

# Compute the average correlation matrix
if (valid_image_count > 0) {
  avg_correlation <- avg_correlation / valid_image_count  # Divide by the number of valid images
  
  # Plotting the average correlation matrix
  colnames(avg_correlation) <- band_names  # Set column names
  rownames(avg_correlation) <- band_names  # Set row names
  corrplot(avg_correlation, method = "color", title = "Average Correlation Matrix of Bands Across Images", tl.col = "black", tl.srt = 45)
} else {
  cat("No valid images to compute correlation.\n")
}

