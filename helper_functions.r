## @param point_matrix a matrix where each row is a n-dimensional point sampled. columns are muscles.
## @return val a list of histogramdatastructures
histogram_all_columns <- function(point_matrix, binwidth=0.02){
	iterator <- 1:ncol(point_matrix)
	lapply(
		iterator,
		function(i){
				hist(point_matrix[,i], breaks=seq(0,1,by=binwidth), plot=FALSE)
			}
	)
}

get_nth_element_from_all_sublists <- function(big_list, n) {
	lapply(big_list, function(sublist) {sublist[n]})
}

#by default, it gets only the length of the first sublist.
get_length_of_sublist <- function(big_list, n=1){
	return(ncol(big_list[[n]]))
}

get_nth_element_of_sublists <- function(list_of_sublists, n){
	lapply(list_of_sublists, 
		function(x) {
			x[[n]]
			})
}

get_matrix_of_counts <- function(list_of_histograms_for_a_given_muscle){
	res <- lapply(list_of_histograms_for_a_given_muscle, 
		function(x) {
			x$count
		})
	return(do.call(rbind,res)) #convert from list of vectors into matrix (index=>rownum)
}
library(fields)


plot_force_progression_map <- function(histogram_progression_matrix, log_the_values=FALSE){
	#force 0 to be pure black, and next to zero to be bright orange.
	color_ramp <- colorRampPalette(c("#27499b","#2d7cb0","#45afba","#90d0b2", "#eaf2c3", "#ffffff"))(99)
	color_ramp <- c("#000000", color_ramp) #append black as 0 for contrast
	if (log_the_values) {
		histogram_progression_matrix <- log10(histogram_progression_matrix + 1)
	}
	fields::image.plot(histogram_progression_matrix, col= color_ramp, axes=FALSE,xlab="",ylab="", horizontal=TRUE )
	axes(histogram_progression_matrix,0,1)
}

get_unix_time_string_now <- function()
	{return(as.character(as.numeric(Sys.time())))}

density_normalization <- function(list_of_histogram_matrices_count) {
	sample_size = sum(list_of_histogram_matrices_count[1,])
	return(list_of_histogram_matrices_count/sample_size)
}

#for many files in one directory
get_list_of_point_matrices <- function(filename_list, folder_path){
  list_of_point_matrices <- lapply(
   filename_list,
   function(i){
     read.csv(paste0(folder_path,i), header=FALSE)
   })
  return(list_of_point_matrices)
}




# here we are calculating the percent of explained variance for each principal component. 
#To this plot, we will add a line that indicates the amount of variance each variable 
# would contribute if all contributed the same amount.
# https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/#ref2
pca_muscle_solution_space <- function(filename, folder_path){
	dataset <- read.csv(paste0(folder_path,filename), header=FALSE)
	# log transform 
	require(caret)
	number_of_muscles = length(dataset[1,])
	dataset.pca <- prcomp(dataset, retx=TRUE, center=TRUE, scale.=TRUE)
	sd <- dataset.pca$sdev
	loadings <- dataset.pca$rotation
	rownames(loadings) <- colnames(dataset)
	scores <- dataset.pca$x
	var <- sd^2
	var.percent <- var/sum(var) * 100
	dev.new()
	barplot(var.percent, xlab="PC", ylab="Percent Variance", main=filename, names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), col="gray")
	abline(h=1/ncol(dataset)*100, col="red")
	print(loadings)
}

plot_histogram_matrices <- function(list_of_histogram_matrices){
	require(rgl)
	open3d()
	mat <- t(matrix(1:8,ncol=4))
	layout3d(mat, sharedMouse = TRUE)
	for (i in 1:7) {
	  next3d()
	  plot_histogram_matrix_3d(list_of_histogram_matrices[[i]], 100)
	  next3d()
	  
	}
	highlevel(integer())
}

plot_histogram_matrices_same_fig <- function(list_of_histogram_matrices){
	require(rgl)
	open3d()
	mat <- t(matrix(1:8,ncol=4))
	layout3d(mat, sharedMouse = TRUE)
	for (i in 1:7) {
	  plot_histogram_matrix_3d(list_of_histogram_matrices[[i]], 100)
	}
	highlevel(integer())
}
