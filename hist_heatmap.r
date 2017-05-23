source('vectormap.r')
source('distal_progression_csv_filename_list.r')
source('plot_histogram_matrix_3d.r')

source('helper_functions.r')

#main#
#if you want to use a different progression of more points, put the new filenames here
require(parallel)
# by default it's just looking in the current folder.
main <- function(filename_list, folder_path = "", binwidth=0.02, log_the_values=FALSE){
	#specify the filenames of the data for analysis
  list_of_point_matrices<- get_list_of_point_matrices(filename_list, folder_path)
	message("All point sets from CSV are in active Memory.")
	list_of_histogram_sublists <- lapply(list_of_point_matrices, histogram_all_columns, binwidth)
	
	# split the sublists and reorder into histograms by muscle
	# tasks_iterator = 1:length(finger_alpha_progression_filenames)
	# get the number of muscles and construct an iterator
	muscle_iterator = 1:get_length_of_sublist(list_of_point_matrices)
	message("All samples have been broken into histogram bins")
	list_of_histogram_progressions <- lapply(muscle_iterator, 
		function(x) {
				get_nth_element_of_sublists(list_of_histogram_sublists,x)
		}
	)
	#for each muscle, make an image
	list_of_histogram_matrices <- lapply(list_of_histogram_progressions, get_matrix_of_counts)
	list_of_histogram_matrices_density <- lapply(list_of_histogram_matrices, density_normalization)
	pdf(paste0("histogram_heatmap_time_", get_unix_time_string_now(), ".pdf"), height = 9, width = 8)
	par(mfrow=c(7,1))
	par(mar=c(0.2,0.2,0.2,0.2))
	lapply(list_of_histogram_matrices_density, plot_force_progression_map, log_the_values)
	dev.off()
	message('plotting over')
	return(list_of_histogram_matrices)
}

filenames_to_visualize <- sorted_distal_progression_csv_filename_list()
list_of_histogram_matrices<- main(filenames_to_visualize, folder_path = 'n_1000_alphalen_1000/', binwidth=0.05, log_the_values=FALSE)
plot_histogram_matrices(list_of_histogram_matrices)


rgl.clear()
plot_3d_histogram_lineup(list_of_histogram_matrices, y_scale=50, time_spacing=100, activation_spacing = 1000, interplot_spacing=21000)
bg3d('white')

#if you want to get a PCA variance_explained plot for a couple of the plots. Loadings will be printed to the console.

# dev.off()
# plot.new()
# par(mfrow=c(1,3))
# pca_muscle_solution_space('finger2.881155463796023E-71474701464678.csv', '~/Documents/GitHub/bcohn12/space/output/')
# pca_muscle_solution_space('finger23.0492437103681881475092289825.csv', '~/Documents/GitHub/bcohn12/space/output/')
# pca_muscle_solution_space('finger28.8115546350790771475101516287.csv', '~/Documents/GitHub/bcohn12/space/output/')
# pca_muscle_solution_space('finger25.9078901456739781475102067373.csv', '~/Documents/GitHub/bcohn12/space/output/')
# main(long_list[seq(0,1000, length.out=20)], folder_path = '~/Documents/GitHub/bcohn12/space/output/')