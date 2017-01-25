
library(testthat)
#library("pkg")
#test_package("pkg")
source('vectormap.r')
source('distal_progression_csv_filename_list.r')

test_that('Sort the list in numerical ascending order', {
	test_list <- c(
				"finger_forcevector_0.0_1484767835427.csv",
				"finger_forcevector_0.461446320481747_1484785251285.csv",
				"finger_forcevector_0.778690665812948_1484788980242.csv",
				"finger_forcevector_0.922892640963494_1484768166294.csv",
				"finger_forcevector_0.951733035993603_1484768498216.csv",
				"finger_forcevector_0.2307231602408735_1484782424122.csv",
				"finger_forcevector_0.2595635552709827_1484782739603.csv",
				"finger_forcevector_0.3749251353914194_1484784223650.csv",
				"finger_forcevector_0.4326059254516378_1484784920080.csv",
				"finger_forcevector_0.4902867155118562_1484785589496.csv",
				"finger_forcevector_0.5191271105419654_1484785918124.csv"
		)
	sorted_list <- c(
			"finger_forcevector_0.0_1484767835427.csv",
			"finger_forcevector_0.2307231602408735_1484782424122.csv",
			"finger_forcevector_0.2595635552709827_1484782739603.csv",
			"finger_forcevector_0.3749251353914194_1484784223650.csv",
			"finger_forcevector_0.4326059254516378_1484784920080.csv",
			"finger_forcevector_0.461446320481747_1484785251285.csv",
			"finger_forcevector_0.4902867155118562_1484785589496.csv",
			"finger_forcevector_0.5191271105419654_1484785918124.csv",
			"finger_forcevector_0.778690665812948_1484788980242.csv",
			"finger_forcevector_0.922892640963494_1484768166294.csv",
			"finger_forcevector_0.951733035993603_1484768498216.csv"
		)
		test_list_sorted <- order_filenames_by_force_number(test_list)
		expect_that(test_list_sorted,  is_equivalent_to(sorted_list) )
	
	test_list_2 <- c(

			"finger_forcevector_23.995208665050843_1484884791368.csv",
			"finger_forcevector_24.1682510352315_1484886237840.csv",
			"finger_forcevector_24.02404906008095_1484885029145.csv",
			"finger_forcevector_24.4854953805627_1484888866823.csv",
			"finger_forcevector_24.05288945511106_1484885283471.csv",
			"finger_forcevector_24.8027397258939_1484891278765.csv",
			"finger_forcevector_24.08172985014117_1484885522449.csv",
			"finger_forcevector_24.11057024517128_1484885761127.csv",
			"finger_forcevector_24.39897419547237_1484888174273.csv",
			"finger_forcevector_24.45665498553259_1484888635514.csv",
			"finger_forcevector_24.51433577559281_1484889097584.csv",
			"finger_forcevector_24.68737814577346_1484890484115.csv",
			"finger_forcevector_24.74505893583368_1484890940755.csv",
			"finger_forcevector_24.77389933086379_1484891129558.csv"
		)

	sorted_list_2 <- c(
			"finger_forcevector_23.995208665050843_1484884791368.csv",
			"finger_forcevector_24.02404906008095_1484885029145.csv",
			"finger_forcevector_24.05288945511106_1484885283471.csv",
			"finger_forcevector_24.08172985014117_1484885522449.csv",
			"finger_forcevector_24.11057024517128_1484885761127.csv",
			"finger_forcevector_24.1682510352315_1484886237840.csv",
			"finger_forcevector_24.39897419547237_1484888174273.csv",
			"finger_forcevector_24.45665498553259_1484888635514.csv",
			"finger_forcevector_24.4854953805627_1484888866823.csv",
			"finger_forcevector_24.51433577559281_1484889097584.csv",
			"finger_forcevector_24.68737814577346_1484890484115.csv",
			"finger_forcevector_24.74505893583368_1484890940755.csv",
			"finger_forcevector_24.77389933086379_1484891129558.csv",
			"finger_forcevector_24.8027397258939_1484891278765.csv" 
		)

		test_list_sorted_2 <- order_filenames_by_force_number(test_list_2)
		expect_that(test_list_sorted_2,  is_equivalent_to(sorted_list_2) )

})
test_that('Get Numerical value',{

	test_list <- c(		
		'finger_forcevector_0.0_1484767835427.csv',
		'finger_forcevector_0.461446320481747_1484785251285.csv',
		'finger_forcevector_1.06709461611404_1484781868372.csv'
		)

	answer_list <- c(
		0.0, 0.461446320481747, 1.06709461611404
		)
	
	expect_that(extract_force_values(test_list), is_equivalent_to(answer_list))

	})


test_that('Extract force number', {

	value1 <- extract_force_number_from_filename_string('finger_forcevector_0.0_1484767835427.csv')
	value2 <- extract_force_number_from_filename_string('finger_forcevector_0.461446320481747_1484785251285.csv')
	value3 <- extract_force_number_from_filename_string('finger_forcevector_1.06709461611404_1484781868372.csv')

	expect_that(value1, matches('0.0'))
	expect_that(value2, matches('0.461446320481747'))
	expect_that(value3, matches('1.06709461611404'))
})
