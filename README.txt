# Description of dataset variables

p_id: 		participant id
ex_id:		exercise id
ex_order:	exercise order id (1-18) 
time:		time per exercise (in seconds)
response:	participant input
difficulty:	exercise difficulty level (1 = easy, 2 = difficult)
block_order:	block order id (1 = first block, 2 = second block, 3 = third block)
block_type:	condition (1 = SwissMedicInfo, 2 = PEDeDose basic, 3 = PEDeDose with calculator)
drug:		drug asked in exercise
PEDeDose:	correct result calculated with PEDeDose
SwissMedicInfo: correct result calculated with SwissMedicInfo

true_result:	true result in regard to block_type
is_error:	does response match true_result


# Descriptive Statistics

table_1_factor:		Frequency of categorical variable subtypes (long)
table_1_age:		Mean, median and IQR of participant age

overall.total:		Errors made over all exercises and all conditions
condition.total:	Errors made over all exercises stratified by condition
exercise.total:		Errors made over all conditions stratified by exercise
ex.cond.total 		Errors made stratified by exercise and by condition