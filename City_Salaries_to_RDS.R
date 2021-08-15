# Load Salaries File
# 
#
#
library(bit64)
library(data.table)

inputdirectory <- 'C:/Data/'
inputfile <- 'Citywide_Payroll_Data__Fiscal_Year_20210810.csv'
outputfile <- 'Citywide_Payroll_20210810.RDS'

dataset <- fread(paste0(inputdirectory, inputfile))

saveRDS(dataset, paste0(inputdirectory, outputfile))

test <- dataset[`Last Name` == 'LAM']
