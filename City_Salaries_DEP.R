
library(bit64)
library(data.table)

inputdirectory <- 'C:/Data/'
inputfile <- 'Citywide_Payroll_20210810.RDS'

dataset <- fread(paste0(inputdirectory, inputfile))

DEP_dataset <- dataset[`Payroll Number` == 826]
