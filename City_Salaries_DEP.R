
library(bit64)
library(data.table)

inputdirectory <- 'd:/Data/'
inputfile <- 'Citywide_Payroll_DEP_20210810.RDS'

dataset <- fread(paste0(inputdirectory, inputfile))


