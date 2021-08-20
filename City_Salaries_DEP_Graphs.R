rm(list=ls())

library(bit64)
library(data.table)
library(ggplot2)

inputdirectory <- 'C:/Data/'
inputfile <- 'Citywide_Payroll_DEP_20210810.RDS'


DEPdataset <- readRDS(paste0(inputdirectory, inputfile))

# Convert some fields to more usable data types.
DEPdataset[, `:=` (`Agency Start Date` = as.IDate(`Agency Start Date`, format = '%m/%d/%Y'),
                   `Base_Salary_n` = as.numeric(gsub(',', '', `Base Salary`)))]

# Give each employee a synthetic ID. Note that people can move across agencies, and can also
# share the same names.
DEPdataset[, SynID := .GRP, by = c('Last Name', 'First Name', 'Mid Init', 'Agency Start Date')]

DEPdataset[, DupFY := duplicated(DEPdataset, by = c('SynID', 'Fiscal Year'))]
DupSet <- DEPdataset[DupFY == TRUE]

Num_ID <- DEPdataset[, max(SynID)]

Harris <- DEPdataset[`Last Name` == 'LAM' & `First Name` == 'HARRIS'][order (`Fiscal Year`)]
LuLiu <- DEPdataset[`Last Name` == 'LIU' & `First Name` == 'LU'][order (`Fiscal Year`)]
Mike <- DEPdataset[`Last Name` == 'MORAN' & `First Name` == 'MICHAEL'][order (`Fiscal Year`)]
Belinda <- DEPdataset[`Last Name` == 'LI' & `First Name` == 'JUN'][order (`Fiscal Year`)]

ggplot(Harris, aes(x = `Fiscal Year`, Base_Salary_n)) + geom_line()
ggplot(LuLiu, aes(x = `Fiscal Year`, Base_Salary_n)) + geom_line()

ggplot(DEPdataset, aes(x = `Fiscal Year`, y = Base_Salary_n, group = `Fiscal Year`)) + geom_boxplot()




