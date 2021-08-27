# Load Salaries File
# 
#
#
rm(list=ls())
library(bit64)
library(data.table)
library(ggplot2)

inputdirectory <- 'd:/Data/'
inputfile <- 'Citywide_Payroll_20210810.RDS'

dataset <- readRDS(paste0(inputdirectory, inputfile))

# Convert some fields to more usable data types, and add a row number.
ConvertCols <- c('Base Salary', 'Regular Gross Paid', 'Total Other Pay', 'Total OT Paid')
dataset[, (ConvertCols) := lapply(.SD, gsub, pattern = ',', replacement = ''), .SDcols=ConvertCols]
dataset[, (ConvertCols) := lapply(.SD, as.numeric), .SDcols=ConvertCols]

dataset[, `:=` (`Agency Start Date` = as.IDate(`Agency Start Date`, format = '%m/%d/%Y'),
                RowNum = .I)]


# Give each employee a synthetic ID. Note that people can move across agencies, and can also
# share the same names.
dataset[, SynID := .GRP, by = c('Last Name', 'First Name', 'Mid Init', 'Agency Start Date')]

No_Regular_Gross <- dataset[`Regular Gross Paid` <= 0]
Regular_Gross <- dataset[!No_Regular_Gross, on = 'RowNum']


Harris <- dataset[`Last Name` == 'LAM' & `First Name` == 'HARRIS']
Mike <- dataset[`Last Name` == 'MORAN' & `First Name` == 'MICHAEL'][order (SynID, `Fiscal Year`)]
Arthur <- dataset[`Last Name` == 'HEARD JR' & `First Name` == 'ARTHUR'][order (SynID, `Fiscal Year`)]
Carol <- dataset[`Last Name` == 'DAVIS' & `First Name` == 'CAROL'][order (SynID, `Fiscal Year`)]
Christine <- dataset[`Last Name` == 'SAM' & `First Name` == 'CHRISTINE'][order (SynID, `Fiscal Year`)]

AgencyList <- data.table(unique(dataset$`Agency Name`))

# Do people appear more than once per fiscal year?
dataset[, DupFY := duplicated(dataset, by = c('SynID', 'Fiscal Year'))]
DupSet <- dataset[DupFY == TRUE]
SampleDup <- dataset[SynID == 127787]

ggplot(Regular_Gross, aes(x = `Fiscal Year`, y = `Regular Gross Paid`, group = `Fiscal Year`)) + geom_boxplot()
