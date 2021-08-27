rm(list=ls())

library(bit64)
library(data.table)
library(ggplot2)
library(openxlsx)

inputdirectory <- 'd:/Data/'
outputdirectory <- 'd:/Data/'
inputfile <- 'Citywide_Payroll_DEP_20210810.RDS'
outputprefix <- 'CP_'

# Create worksheet format styles
# Some formatting afterwards.
Style0 <- createStyle(numFmt = '0')
StyleH <- createStyle(textDecoration = 'bold', fontColour = 'white', bgFill = 'black')

Save_to_XLSX <- function(Table2Save, TabName, FileName)
{
  # Format some of the RTS Table beforehand.
  NumRows <- nrow(Table2Save)
  NumCols <- ncol(Table2Save)
  
  # Write the RTS Table into a worksheet.
  RTSBook <- createWorkbook()
  TabName <- paste0(TabName)
  addWorksheet(RTSBook, TabName)
  writeDataTable(RTSBook, TabName, Table2Save, tableStyle = 'none')
  
  # Some formatting afterwards.
  addStyle(RTSBook, TabName, style = Style0, rows = 1:NumRows+1, cols = match('acct', names(Table2Save)), gridExpand = TRUE, stack = TRUE)
  addStyle(RTSBook, TabName, style = StyleH, rows = 1, cols = 1:NumCols, gridExpand = TRUE, stack = TRUE)
  
  # Auto-width everything
  setColWidths(RTSBook, TabName, 1:NumCols, widths = 'auto', ignoreMergedCells = TRUE)
  
  # Freeze Panes
  freezePane(RTSBook, TabName, firstRow = TRUE, firstCol = TRUE)
  
  # Write the table to an XLSX spreadsheet
  saveWorkbook(RTSBook, paste0(outputdirectory, outputprefix, FileName, '.xlsx'), overwrite = TRUE)
}

DEPdataset <- readRDS(paste0(inputdirectory, inputfile))

# Convert some fields to more usable data types, and add a row number.
ConvertCols <- c('Base Salary', 'OT Hours', 'Regular Hours', 'Regular Gross Paid', 
                 'Total Other Pay', 'Total OT Paid')
DEPdataset[, (ConvertCols) := lapply(.SD, gsub, pattern = ',', replacement = ''), .SDcols=ConvertCols][
           , (ConvertCols) := lapply(.SD, as.numeric), .SDcols=ConvertCols]
DEPdataset[, `:=` (`Agency Start Date` = as.IDate(`Agency Start Date`, format = '%m/%d/%Y'),
                RowNum = .I)]

# Give each employee a synthetic ID. Note that people can move across agencies, and can also
# share the same names.
DEPdataset[, SynID := .GRP, by = c('Last Name', 'First Name', 'Mid Init', 'Agency Start Date')]

# Does anyone here have multiple entries in a year?
DEPdataset[, DupFY := duplicated(DEPdataset, by = c('SynID', 'Fiscal Year'))]
DupSet <- DEPdataset[DupFY == TRUE]

Num_ID <- DEPdataset[, max(SynID)]

DEP_No_Regular_Gross <- DEPdataset[`Regular Gross Paid` <= 0]
DEP_Regular_Gross <- DEPdataset[!DEP_No_Regular_Gross, on = 'RowNum']
DEP_Per_Annum_Active <- DEP_Regular_Gross[`Pay Basis` == 'per Annum' & `Leave Status as of June 30` == 'ACTIVE']

Harris <- DEPdataset[`Last Name` == 'LAM' & `First Name` == 'HARRIS'][order (`Fiscal Year`)]

# Do some salary math per each synthetic ID.
# Changing NA to values to 0 is important, otherwise the cumulative sum function doesn't work.
# Done in blocks because upstream variables must be calculated first.
setorder(DEPdataset, `Fiscal Year`)
DEPdataset[, SalaryChange := `Regular Gross Paid` - shift(`Regular Gross Paid`, 1), by = SynID][
           is.na(SalaryChange), SalaryChange := 0L][
           , `:=` (Pct_SalaryChange = SalaryChange / shift(`Regular Gross Paid`, 1),
                   Cum_Salary_Change = cumsum(SalaryChange)), by = SynID][
           , Pct_Cum_Salary_Change := Cum_Salary_Change / `Regular Gross Paid`[1], by = SynID]
                  
Save_to_XLSX(DEPdataset, 'DEPdataset', 'DEPdataset')

# Figure out who was an active Admin Staff Analyst in FY 2020
AdminStaffAnalystIDs <- DEPdataset[`Fiscal Year` == 2020L & 
                        `Title Description` == 'ADMINISTRATIVE STAFF ANALYST' & 
                        `Leave Status as of June 30` == 'ACTIVE', SynID]
AdminSA <- DEPdataset[SynID %in% AdminStaffAnalystIDs]
ggplot(AdminSA, aes(x = `Fiscal Year`, y = `Regular Gross Paid`, group = `Fiscal Year`)) + geom_boxplot()

AdminSA_7Yr_Growth <- AdminSA[`Fiscal Year` == 2020L][, .(`Last Name`, Cum_Salary_Change, Pct_Cum_Salary_Change = Pct_Cum_Salary_Change * 100)]

AdminSA_7Yr_Growth <- head(AdminSA_7Yr_Growth, 10)

ggplot(AdminSA_7Yr_Growth, aes(x = `Last Name`, y = 'Cum_Salary_Change')) + 
  geom_bar(stat = 'identity')
  
  
ggplot(AdminSA_7Yr_Growth, aes(x = `Last Name`, y = 'Pct_Cum_Salary_Change')) + geom_line()


# # Changing NA to values to 0 is important, otherwise cumsum function doesn't work.
# Harris[, SalaryChange := `Regular Gross Paid` - shift(`Regular Gross Paid`, 1)]
# Harris[is.na(SalaryChange), SalaryChange := 0L] 
# Harris[, Pct_SalaryChange := SalaryChange / shift(`Regular Gross Paid`, 1)]
# Harris[, Cum_Salary_Change := cumsum(SalaryChange)]
# Harris[, Pct_Cum_Salary_Change := Cum_Salary_Change / `Regular Gross Paid`[1]]
# # Harris[is.na(Pct_SalaryChange), Pct_SalaryChange := 0L] 
# # 
# 
Save_to_XLSX(Harris, 'Harris', 'Harris')

LuLiu <- DEPdataset[`Last Name` == 'LIU' & `First Name` == 'LU'][order (`Fiscal Year`)]
Save_to_XLSX(LuLiu, 'LuLiu', 'LuLiu')

Carol <- DEPdataset[`Last Name` == 'DAVIS' & `First Name` == 'CAROL'][order (`Fiscal Year`)]
Save_to_XLSX(Carol, 'Carol', 'Carol')

Christine <- DEPdataset[`Last Name` == 'SAM' & `First Name` == 'CHRISTINE'][order (`Fiscal Year`)]
Save_to_XLSX(Christine, 'Christine', 'Christine')


Mike <- DEPdataset[`Last Name` == 'MORAN' & `First Name` == 'MICHAEL'][order (`Fiscal Year`)]
Belinda <- DEPdataset[`Last Name` == 'LI' & `First Name` == 'JUN'][order (`Fiscal Year`)]
Save_to_XLSX(Belinda, 'Belinda', 'Belinda')

ggplot(Harris, aes(x = `Fiscal Year`, PctChange)) + geom_line()
ggplot(LuLiu, aes(x = `Fiscal Year`, `Base Salary`)) + geom_line()
ggplot(LuLiu, aes(x = `Fiscal Year`, PctChange)) + geom_line()

# Boxplot of FY 2014 to FY 2020 of Regular Gross Paid
ggplot(DEPdataset, aes(x = `Fiscal Year`, y = `Regular Gross Paid`, group = `Fiscal Year`)) + geom_boxplot()

ggplot(DEP_Per_Annum_Active, aes(x = `Fiscal Year`, y = `Regular Gross Paid`, group = `Fiscal Year`)) + geom_boxplot()





# Create data
data <- data.frame(
  name=c("A","B","C","D","E") ,  
  value=c(3,12,5,18,45)
)

# Barplot
ggplot(data, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

