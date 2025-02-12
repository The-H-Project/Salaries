rm(list=ls())

library(bit64)
library(data.table)
library(ggplot2)
library(ggrepel)
library(scales)
library(lubridate)
library(openxlsx)

# Find out which platform we're running under.
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Set environmental variables based on platform.
os <- get_os()
if (os == 'osx')
{
  inputdirectory <- '/Volumes/Corsair/Data/'
  outputdirectory <- '/Volumes/Corsair/Data/'
} else if (os == 'windows')
{
  inputdirectory <- 'c:/Data/'
  outputdirectory <- 'c:/Data/'
  Sys.setenv('R_ZIPCMD' = 'c:/rtools/bin/zip.exe')
} else
{
  stop('Operating System unknown.')
}


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

# From Stack Overflow:
# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
rounder <- function(x,y) {
  if(y >= 0) { x + (y - x %% y)}
  else { x - (x %% abs(y))}
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

# Does anyone here have multiple entries in a year? This does happen.
DEPdataset[, DupFY := duplicated(DEPdataset, by = c('SynID', 'Fiscal Year'))]
DupSet <- DEPdataset[DupFY == TRUE]

Num_ID <- DEPdataset[, max(SynID)]

DEP_No_Regular_Gross <- DEPdataset[`Regular Gross Paid` <= 0]
DEP_Regular_Gross <- DEPdataset[!DEP_No_Regular_Gross, on = 'RowNum']
DEP_Per_Annum_Active <- DEP_Regular_Gross[`Pay Basis` == 'per Annum' & `Leave Status as of June 30` == 'ACTIVE']

# Do some salary math per each synthetic ID.
# Changing NA to values to 0 is important, otherwise the cumulative sum function doesn't work.
# Done in blocks because upstream variables must be calculated first.
# Note that OT_Util_Rate has NaN values where division by 0 occurs. No adjustments yet because
# we're not doing anything with the data.
setorder(DEPdataset, `Fiscal Year`)
DEPdataset[, `:=` (SalaryChange = `Base Salary` - shift(`Base Salary`, 1),
                   OT_Util_Rate = `OT Hours` / (`OT Hours` + `Regular Hours`),
                   B_Sal_Inc = 0), by = SynID][
           SalaryChange > 0, B_Sal_Inc := 1][   
           is.na(SalaryChange), SalaryChange := 0L][
           , `:=` (Pct_SalaryChange = SalaryChange / shift(`Base Salary`, 1),
                   Cum_Salary_Change = cumsum(SalaryChange)), by = SynID][
           , Pct_Cum_Salary_Change := Cum_Salary_Change / `Base Salary`[1], by = SynID]
Save_to_XLSX(DEPdataset, 'DEPdataset', 'DEPdataset')


DEPActive2020 <- DEPdataset[`Fiscal Year` == 2020L & `Leave Status as of June 30` == 'ACTIVE' & `Pay Basis` == 'per Annum']
DEPActive2020[, Yrs_Svc := interval(`Agency Start Date`, as.IDate('2020-06-30')) / years(1)]
Graph03 <- ggplot(DEPActive2020, aes(x = Pct_Cum_Salary_Change, y = Cum_Salary_Change, size = Yrs_Svc)) + geom_point() +
  xlab('Percentage Cumulative Change') +
  ylab('Cumulative Salary Change ($)') +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar(negative_parens = TRUE))
print(Graph03)


# Figure out who was an active Admin Staff Analyst at the end of FY 2020, and:
# - number of years of total service at DEP
# - number of title changes 
AdminStaffAnalystIDs <- DEPdataset[`Fiscal Year` == 2020L & 
                        `Title Description` == 'ADMINISTRATIVE STAFF ANALYST' & 
                        `Leave Status as of June 30` == 'ACTIVE', SynID]
AdminSA <- DEPdataset[SynID %in% AdminStaffAnalystIDs]

# Summary statistics Table.
AdminSA_Stats <- AdminSA[, .(NumTitleChanges = uniqueN(`Title Description`) - 1,
                             Sal_Inc = sum(B_Sal_Inc),
                             Min_Base_Salary = min(`Base Salary`),
                             Max_Base_Salary = max(`Base Salary`)), by = SynID]
                                 
# DEP Admin Staff Analyst plots
# Graph 01: Boxplot
Graph01_Max_Y <- AdminSA[, rounder(max(`Base Salary`), 50000)]
Graph01 <- ggplot(AdminSA, aes(x = `Fiscal Year`, y = `Base Salary`, group = `Fiscal Year`)) + 
  geom_boxplot() +
  labs(title = 'DEP Admin Staff Analysts: Base Salaries FY 2014 to 2020') +
  scale_y_continuous(limits = c(25000, Graph01_Max_Y), 
                     labels = label_dollar(negative_parens = TRUE)) 
print(Graph01)

# Graph 02: BoxPlot 2
# 1. Summary table for graph. Use Fiscal Year 2020 to get the final cumulative salary change, 
#    percent cumulative salary change, etc.
# 2. Merge with Stat Table
# 3. Sort by Cumulative Salary Change
AdminSA_7Yr_Growth <- AdminSA[`Fiscal Year` == 2020L][
  , .(`Last Name`, `First Name`, Cum_Salary_Change, Pct_Cum_Salary_Change, 
      SCM = Pct_Cum_Salary_Change + 1,
      Yrs_Svc = interval(`Agency Start Date`, as.IDate('2020-06-30')) / years(1)), by = SynID][
    AdminSA_Stats, on ='SynID'][
    order (Cum_Salary_Change)]

Graph02_Max_Y <- AdminSA[, rounder(max(Cum_Salary_Change), 25000)]
Graph02 <- ggplot(AdminSA_7Yr_Growth, aes(x = SCM, y = Cum_Salary_Change, col = Sal_Inc, size = Yrs_Svc)) + 
  geom_point() +
  labs(title = 'DEP Admin Staff Analysts: Cumulative Salary Change FY 2014 to 2020',
       subtitle = 'for Active Staff as of 6/30/2020') +
  xlab('Salary Growth Multiple from 6/30/2014 to 6/30/2020') +
  ylab('Cumulative Salary Change ($)') +
  scale_x_continuous() +
  scale_y_continuous(limits = c(0, Graph02_Max_Y), labels = label_dollar(negative_parens = TRUE)) +
  scale_color_gradientn(colors = blues9) 
print(Graph02)

# Who are the people with a base salary growth of more than 1.6x since 2014?
AdminSA_HighGrowth <- AdminSA_7Yr_Growth[SCM >= 1.6]
Save_to_XLSX(AdminSA_HighGrowth, 'AdminSA_HG', 'AdminSA_High_Growth')

Graph02a_Max_Y <- AdminSA_HighGrowth[, rounder(max(Cum_Salary_Change), 20000)]
Graph02a_Min_Y <- AdminSA_HighGrowth[, rounder(min(Cum_Salary_Change), -20000)]
Graph02a <- ggplot(AdminSA_HighGrowth, aes(x = SCM, y = Cum_Salary_Change, col = Sal_Inc, size = Yrs_Svc)) + 
  geom_point() +
  labs(title = 'DEP Admin Staff Analysts: Cumulative Salary Change FY 2014 to 2020',
       subtitle = 'Active Staff as of 6/30/2020 with Salary Growth >= 1.6x') +
  xlab('Salary Change Multiple from 6/30/2014 to 6/30/2020') +
  ylab('Cumulative Salary Change ($)') +
  scale_x_continuous() +
  scale_y_continuous(limits = c(Graph02a_Min_Y, Graph02a_Max_Y), labels = label_dollar(negative_parens = TRUE)) +
  scale_color_gradientn(colors = blues9) +
  # scale_color_gradientn(colors = blues9, breaks = c(0,1,2), labels = c('0', '1', '2'), limits=c(0,2)) +
  geom_text_repel(data = AdminSA_HighGrowth, color = 'black',
            aes(x = SCM, y = Cum_Salary_Change, 
                label = c(paste0(`First Name`, ' ', `Last Name`)))) +
  scale_size(range = c(2, 4))
print(Graph02a)

# Graph03 <- ggplot(AdminSA, aes(x = `Fiscal Year`, y = 'OT_Util_Rate')) + geom_point() +
#   scale_y_continuous(limits = c(0, .99)) 
# print(Graph03)
# max(AdminSA$OT_Util_Rate)
# 
# Graph03 <- ggplot()

EveryoneID <- DEPdataset[`Fiscal Year` == 2020L & 
                      `Leave Status as of June 30` == 'ACTIVE', SynID]
EveryoneTable <- DEPdataset[SynID %in% EveryoneID]





# # Still the same problem.  
# AdminSATest <- AdminSA_7Yr_Growth[, `Last Name` := reorder(`Last Name`, Cum_Salary_Change)]
# ggplot(AdminSATest, aes(x = `Last Name`, y = Cum_Salary_Change)) + 
#   geom_bar(stat = 'identity') 
# 
# AdminTest1 <- AdminSA[Cum_Salary_Change > 100000]
# Save_to_XLSX(AdminTest1, 'AdminTest1', 'AdminTest1')


#ggplot(AdminSA_7Yr_Growth, aes(x = Cum_Salary_Change, y = 'Pct_Cum_Salary_Change')) + geom_line()


# # Changing NA to values to 0 is important, otherwise cumsum function doesn't work.
# Harris[, SalaryChange := `Regular Gross Paid` - shift(`Regular Gross Paid`, 1)]
# Harris[is.na(SalaryChange), SalaryChange := 0L] 
# Harris[, Pct_SalaryChange := SalaryChange / shift(`Regular Gross Paid`, 1)]
# Harris[, Cum_Salary_Change := cumsum(SalaryChange)]
# Harris[, Pct_Cum_Salary_Change := Cum_Salary_Change / `Regular Gross Paid`[1]]
# # Harris[is.na(Pct_SalaryChange), Pct_SalaryChange := 0L] 
# # 
# 
# Save_to_XLSX(Harris, 'Harris', 'Harris')

Harris <- DEPdataset[`Last Name` == 'LAM' & `First Name` == 'HARRIS'][order (`Fiscal Year`)]
Save_to_XLSX(Harris, 'Harris', 'Harris')


Harris_7 <- AdminSA_7Yr_Growth[`Last Name` == 'LAM' & `First Name` == 'HARRIS']
Save_to_XLSX(Harris_7, 'Harris_7', 'Harris_7')
# 
# 
LuLiu <- DEPdataset[`Last Name` == 'LIU' & `First Name` == 'LU'][order (`Fiscal Year`)]
Save_to_XLSX(LuLiu, 'LuLiu', 'LuLiu')

LuLiu_7 <- AdminSA_7Yr_Growth[`Last Name` == 'LIU' & `First Name` == 'LU']
Save_to_XLSX(LuLiu_7, 'LuLiu_7', 'LuLiu_7')


# 
Carol <- DEPdataset[`Last Name` == 'DAVIS' & `First Name` == 'CAROL'][order (`Fiscal Year`)]
Save_to_XLSX(Carol, 'Carol', 'Carol')

Christine <- DEPdataset[`Last Name` == 'SAM' & `First Name` == 'CHRISTINE'][order (`Fiscal Year`)]
Save_to_XLSX(Christine, 'Christine', 'Christine')


# 
# 
# Mike <- DEPdataset[`Last Name` == 'MORAN' & `First Name` == 'MICHAEL'][order (`Fiscal Year`)]
Belinda <- DEPdataset[`Last Name` == 'LI' & `First Name` == 'JUN'][order (`Fiscal Year`)]
Save_to_XLSX(Belinda, 'Belinda', 'Belinda')

# ggplot(Harris, aes(x = `Fiscal Year`, PctChange)) + geom_line()
# ggplot(LuLiu, aes(x = `Fiscal Year`, `Base Salary`)) + geom_line()
# ggplot(LuLiu, aes(x = `Fiscal Year`, PctChange)) + geom_line()

# Boxplot of FY 2014 to FY 2020 of Regular Gross Paid
ggplot(DEPdataset, aes(x = `Fiscal Year`, y = `Regular Gross Paid`, group = `Fiscal Year`)) + geom_boxplot()

ggplot(DEP_Per_Annum_Active, aes(x = `Fiscal Year`, y = `Regular Gross Paid`, group = `Fiscal Year`)) + geom_boxplot()


AdminMgr_ID <- DEPdataset[`Fiscal Year` == 2020L & 
                                     `Title Description` == 'ADM MANAGER-NON-MGRL' & 
                                     `Leave Status as of June 30` == 'ACTIVE', SynID]
AdminMgr_data <- DEPdataset[SynID %in% AdminMgr_ID]


# Graph 01: Boxplot
Graph10_Max_Y <- AdminMgr_data[, rounder(max(`Base Salary`), 50000)]
Graph10_Min_Y <- AdminMgr_data[, min(Cum_Salary_Change)]

Graph10 <- ggplot(AdminMgr_data, aes(x = `Fiscal Year`, y = `Base Salary`, group = `Fiscal Year`)) + 
  geom_boxplot() +
  labs(title = 'DEP Admin Managers (Non-Managerial): Base Salaries FY 2014 to 2020') +
  scale_y_continuous(limits = c(25000, Graph10_Max_Y), 
                     labels = label_dollar(negative_parens = TRUE)) 
print(Graph10)

