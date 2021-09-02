# Load Salaries File
# 
#
#
rm(list=ls())

library(bit64)
library(data.table)
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
  inputdirectory <- 'd:/Data/'
  outputdirectory <- 'd:/Data/'
  Sys.setenv('R_ZIPCMD' = 'c:/rtools/bin/zip.exe')
} else
{
  stop('Operating System unknown.')
}


inputfile <- 'Citywide_Payroll_Data_Fiscal_Year_20210810.csv'
outputfile <- 'Citywide_Payroll_20210810.RDS'
DEP_outputfile <- 'Citywide_Payroll_DEP_20210810.RDS'
outputprefix <- 'CP_20210810_'

dataset <- fread(paste0(inputdirectory, inputfile))

saveRDS(dataset, paste0(inputdirectory, outputfile))

test <- dataset[`Last Name` == 'WILSON' & `First Name` == 'PETA GAY']

DEP_dataset <- dataset[`Agency Name` == 'DEPT OF ENVIRONMENT PROTECTION']


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


Save_to_XLSX(test, 'PWilson', 'PWilson.xlsx')
Save_to_XLSX(DEP_dataset, 'DEP_Dataset', 'DEP_Dataset.xlsx')
saveRDS(DEP_dataset, paste0(inputdirectory, DEP_outputfile))
