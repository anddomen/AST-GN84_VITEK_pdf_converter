## Name of script: AST-GN84_VITEK_pdf_to_csv.R
##
## Purpose:
## Starting with the PDF files from the bioMerieux Vitek 2 Compact,
## we iterate through each PDF file, extract the necessary information
## for each isolate, and create a Master CSV file containing the
## data. 
##
## This is specifically for the AST-GN84 card type.
##
## Authors: Andrea Domen and James Molyneux
##

## REQUIRES USER INFO: filepath where pdf's are located (line 26),
## the desired master file name (line 176)
## the desired filepath for exported file (line 182) 


## Load necessary R libraries:
## tidyverse for general data manipulations
library(tidyverse)
## tabulizer for ripping tables from PDFs
library(tabulizer)

##SPECIFY FILEPATH WHERE PDF'S ARE LOCATED
user.filepath <- ""

#########################################################
## Create a function which formats the data extracted
## from the PDF files
## Note: Might need to "clean" this up slightly for the
## sake of "longevity".
##
## The idea is to scrape each PDF table and then send it
## through this function to scrub it into a nice "clean"
## data frame to work with and manipulate later on.

format_cells <- function(x) {
  # Initial goal: Coerce matrix from PDF to data.frame
  # Extract the column names
  # Column 2 is "blank" but the name of column 1 is located in column 2
  # So, we get the name from columns 2, 3, 4 but we next drop
  # column 2
  # We also need to grab the interpretation column
  column_names <- x[1, c(2, 3, 4)]
  # Take the two sets of columns and combine them into
  # one column of variables in the data frame.
  df <- as.data.frame(rbind(x[-1, c(1, 3, 4)], x[-1, 5:7]))
  # Add the names of the columns to the data.frame
  names(df) <- column_names
  
  # Find the rows which contain the words highlighted below.
  # We want the rows so that we can manipulate them later.
  sulfa_rows <- which(df$Antimicrobial == "Sulfamethoxazole")
  
  # Take the rows containing the word "Sulfamethoxazole" and add them 
  # to the name of the entry from the previous row.
  df$Antimicrobial[sulfa_rows - 1] <- paste0(df$Antimicrobial[sulfa_rows - 1], "Sulfamethoxazole")
  
  # # Drop the lone "Sulfamexthoxazole" and the blank rows
  # df[-c(which(df$Antimicrobial == ""), sulfa_rows), ]
  
  # Remove the rows in the "reverse" order of where they appear 
  # in the data. This way, we don't end up altering the number
  # of rows before removing them.
  df <- df[-sulfa_rows, ]

  # Remove blank rows since they make the data ugly
  blank_rows <- which(df$Antimicrobial == "")
  df <- df[-blank_rows, ]
  
  # Replace blank values (if there are any) with NA
  df[df == ""] <- NA
  
  # Give us the clean data back.
  return(df)
}


#########################################################
## This is where the bulk of the work happens. We want 
## to find where the PDF files are 
## located and then extract all of the names of the files
## so we can (1) scrape the data from the pdf and (2) 
## pull some of the info from the PDF file name.

## Point to the base directory containing the PDF file
setwd(user.filepath)
base_dir <- getwd()

## Extract the file names
pdf_files <- list.files(base_dir)

## Find the isolate names using the file names. Isolate
## name is the title of the PDF.
## Uses wildcard (*) to ID the isolate numbers and trim
## off the .pdf
isolate_numbers <- gsub("(*).pdf$", "\\1", pdf_files)

#########################################################
## Now, we want to loop through each isolate and 
## format the data 

## Use i = 1 so I could look at intermediate outputs
## while prototyping
#i = 1

## Define the number of files in the folder for use 
## in the for loop
files <- dir(pattern = "*.pdf")

for (i in 1:length(files)) {
  
  ## Define the "card type", which we take to always be the
  ## same value.
  card_type <- "AST-GN84"
  
  #########################################################  
  ## Our next goal is to scrape the PDF tables using tabulizer
  ## We use extract_tables(...)[[4]] to get the "4th" table 
  ## in the pdf doc, which has the MIC data. 
  ##And each doc is found using some file path trickery 
  ##inside paste0
  ecoli_pdf <- extract_tables(paste0(base_dir,"/", pdf_files[i]))[[4]]
  
  ## Use the format_cells() function we made to format the 
  ## scraped data
  ecoli_df <- format_cells(ecoli_pdf)
  
  ## Create a temporary data frame which we can "add onto" as
  ## we iterate through the isolates. Create this table by
  ## taking our combined data frames post cleaning and merging.
  ## Makes the data "wide" so antimicrobials and interpretations
  ## are columns
  tmp_df <- ecoli_df %>% 
    
    # this line breaks things and makes things look terrible,
    # have faith
    pivot_longer(cols = MIC:Interpretation, names_to = "Type") %>% 
    # this line fixes everything, lists MIC then interpretation
    # and finally pipes to mutate
    pivot_wider(names_from = c(Type, Antimicrobial), values_from = value) %>% 
    
    ## Include info about the isolate and card type
    ## Remember isolate_numbers are from the title of the file
    mutate(Isolate = isolate_numbers[i],
           Card = card_type) %>%
    ## Here, we altered one variable name to match the 
    ## original master data.
    dplyr::select(Isolate, Card, everything())
  
  ## For the first iteration only, we setup our first cleaned
  ## pdf and call it the "master" data. We then move to the 
  ## next iteration to avoid the "joining" process shown
  ## below.
  if (i == 1) {
    master_df <- tmp_df
    next 
  }
  

  ## Create our master_df by joining the new data (tmp_df)
  ## onto the original master_df
  master_df <- full_join(master_df, tmp_df) #%>% 

}

#########################################################
## Once we've iterated and created our updated master
## data file, we output that data file using the line
## of code below.

## What do you want your file to be named? Put what you
## want it called in the quotations under FILE_NAME
## make sure it ends in .csv
FILE_NAME <- "example.csv"

## glues date onto user inputted file name
output_file_name <- paste0(gsub("-", "", Sys.Date()), FILE_NAME)

## Where do you want the exported file to go?
EXPORT_FILE_PATH <- "" 

## glues desired exported path with the file name
output_file_path <- paste0(EXPORT_FILE_PATH, output_file_name)

## Export it!
## Sit back and marvel
    # Or something happened and it broke
write_csv(master_df, file = output_file_path)


