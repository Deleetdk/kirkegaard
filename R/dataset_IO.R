## DATASET AND I/O FUNCTIONS

#' Dataset merger function
#'
#' This function allows you to merge two data.frames by their overlapping rownames.
#' @param DF1 the first data.frame
#' @param DF2 the second data.frame
#' @param main which data.frame should be used as the main? Choose the larger one if working with large datasets. Default to using neither.
#' @param join (character scalar) Which data.frames to use cases from. Defaults to "both". Can be: both, left, right.
#' @keywords merging combining datasets data.frame
#' @export
#' @examples
#' merge_datasets()
merge_datasets = function (DF1, DF2, main=1, time=F, join = "both"){
  #time if desired
  if (time) {time1 = proc.time()} #start timer


  #checks
  if (!main %in% 0:2){ #check for valid input
    stop("Invalid input to main parameter provided!")
  }
  if (!join %in% c("both", "left", "right")) stop("Invalid join parameter!")


  #main setting decides how to combine
  if (join == "left") {
    DF2 = DF2[intersect(rownames(DF1), rownames(DF2)), ] #subset to overlap with DF1
  }
  if (join == "right") {
    DF1 = DF1[intersect(rownames(DF1), rownames(DF2)), ] #subset to overlap with DF2
  }

  #if nothing to join
  if (nrow(DF1) == 0) {
    message("Warning, nothing joined! No case in DF1 matches any in DF2!")
    return(DF2)
  }
  if (nrow(DF2) == 0) {
    message("Warning, nothing joined! No case in DF2 matches any in DF1!")
    return(DF1)
  }

  #combined dataset
  if (main==0){ #create a combined dataset
    #colnames, remove duplicates
    total.colnames = c(colnames(DF1), colnames(DF2))
    total.colnames.unique = unique(total.colnames)

    #rownames, remove duplicates
    total.rownames = c(rownames(DF1), rownames(DF2))
    total.rownames.unique = unique(total.rownames)

    #make DF3
  	DF3 = as.data.frame(matrix(nrow = length(total.rownames.unique),
                               ncol = length(total.colnames.unique)))
  	rownames(DF3) = sort(total.rownames.unique)
  	colnames(DF3) = total.colnames.unique
  }
  if (main==1){ #use first DF as main
	  DF3 = DF1
  }
  if (main==2){ #use second DF as main
	  DF3 = DF2
  }

  if (main!=2){
  	#loop over input dataset 2
  	for (variable in 1:length(colnames(DF2))){ #loop over variables/cols
  		for (case in 1:length(rownames(DF2))){ #loop over cases/rows
  		  if (is.na(DF2[case, variable])){ #skip if datapoint is missing
  			  next
  		  }
  		  DF3[rownames(DF2)[case], colnames(DF2)[variable]] = DF2[case, variable]
  		  #print(DF3[rownames(DF2)[case], colnames(DF2)[variable]]) #used for debugging
  		}
  	}
  }
  if (main!=1){ #if DF2 is main
	    #loop over input dataset 1
		for (variable in 1:length(colnames(DF1))){ #loop over variables/cols
			for (case in 1:length(rownames(DF1))){ #loop over cases/rows
			  if (is.na(DF1[case, variable])){ #skip if datapoint is missing
				next
			  }
		    DF3[rownames(DF1)[case], colnames(DF1)[variable]] = DF1[case, variable]
			#print(DF3[rownames(DF1)[case], colnames(DF1)[variable]]) #used for debugging
			}
		}
	}

  #output time
  if (time) {
    time2 = proc.time() - time1 #end timer
    message(time2) #print time
  }

  return(DF3)
}


#' Dataset merger function for multiple data.frames.
#'
#' This is a wrapper for merge_datasets().
#' @param ... (data.frames) Two or more data.frames to merge.
#' @keywords merging, combining, datasets, data.frame, multi, wrapper
#' @export
#' @examples
#' merge_datasets_multi()
merge_datasets_multi = function(..., main=1, time=F) {
  #wrap with Reduce
  Reduce(function(x, y) merge_datasets(x, y, main=main, time=time), list(...))

  #debugging, verify that the anonymous function works
  #(function(x, y) merge_datasets(x, y, main=main, time=time))(iris[1:50, ], iris[51:100, ])
}


#these two functions are wrappers intended to make it easier to work with the megadataset

#' A wrapper for read.csv()
#'
#' This is a convenient wrapper for read.csv()
#' @param filename the file to be read
#' @keywords wrapper read.csv
#' @export
#' @examples
#' read_mega()
read_mega = function(filename){
  return(read.csv(filename,sep=";",row.names=1, #this loads the rownames
                  stringsAsFactors=FALSE)) #dont want factors
}

#' A wrapper for write.csv()
#'
#' This is a convenient wrapper for write.csv()
#' @param object the object to be written to a file
#' @param filename the name of the file you want to write to
#' @keywords wrapper, write.csv
#' @export
#' @examples
#' write_mega()
write_mega = function(object, filename){
  datafile = cbind(ID=rownames(object), object) #adds an ID column with the rownames
  return(write.table(datafile, file = filename, #filename
            sep=";", #use tabs to separate
            na="", #no data is empty cell
            row.names = FALSE)) #add rownames
}

#' Write a nice sorted vector with names.
#'
#' This is a convenience function for outputting lists
#' @param df a data frame.
#' @param var a string with the name of the variable to write.
#' @param var a string with the desired filename to write to.
#' @keywords convenience, output
#' @export
#' @examples
#' output_sorted_var()
output_sorted_var = function(df, var, filename) {
  s = df[var] #subset
  s2 = s[order(s[1], decreasing = T),,drop=F] #sort
  s3 = data.frame(1:nrow(s2),
                  s2)
  rownames(s3) = rownames(s2)
  colnames(s3) = c("Rank", var)
  write.csv(s3, file.name, fileEncoding = "UTF-8") #save
}


#' Abbreviate country and regional names to ISO-3.
#'
#' To enable easier merging of datasets of international data. You need to download the countrylist.csv file yourself.
#' @param names A character vector of the full names of countries and regions.
#' @keywords abbreviate, names, shorten; ISO
#' @export
#' @examples
#' as_abbrev()
as_abbrev = function(names, georgia = "country"){
  #get dictionary
  codes = read.csv("countrycodes.csv", sep=";", row.names=1, encoding = "UTF-8", stringsAsFactors = F)

  #Georgia as state or country?
  if (substr(georgia, 1, 1) == "s") {
    codes["Georgia", ] = "USA_GA"
  }

  #loop thru and get abbrevs
  abbrevs = character()
  for (name in names){
    abbrevs = c(abbrevs, codes[name, ])
    if (is.na(codes[name, ])){
      print(paste(name, "is missing"))
    }
  }
  return(abbrevs)
}


#' Abbreviate country and regional names to ISO-3. For megadataset version >=3.
#'
#' To enable easier merging of datasets of international and regional data. Data file can be downloaded from https://osf.io/zdcbq/files/
#' @param x A character vector of the full names of countries and regions to abbreviate.
#' @param mega Full path to the mega file. This is the .xlsx file you downloaded above.
#' @param georgia Whether to treat georgia as a country or US state. Defaults to country. Use "state" to use as state.
#' @param miss_msg Whether to output messages when abbreviations are missing. Defaults to T.
#' @keywords abbreviate, names, shorten; ISO
#' @export
#' @examples
#' as_abbrev2()
as_abbrev2 = function(x, mega, georgia = "country", miss_msg = T) {
  library(XLConnect)
  library(stringr)

  #load mega
  wb_mega = XLConnect::loadWorkbook(mega)

  #load the dictionary sheet
  d_dict = XLConnect::readWorksheet(wb_mega, 3)

  #work around the rownames bug
  rownames(d_dict) = d_dict$Names
  d_dict$Names = NULL

  #georgia?
  #if begins with "s", then use state abbrev, otherwise use country abbrev
  if (substr(georgia, 1, 1) == "s") {
    d_dict["Georgia", "ISO3"] = "USA_GA"
  }

  #translate
  for (i in seq_along(x)) {
    #missing?
    if (is.na(d_dict[x[i], ])) {
      if (miss_msg) {
        message(str_c(x[i], " is missing from the dictionary. Add it to the megadataset file and retry."))
      }
      next
    }

    #swap to abbreviation
    x[i] = d_dict[x[i], ]

  }

  return(x)
}


#' Get full country names from ISO-3.
#'
#' To enable easier merging of datasets of international data. You need to download the countrylist.csv file yourself.
#' @param x (character vector) The ISO-3 codes.
#' @keywords names, ISO
#' @export
#' @examples
#' as_long()
as_long = function(x) {
  library(stringr)
  d_names = read.csv("countrycodes.csv", sep = ";", header = T, stringsAsFactors = F, encoding = "UTF-8")

  sapply(x, function(i) {
    indice = str_detect(d_names$Codes, i) %>% #find matches
      which %>% #their indices
      `[`(1) #get the first
    if(is.na(indice)) message(str_c(i, " could not be found!"))

    return(d_names$Names[indice])
  })
}


#' Write object to clipboard
#'
#' A wrapper function to write.table() for writing to the clipboard for pasting in a spreadsheet.
#' @param x (any object that works with write.table) Something to write to the clipboard.
#' @param digits (integer) A number of digits to round the data to.
#' @param clean_names (boolean) Whether to clean the names. Default=F.
#' @param clean_what (character vector) Which things to clean. Defaults to underscores and dots.
#' @keywords write, output, export, clipboard
#' @export
#' @examples
#' write_clipboard()
write_clipboard = function(x, digits = 3, clean_names = F, clean_what = c("_", "\\.")) {
  library(stringr)

  #round
  x = round_df(x, digits)

  #clean
  if (clean_names) {
    for (char in clean_what) {
      if (is.data.frame(x) | is.matrix(x)) {
        rownames(x) = str_replace_all(rownames(x), char, " ")
        colnames(x) = str_replace_all(colnames(x), char, " ")
      }

      if (is.vector(x)) {
        names(x) = str_replace_all(names(x), "_", " ")
      }
    }
  }

  write.table(x, "clipboard", sep = "\t", na = "")
}
