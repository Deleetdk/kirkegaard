## DATASET AND I/O FUNCTIONS

#' Dataset merger function
#'
#' This function allows you to merge two data.frames by their overlapping rownames.
#' @param DF1 the first data.frame
#' @param DF2 the second data.frame
#' @param main which data.frame should be used as the main? Choose the larger one if working with large datasets. Default to using neither.
#' @keywords merging combining datasets data.frame
#' @export
#' @examples
#' merge_datasets()
merge_datasets = function (DF1, DF2, main=1, time=F){
  #time if desired
  if (time) {time1 = proc.time()} #start timer
  
  #colnames, remove duplicates
  total.colnames = c(colnames(DF1),colnames(DF2))
  total.colnames.unique = unique(total.colnames)
  
  #rownames, remove duplicates
  total.rownames = c(rownames(DF1),rownames(DF2))
  total.rownames.unique = unique(total.rownames)
  
  #combined dataset
  #main setting decides how to combine
  #default is to create a new DF and add everything into it
  #but this will be slow for larger DFs
  if (!(main == 1 | main == 2 | main == 0)){ #check for valid input
	  print("Valid input to parameter 'main' not provided");return(NULL)
  }
  if (main==0){ #create a combined dataset
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
  		  if (is.na(DF2[case,variable])){ #skip if datapoint is missing
  			  next
  		  }
  		  DF3[rownames(DF2)[case], colnames(DF2)[variable]] = DF2[case,variable]
  		  #print(DF3[rownames(DF2)[case], colnames(DF2)[variable]]) #used for debugging
  		}
  	}
  }
  if (main!=1){ #if DF2 is main
	    #loop over input dataset 1
		for (variable in 1:length(colnames(DF1))){ #loop over variables/cols
			for (case in 1:length(rownames(DF1))){ #loop over cases/rows
			  if (is.na(DF1[case,variable])){ #skip if datapoint is missing
				next
			  }
		    DF3[rownames(DF1)[case], colnames(DF1)[variable]] = DF1[case,variable]
			#print(DF3[rownames(DF1)[case], colnames(DF1)[variable]]) #used for debugging
			}
		}
	}

  #output time
  if (time) {
    time2 = proc.time()-time1 #end timer
    print(time2) #print time
  }

  return(DF3)
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


#' Write object to clipboard
#'
#' A wrapper function to write.table() for writing to the clipboard for pasting in a spreadsheet.
#' @param x An matrix or data.frame, or something similar.
#' @keywords write, output, export, clipboard
#' @export
#' @examples
#' write_clipboard()
write_clipboard = function(x) {
  write.table(x, "clipboard", sep = "\t", na = "")
}