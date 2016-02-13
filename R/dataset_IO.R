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
    DF2 = DF2[intersect(rownames(DF1), rownames(DF2)), , drop = F] #subset to overlap with DF1
  }
  if (join == "right") {
    DF1 = DF1[intersect(rownames(DF1), rownames(DF2)), , drop = F] #subset to overlap with DF2
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
#' merge_datasets_multi(iris[1:50, ], iris[51:100, ], iris[101:150, ]) #merge three-part iris
merge_datasets_multi = function(...) {
  #wrap with Reduce
  Reduce(function(x, y) merge_datasets(x, y), list(...))
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
#' @param x (any object that works with write.table) Something to write to the clipboard.
#' @param digits (integer) A number of digits to round the data to.
#' @param clean_names (boolean) Whether to clean the names. Default=F.
#' @param clean_what (character vector) Which things to clean. Defaults to underscores and dots.
#' @keywords write, output, export, clipboard
#' @export
#' @examples
#' write_clipboard(cor(iris[-5]))
write_clipboard = function(x, digits = 2, clean_names = FALSE, clean_what = c("_", "\\."), pad_digits = T, print = FALSE) {
  library("stringr")
  library("magrittr")

  #round
  x = as.data.frame(x) %>% round_df(digits)

  #format if desired
  if (pad_digits) {
    x = format(x, nsmall = digits)
  }

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

  #print
  if (print) print(x)

  write.table(x, "clipboard", sep = "\t", na = "")
}


#' Stack data into n columns.
#'
#' Reshapes the data to an n column structure for easy use in documents. Pads empty lines between variables and can include their names as well. Outputs a character matrix.
#' @param data (data.frame, matrix, or something coercible into a matrix) The data to reshape.
#' @param columns (whole number scalar) How many columns to stack the data into.
#' @param pad_columns (logical) Whether to pad empty columns to the data if the data and column dimensions do fit divide into a whole number. Defaults to TRUE.
#' @param include_colnames (logical) Whether to include the column names in the output. Defaults to TRUE.
#' @param rownames_colnames (character scalar) If adding colnames in rows, which rownames should these be given? Defaults to "name".
#' @keywords reshape, stack, column, matrix, data.frame
#' @export
#' @examples
#' df = split_every_k(1:12, 2) %>% as.data.frame
#' stack_into_n_columns(df, 2)
#' stack_into_n_columns(df, 3)
#' stack_into_n_columns(df, 4)
stack_into_n_columns = function(data, columns, pad_columns = TRUE, include_colnames = TRUE, rowname_colnames = "name") {
  library("stringr")
  library("assertthat")

  #checks
  data = as.matrix(data)
  assert_that(is.matrix(data))
  assert_that(is_whole_number(columns))
  assert_that(is.logical(pad_columns))

  #already the case?
  if (ncol(data) == columns) {
    message("data already was in the desired number of columns")
    return(data)
  }

  #column number
  if (pad_columns) {
    if (ncol(data) %% columns != 0) {
      message(str_c("data isn't integer divisible into ", columns, " columns.", ncol(data), "/", columns, "=", ncol(data) %% columns), ". Padding empty columns.")

      #pad cols
      v_n_to_pad = columns - (ncol(data) %% columns)
      data = cbind(data, matrix(NA, ncol=v_n_to_pad, nrow=nrow(data)))
    }
  } else {
    if (ncol(data) %% columns != 0) {
      stop(str_c("data isn't integer divisible into ", columns, " columns!", ncol(data), "/", columns, "=", ncol(data) %% columns))
    }
  }

  #stack
  l_indices = split_every_k(1:ncol(data), columns)
  m = matrix(ncol = columns, nrow = 0)
  for (indices in l_indices) {
    #fetch colnames
    v_names = colnames(data)[indices]

    if (include_colnames) {
      m = rbind(m, #the object so far
                v_names, #the column names
                data[, indices], #the data slice
                matrix(NA, nrow=1, ncol=columns)) #empty row
    } else {
      m = rbind(m, #the object so far
                data[, indices], #the data slice
                matrix(NA, nrow=1, ncol=columns)) #empty row
    }

  }

  #clean
  m = m[-nrow(m), ] #remove last empty row

  #rownames
  if (is.null(rownames(data))) {
    rownames(data) = 1:nrow(data)
  }
  if (include_colnames) {
    rownames(m) = rep(c(rowname_colnames, rownames(data), NA), length.out = nrow(m))
  } else {
    rownames(m) = rep(c(rownames(data), NA), length.out = nrow(m))
  }
  #remove colnames
  colnames(m) = NULL

  return(m)
}


#' Improved dataset merger function
#'
#' This function allows you to merge two data.frames by their overlapping rownames. About 15 times faster than the earlier version.
#' @param DF1 (data.frame) A data.frame to merge into.
#' @param DF2 (data.frame) A data.frame with the new data.
#' @param join (character scalar) Which data.frame to use cases from. Defaults to "both". Can be: both, left, right.
#' @param overwrite_NA (logical scalar) Whether to overwrite with NA values. Default = FALSE.
#' @param restore_factors (logical scalar) Whether to recreate factors in the merged data.frame. Does not keep levels. Default = FALSE.
#' @export
#' @examples
#' merge_datasets2(iris[1:4], iris[1:5]) #merge together two parts of iris
merge_datasets2 = function (DF1, DF2, join = "both", overwrite_NA = FALSE, restore_factors = FALSE){
  library(magrittr)

  #checks
  if (!join %in% c("both", "left", "right")) stop("Invalid join parameter!")

  #join setting decides how to combine
  v_shared_rows = intersect(rownames(DF1), rownames(DF2))
  if (join == "left") {
    DF2 = DF2[v_shared_rows, , drop = FALSE] #subset to overlap with DF1
  }
  if (join == "right") {
    DF1 = DF1[v_shared_rows, , drop = FALSE] #subset to overlap with DF2
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

  #new DF
  v_rows = unique(c(rownames(DF1), rownames(DF2)))
  v_cols = unique(c(colnames(DF1), colnames(DF2)))
  DF3 = matrix(NA, nrow = length(v_rows), ncol = length(v_cols)) %>% as.data.frame()
  rownames(DF3) = v_rows;colnames(DF3) = v_cols

  #for each dataset
  for (DF in list(DF1, DF2)) {
    v_rows_to_add = rownames(DF)

    #for each col
    for (col in colnames(DF)) {

      #dont overwrite NAs
      if (!overwrite_NA) {
        #all
        v_rows_to_add = rownames(DF)

        #subset
        v_rows_to_add = v_rows_to_add[!is.na(DF[[col]])] #subset rows with non-NA
      }


      #write in
      #is factor?
      logi_nonfactor = !is.factor(DF[v_rows_to_add, col])
      if (logi_nonfactor) {
        DF3[v_rows_to_add, col] = DF[v_rows_to_add, col]
      } else {
        DF3[v_rows_to_add, col] = DF[v_rows_to_add, col] %>% as.character()
      }
    }
  }

  #restore factors
  if (restore_factors) {
    v_factors = sapply(colnames(DF3), function(col) {
      #check if cols are factors or nulls in both datasets
      if ((is.factor(DF1[[col]]) || is.null(DF1[[col]])) &&
          (is.factor(DF2[[col]]) || is.null(DF2[[col]]))) return(TRUE)
      #if they are, treat as factor
      #otherwise, treat as non-factor
      FALSE
    })

    #change cols to factors
    for (col_i in v_factors %>% which) {
      DF3[col_i] = as.factor(DF3[[col_i]])
    }
  }


  return(DF3)
}


#' Merge multiple datasets at once, improved version.
#'
#' This is a wrapper for merge_datasets2().
#' @param ... (data.frames) Two or more data.frames to merge.
#' @param join (character scalar) Which data.frame to use cases from. Defaults to "both". Can be: both, left, right.
#' @param overwrite_NA (logical scalar) Whether to overwrite with NA values. Default = FALSE.
#' @param restore_factors (logical scalar) Whether to recreate factors in the merged data.frame. Does not keep levels. Default = FALSE.
#' @export
#' @examples
#' merge_datasets2_multi(iris[1:50, ], iris[51:100, ], iris[101:150, ]) #merge three-part iris
merge_datasets2_multi = function(..., join = "both", overwrite_NA = FALSE, restore_factors = FALSE) {
  #wrap with Reduce
  Reduce(function(x, y) merge_datasets2(x, y, join=join, overwrite_NA = overwrite_NA, restore_factors = restore_factors), list(...))
}

