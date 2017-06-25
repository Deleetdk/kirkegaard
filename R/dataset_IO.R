## DATASET AND I/O FUNCTIONS


#these two functions are wrappers intended to make it easier to work with the megadataset

#' A wrapper for read.csv()
#'
#' This is a convenient wrapper for read.csv()
#' @param filename the file to be read
#' @export
read_mega = function(filename){
  return(read.csv(filename,sep=";",row.names=1, #this loads the rownames
                  stringsAsFactors=FALSE)) #dont want factors
}

#' A wrapper for write.csv()
#'
#' This is a convenient wrapper for write.csv()
#' @param object the object to be written to a file
#' @param filename the name of the file you want to write to
#' @export
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
#' @export
output_sorted_var = function(df, var, filename) {
  s = df[var] #subset
  s2 = s[order(s[1], decreasing = T),,drop=F] #sort
  s3 = data.frame(1:nrow(s2),
                  s2)
  rownames(s3) = rownames(s2)
  colnames(s3) = c("Rank", var)
  write.csv(s3, file.name, fileEncoding = "UTF-8") #save
}

#' Write object to clipboard
#'
#' S3 generic function to write objects to the clipboard for easy use.
#'
#' See \code{\link{write_clipboard.data.frame}}, \code{\link{write_clipboard.model_summary}}.
#' @export
write_clipboard <- function(...) UseMethod("write_clipboard")


#' Write data frame to clipboard
#'
#' A wrapper function to \code{\link{write.table}} for writing to the clipboard for pasting in a spreadsheet.
#' @param x (any object that works with write.table) Something to write to the clipboard.
#' @param digits (int scalar) A number of digits to round the data to.
#' @param clean_names (log scalar) Whether to clean the names. Default=T.
#' @param clean_what (chr vector) Which things to clean. Defaults to underscores and dots.
#' @param pad_digits (log scalar) Whether to pad zeros to the digits (for prettier tables; default = T).
#' @param print (log scalar) Whether to also print the output in R (default T).
#' @param .rownames (lgl scalar) Whether to write rownames. Default yes. These are written to a column in front called .rownames.
#' @param write_to_clipboard (lgl) Whether to write to the clipboard. Can be useful to disable in rare cases.
#' @param return_modified (lgl) Whether to return the modified input instead of the original. Useful if one wants to modify it further.
#' @export
#' @examples
#' iris[-5] %>% cor %>% write_clipboard
#' iris %>% head %>% miss_add_random %>% write_clipboard
write_clipboard.data.frame = function(x, digits = 2, clean_names = T, clean_what = c("_", "\\."), pad_digits = T, print = T, .rownames = T, write_to_clipboard = interactive(), return_modified = F) {

  #round
  x_orig = x
  x = as.data.frame(x) %>% df_round(digits)

  #format if desired
  if (pad_digits) {
    x = format(x, nsmall = digits)

    #recode "NAs" as empty strings
    x[] = lapply(x, str_detect_replace, pattern = " *NA", replacement = "")
  }

  #clean
  if (clean_names) {
    for (char in clean_what) {
      if (is.data.frame(x) | is.matrix(x)) {
        rownames(x) = stringr::str_replace_all(rownames(x), char, " ")
        colnames(x) = stringr::str_replace_all(colnames(x), char, " ")
      }

      if (is.vector(x)) {
        names(x) = stringr::str_replace_all(names(x), "_", " ")
      }
    }
  }

  #print
  if (print) print(x)

  #write to clipboard
  if (write_to_clipboard) {
    #add rownames if desired
    if (.rownames) {
      x_modded = cbind(".rownames" = rownames(x), x)
    } else {
      x_modded = x
    }

    #decide how to write
    #windows is easy!
    if (Sys.info()['sysname'] %in% c("Windows")) {
      #just write as normal
      write.table(x, "clipboard", sep = "\t", na = "", row.names = F)
    } else {
      #for non-windows, try xclip approach
      #https://stackoverflow.com/a/10960498/3980197
      write.xclip = function(x) {
        #if xclip not installed
        if (!isTRUE(file.exists(Sys.which("xclip")[1L]))) {
          stop("Cannot find xclip")
        }
        con <- pipe("xclip -selection c", "w")
        on.exit(close(con))
        write.table(x, con, sep = "\t", na = "", row.names = F)
      }

      tryCatch({
        write.xclip(x)
      }, error = function(e) {
        message("Could not write using xclip")
      })
    }
  }


  #return
  if (return_modified) {
    return(invisible(x))
  } else {
    return(invisible(x_orig))
  }
}


#for matrix, use data frame function
#' Write matrix to clipboard
#'
#' Write a matrix to the clipboard with clean formating. Calls \code{\link{write_clipboard.data.frame}}.
#' @export
write_clipboard.matrix = write_clipboard.data.frame

#Read Clipboard
read_clipboard = function(header = T,
                          sep = "\t",
                          na.strings = c("", "NA"),
                          check.names = T,
                          stringsAsFactors = F,
                          dec = ".",
                          ...) {
  #decide how to read
  #windows is easy!
  if (Sys.info()['sysname'] %in% c("Windows")) {
    #just read as normal
    read.table(file = con, sep = sep, header = header, check.names = check.names, na.strings = na.strings, stringsAsFactors = stringsAsFactors, dec = dec, ...)
  } else {
    #for non-windows, try xclip approach
    #https://stackoverflow.com/a/10960498/3980197
    read.xclip = function(x) {
      #if xclip not installed
      if (!isTRUE(file.exists(Sys.which("xclip")[1L]))) {
        stop("Cannot find xclip")
      }
      con <- pipe("xclip -o -selection c", "r")
      on.exit(close(con))
      read.table(file = con, sep = sep, header = header, check.names = check.names, na.strings = na.strings, stringsAsFactors = stringsAsFactors, dec = dec, ...)
    }

    tryCatch({
      read.xclip(x)
    }, error = function(e) {
      message(sprintf("error: %s", e$message))
    })
  }
}

#helper function for printing lists of heterogenous data frames
ldf_to_long_mat = function(x, rowname_headers = NULL, only_interesting_rownames = T, clean_colnames = T, clean_rownames = T) {
  #rowname headers?
  if (is.null(rowname_headers)) {
    rowname_headers = rep(NA, length(x))
  }

  #convert rownames and colnames to explicit names
  x_explicit_names = purrr::map2(x, rowname_headers, function(., rh) {
    #boring rownames?
    if (only_interesting_rownames && are_equal(rownames(.), as.character(1:nrow(.)))) {
      y = rbind(colnames(.) %>% str_clean, as.matrix(.))
      rownames(y) = NULL
      colnames(y) = NULL
      return(y)
    }

    #append rownames to new leftmost col
    #and colnames to new top row, put NA in the corner
    y = cbind(c(rh, rownames(.) %>% str_clean), rbind(colnames(.) %>% str_clean, as.matrix(.)))
    rownames(y) = NULL
    colnames(y) = NULL
    y
  })

  #widest?
  x_widths = purrr::map_int(x_explicit_names, ~ncol(.))
  x_width_max = max(x_widths)

  #add empty columns
  x_cols_added = purrr::map(x_explicit_names, function(.) {
    #need to add?
    if (ncol(.) < x_width_max) {
      delta_ncol = x_width_max - ncol(.)
      with_added = cbind(., matrix(rep("", nrow(.) * delta_ncol), ncol = delta_ncol))
      return(with_added)
    }

    #otherwise, good to go
    .
  })

  #fill in empty lines and titles
  x_with_empty_lines = purrr::map(seq_along(x_cols_added), function(i) {
    #add empty
    this = x_cols_added[[i]]
    this_name = names(x)[i]

    #add header?
    if (has_names(x)) {
      #add a header based on the name of the list
      this = rbind(
        c(this_name, rep("", x_width_max - 1)),
        this
      )
    }

    #add empty
    this = rbind(
      rep("", x_width_max),
      this
    )

    this
  })

  #finally, rbind and cut top line
  purrr::reduce(x_with_empty_lines, rbind) %>%
    #remove the top empty line
    `[`(-1, )
}


#' Write model summary to clipboard
#'
#' Restructures a model summary object to a matrix and writes it to the clipboard.
#' @export
#' @examples
#' lm(Sepal.Length ~ Petal.Length, data = iris) %>% MOD_summary(kfold = F) %>% write_clipboard
write_clipboard.model_summary = function(x, digits = 2) {

  #rerestructure to a matrix suitable for clipboard
  ldf_to_long_mat(list(
    #coefs
    coefs = x$coefs %>% df_round(digits = digits),

    #model meta data
    meta = x$meta,

    #etas
    etas = x$aov_etas %>% df_round(digits = digits)
  ), rowname_headers = c("Predictor", NA, "Predictor")) %>%
    #call generic again
    write_clipboard()
}


#' Stack data into n columns.
#'
#' Reshapes the data to an n column structure for easy use in documents. Pads empty lines between variables and can include their names as well. Outputs a character matrix.
#' @param data (data.frame, matrix, or something coercible into a matrix) The data to reshape.
#' @param columns (whole number scalar) How many columns to stack the data into.
#' @param pad_columns (logical) Whether to pad empty columns to the data if the data and column dimensions do fit divide into a whole number. Defaults to TRUE.
#' @param include_colnames (logical) Whether to include the column names in the output. Defaults to TRUE.
#' @param rownames_colnames (character scalar) If adding colnames in rows, which rownames should these be given? Defaults to "name".
#' @export
#' @examples
#' df = split_every_k(1:12, 2) %>% as.data.frame
#' stack_into_n_columns(df, 2)
#' stack_into_n_columns(df, 3)
#' stack_into_n_columns(df, 4)
stack_into_n_columns = function(data, columns, pad_columns = TRUE, include_colnames = TRUE, rowname_colnames = "name") {

  #checks
  data = as.matrix(data)
  assertthat::assert_that(is.matrix(data))
  assertthat::assert_that(is_whole_number(columns))
  assertthat::assert_that(is.logical(pad_columns))

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
      data = cbind(data, matrix(NA, ncol = v_n_to_pad, nrow = nrow(data)))
    }
  } else {
    if (ncol(data) %% columns != 0) {
      stop(stringr::str_c("data isn't integer divisible into ", columns, " columns!", ncol(data), "/", columns, "=", ncol(data) %% columns))
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



#' Split data into n columns.
#'
#' Reshapes the data to a wider structure for easy use in documents. Pads empty lines between and can include their rownames and colnames as well. Outputs a character matrix.
#' @param data (data.frame, matrix, or something coercible into a matrix) The data to reshape.
#' @param split_times (whole number scalar) How many times to split the rows.
#' @param pad_columns (logical) Whether to pad empty columns to the data if the data and column dimensions do fit divide into a whole number. Defaults to TRUE.
#' @param include_colnames (logical) Whether to include the column names in the output. Defaults to TRUE.
#' @param rownames_colnames (character scalar) If adding colnames in rows, which rownames should these be given? Defaults to "name".
#' @export
#' @examples
#' df = data.frame(small = letters[1:6], big = LETTERS[1:6], stringsAsFactors = F)
#' split_into_n_columns(df, 2) #ok
#' split_into_n_columns(df, 3) #ok
#' split_into_n_columns(df, 4) #stupid but no error!
split_into_n_columns = function(data, split_times, pad_rows = T, include_rownames = T, rownames_var = "name", include_colnames = T) {

  #types
  data = as.data.frame(data)

  #convert factors to chr
  data[] = lapply(data, FUN = function(col) {
    if (is.factor(col)) return(as.character(col))
    col
  })

  #check length
  v_remainder = nrow(data) %% split_times
  if (!pad_rows & (v_remainder != 0)) {
    stop("It is not possible to reform the data without padding rows because the number of rows is not integer disible by the desired number of columns! " + nrow(data) + "/" + split_times + "=" + v_remainder)
  }

  #include rownames?
  if (include_rownames) {
    data = data.frame(rownames___ = rownames(data), data, stringsAsFactors = F)
    colnames(data)[1] = rownames_var
  }

  #save colnames
  v_colnames = colnames(data)

  #make out data object
  v_rows_out = (nrow(data) / split_times) %>% ceiling()

  #fill in missing rows
  if ((nrow(data) %% split_times) != 0) {
    #how many rows to fill in?
    v_to_fill = nrow(data) %% split_times

    #make the extra block
    data_extra = matrix(nrow = v_to_fill, ncol = ncol(data)) %>% as.data.frame(stringsAsFactors = F)

    #copy over colnames
    colnames(data_extra) = colnames(data)

    #rbind them
    data = rbind(data, data_extra)
  }

  #transform
  data2 = df_to_ldf(data, by = rep(1:split_times, each = v_rows_out)) %>%
    do.call(what = "cbind", args = .)

  #names
  colnames(data2) = NULL

  #add colnames on top
  if (include_colnames) {
    v_colnames = rep(v_colnames, length.out = ncol(data2))
    data2 = rbind(v_colnames, data2)
  }

  data2
}



#' Improved dataset merger function
#'
#' This function allows you to merge two data.frames by their overlapping rownames. About 15 times faster than the earlier version.
#' @param DF1 (data.frame) A data.frame to merge into.
#' @param DF2 (data.frame) A data.frame with the new data.
#' @param join (character scalar) Which data.frame to use cases from. Options: left, right, both.
#' @param overwrite_NA (lgl scalar) Whether to overwrite with NA values.
#' @param restore_factors (lgl scalar) Whether to recreate factors in the merged data.frame. Does not keep levels.
#' @export
#' @examples
#' merge_datasets(iris[1:4], iris[1:5]) #merge together two parts of iris
merge_datasets = function (DF1, DF2, join = "both", overwrite_NA = FALSE, restore_factors = FALSE){

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

  #factors present?
  if (any(c(purrr::map_lgl(DF1, is.factor), purrr::map_lgl(DF2, is.factor)))) {
    if (!restore_factors) {
      message("Factors were converted to characters.")
    }
    if (restore_factors) {
      message("Factors were converted to characters and back to factors. However, the levels cannot be restored automatically.")
    }
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
    v_factors = purrr::map_lgl(colnames(DF3), function(col) {
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
#' This is a wrapper for merge_datasets().
#' @param ... (data.frames) Two or more data.frames to merge.
#' @param join (character scalar) Which data.frame to use cases from. Defaults to "both". Can be: both, left, right.
#' @param overwrite_NA (logical scalar) Whether to overwrite with NA values. Default = FALSE.
#' @param restore_factors (logical scalar) Whether to recreate factors in the merged data.frame. Does not keep levels. Default = FALSE.
#' @export
#' @examples
#' merge_datasets_multi(iris[1:50, ], iris[51:100, ], iris[101:150, ]) #merge three-part iris
merge_datasets_multi = function(..., join = "both", overwrite_NA = FALSE, restore_factors = FALSE) {
  #wrap with Reduce
  Reduce(function(x, y) merge_datasets(x, y, join=join, overwrite_NA = overwrite_NA, restore_factors = restore_factors), list(...))
}


#' Write rvest/xml object
#'
#' Writes an rvest/xml object to disk for reuse. This is a wrapper around write_rds.
#' @param x (obj) Object to write to disk.
#' @param path (str) Where to save it.
#' @param ... Other parameters to write_rds.
#' @return Invisibly returns x for use in pipelines.
#' @export
write_rvest = function(x, path, ...) {
  #convert to character
  #is list?
  if (inherits(x, "list")) {
    x = purrr::map(x, function(.x) {
      #to char
      .x = as.character(.x)

      #to UTF8
      Encoding(.x) = "UTF-8"

      .x
    })

  } else {
    x = as.character(x)
    Encoding(x) = "UTF-8"
  }

  #save
  readr::write_rds(x, path = path, ...)

  invisible(x)
}

#' Read rvest/xml object
#'
#' Reads an rvest/xml object from disk. This is a wrapper around read_rds.
#' @param path (str) Where to read from.
#' @param ... Other parameters to read_rds.
#' @return An XML object.
#' @export
read_rvest = function(path) {
  #load from file
  x = readr::read_rds(path)

  #read
  if (inherits(x, "list")) {
    x = purrr::map(x, read_html)
  } else {
    x = xml2::read_html(x)
  }

  x
}

