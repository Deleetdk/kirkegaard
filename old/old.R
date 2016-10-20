#' Dataset merger function
#'
#' This function allows you to merge two data.frames by their overlapping rownames.
#' @param DF1 the first data.frame
#' @param DF2 the second data.frame
#' @param main which data.frame should be used as the main? Choose the larger one if working with large datasets. Default to using neither.
#' @param join (character scalar) Which data.frames to use cases from. Defaults to "both". Can be: both, left, right.
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