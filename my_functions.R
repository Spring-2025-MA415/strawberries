drop_one_value_col <- function(df, prt_val = FALSE){ 
  df_id <- ensym(df)
  if(prt_val){
    msg = paste("Looking for single value columns in data frame: ",as.character(df_id) )
    print(msg)}
  ## takes whole dataframe
  dropc <- NULL
  val <- NULL
  ## test each column for a single value
  for(i in 1:dim(df)[2]){   
    if(dim(distinct(df[,i]))[1] == 1){
      dropc <- c(dropc, i)
      val <- c(val, df[1,i])
    }
  } 
  
  if(prt_val){
    if(is.null(dropc)){
      print("No columns dropped")
      return(df)}else{
        print("Columns dropped:")
        # print(colnames(df)[drop])
        print(unlist(val))
        df <- df[, -1*dropc]
        return(df)
      }
  }
  df <- df[, -1*dropc]
  return(df)
}




## function shift_loc
## Moves adjacent data cells in a data.frame on a single row
## Use this function to fix alignment problems after separating 
## columns containing multiple columns of data. 

## Of course the working assumption is that there is room in the 
## data frame for the data you're shifting.
##
## The data cells that are empty after the data shift are NA.
## 
## Input parameters
## 
## df -- data frame
## col_name -- name of colume where the left-most data item is located
## dat_name -- name of data item in the column
## num_col -- the number of columns is the same as the number of
##            adjacent data to be moved.
## num_shift -- the number of rows to move the data 
##
shift_loc <- function(df, col_name, dat_name, num_col, num_shift){
  col_num = which(colnames(df) == col_name)
  row_num = which(df[,col_num] == dat_name)  ## calcs a vector of rows
  
  for(k in 1:length(row_num)){
    d = rep(0,num_col) ## storage for items to be moved
    for(i in 1:num_col){
      d[i] = df[row_num[k], col_num + i - 1]
    }
    for(i in 1:num_col){
      ra = row_num[k]
      cb = col_num + i - 1
      df[ra, cb] <-  NA
    }
    for(j in 1:num_col){
      rc = row_num[k]
      cd = col_num + j - 1 + num_shift
      df[rc, cd] = d[j]
    }
  }
  return(df)
}


show_unique <- function(data, nrows=10 ){
  # make a tibble items to hold the data to show
  a <- nrows * dim(data)[2]  # number of cells in items
  items <- rep(" ", a) # items will coerce everything to char
  dim(items) <- c(nrows ,dim(data)[2]) # shape items
  items <- as_tibble(items)
  colnames(items) <- colnames(data)
  # browser()
  for(i in 1:dim(data)[2]){
    
    col_items <- unique(data[,i])
    # row_ex is the number of rows needed 
    # to make the column length conformable with items
    row_ex <- nrows - dim(col_items)[1] 
    if(row_ex >= 0){
      ex_rows <- tibble(rep(" ",row_ex))
      colnames(ex_rows) <- colnames(col_items)
      col_add <- rbind2(col_items, ex_rows)
      
    } else if(row_ex < 0){
      col_add <- col_items[1:10,]
      
    }
    
    items[,i] <- col_add
    
  }
  
  return(items)
}