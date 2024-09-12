library("data.table")

get_int_var_in_range <- function(par_a, par_b, par_c, par_d, par_e) {
    # print(cbind(par_a, par_c, par_d, par_e))
    ret_par_d = NA
    observed = NA
    if (any(par_c == 0, na.rm = TRUE)) {
        ret_par_d = par_d[par_c == 0]
        observed = par_a[par_c == 0]
    } else {
        if ("first" == tolower(par_e)) {
            ret_par_d = head(par_d, n = 1L)
            observed = head(par_a, n = 1L)
        } else {
            if ("last" == tolower(par_e)) {
                ret_par_d = tail(par_d, n = 1L)
                observed = tail(par_a, n = 1L)
            } else {
                if ("nearest" == tolower(par_e)) {
                  # par_c_par_d <- cbind.data.frame(par_c,par_d)
                  a = cbind.data.frame(par_a, par_c, par_d)[order(par_c), ][1, ]
                  observed = a[[1]]
                  ret_par_d = a[[3]]
                } else {
                  if ("weightedavg" == tolower(par_e)) {
                    par_c_rec = 1/par_c
                    par_c_rec_sum = sum(par_c_rec)
                    ret_par_d = par_d %*% (par_c_rec/par_c_rec_sum)
                    if (1 < length(par_c)) {
                      observed = NA
                    } else {
                      observed = head(par_a, n = 1L)
                    }
                  } else {
                    observed = NA
                    ret = NA
                  }
                }
            }
        }
    }
    # print(c(observed, ret_par_d))
    return(c(observed, ret_par_d))
}


# Input:
# cols - Column names of the input (in the long format) data frame.
check_data_long_colnames <- function(cols) {
    if (is.null(cols)) {
        return(FALSE)
    }
    if (!is.vector(cols)) {
        return(FALSE)
    }
    if (is.list(cols)) {
        return(FALSE)
    }
    if (0 == length(cols)) {
        return(FALSE)
    }
    if (!is.character(cols)) {
        return(FALSE)
    }
    if (anyNA(cols)) {
        return(FALSE)
    }
    
    return(TRUE)
}


check_vector_time_invariant <- function(vec_timeinv, df_colnames) {
    if (missing(vec_timeinv)) {
        return(TRUE)
    } else {
        if (0 == length(vec_timeinv)) {
            return(TRUE)
        } else {
            if (!is.vector(vec_timeinv)) {
                print("Time invariant variables are not in the form of vector.")
                return(FALSE)
            } else {
                if (!is.character(vec_timeinv)) {
                  print("Time invariant variables are not character strings.")
                  return(FALSE)
                } else {
                  return(all(vec_timeinv %in% df_colnames))
                }
            }
        }
    }
}


# Input is a list, with each element being
# [1]: specified the column name in the output (mandatory)
# [2]: the column name in the input (mandatory) 
# [3]: the target (in 'agedays') (optional, default = 100)
# [4]: the tolerance (optional, default = 15)
# [5]: the strategy, one of 'first', 'last', 'nearest' and 'weightedavg' (optional, default = 'nearest')
check_one_time_variant <- function(input, df_col_names) {
    if (missing(input) || !is.list(input)) {
        print("input missing or not a list of var - target - tolerance - strategy")
        return(FALSE)
    }
    
    len <- length(input)
    if (2 > len || 5 < len) {
        print("input not in acceptable length")
        return(FALSE)
    }
    
    if (!input[[2]] %in% df_col_names) {
        print("input not in the given data frame")
        return(FALSE)
    }
    
    if (3 <= len && !is.numeric(input[[3]])) {
        print("input has no valid target")
        return(FALSE)
    }
    
    if (4 <= len && !is.numeric(input[[4]])) {
        print("input has no valid tolerance range")
        return(FALSE)
    }
    
    if (5 <= len && !input[[5]] %in% c("first", "last", "nearest", "weightedavg")) {
        print("input has no supported strategy")
        return(FALSE)
    }
    
    return(TRUE)
}


check_list_time_variant <- function(list_timev, df_colnames) {
    if (missing(list_timev)) {
        return(TRUE)
    } else {
        if (!is.list(list_timev)) {
            print("Time variant variables are not in the form of list.")
            return(FALSE)
        } else {
            if (0 == length(list_timev)) {
                return(TRUE)
            } else {
                for (timev in list_timev) {
                  if (!check_one_time_variant(timev, df_colnames)) {
                    return(FALSE)
                  }
                }
                return(TRUE)
            }
        }
    }
}


# Inputs: 
# df- Input data frame 
# col - a character string being the name of the column to be filled
fill_col <- function(df, col_regexp) {
    allcols <- colnames(df)
    cols_indices <- grep(col_regexp, allcols)
    for (cindex in cols_indices) {
        a <- df[cindex]
        setorder(a, na.last = T)
        df[cindex] <- rep(a[1, 1], nrow(df))
    }
    return(df)
}


# Assuming the input df is already sorted as df[with(df, order(get(key_var), get(mea_var))),]
long_to_wide_one_var <- function(df, oname, target_val, tol_ran, int_var, str_val = "nearest", key_var = "SUBJID", 
    mea_var = "AGEDAYS") {
    dt1 <- setDT(df)[is.finite(get(int_var)), `:=`(offset, abs(get(mea_var) - target_val))]
    dt1 <- dt1[offset <= tol_ran, `:=`(new1, .(list(get_int_var_in_range(get(mea_var), target_val, offset, 
        get(int_var), str_val)))), by = key_var]
    dt1 <- dt1[exists("new1"), `:=`(output_observed_on = sapply(new1, `[`, 1), output_value = sapply(new1, 
        `[`, 2))]
    dt1 <- dt1[!exists("output_observed_on"), `:=`(output_observed_on = NA, output_value = NA)]
    is.na(dt1) <- dt1 == "NULL"
    dt1 <- dt1[, `:=`(output_varname = oname, input_varname = int_var, target = target_val, tolerance = tol_ran, 
        strategy = str_val)]
    return(dt1[!is.na(output_value), .SD[1], .SDcols = c("output_varname", "output_value", "output_observed_on", 
        "input_varname", "target", "tolerance", "strategy"), by = key_var])
}



# Input df_long is 
# (a) a data frame 
# (b) containing 'SUBJID', 'AGEDAYS' and one or more columns that are covariates, e.g. test_1.csv
# (c) the covariates could be time invariant (e.g. 'MAGE' and 'FAGE' in test_1.csv) or time variant 
# (e.g. 'HAZ' and 'WAZ' in test_1.csv) 
# vector_timeinvar is
# (a) a vector of character strings
# (b) each character string being a time invariant covariate that exists in df_long
# list_timevar is 
# (a) a list of entities 
# (b) each represents a request of summary of one time variant covariate; the time variant covariate needs to exist 
# in df_long
# (c) each request is in the form of a list, see the documentation of check_one_time_variant() for its content.
long_to_wide_detailed <- function(df_long, vector_timeinvar = c(), list_timevar = list()) {
    if (missing(df_long) || !is.data.frame(df_long)) 
        stop("Input data not given or not in the form of data.frame")
  
    df_long_col_names <- colnames(df_long)
  
    colname_subjid <- grep("SUBJID", df_long_col_names, ignore.case = T, value = T)
    if ( 0 == length(colname_subjid) )
        stop("Input data frame does not have the column subjid.")

    colname_agedays <- grep("AGEDAYS", df_long_col_names, ignore.case = T, value = T)
    if ( 0 == length(colname_agedays) )
        stop("Input data frame does not have the column agedays.")

    df_long <- df_long[with(df_long, order( get(colname_subjid), get(colname_agedays) ) ), ]
  
    if (!check_data_long_colnames(df_long_col_names)) {
        stop("Column names of the input data frame invalid.")
    }
    
    if (!check_vector_time_invariant(vector_timeinvar, df_long_col_names)) {
        stop("Not all requested time invariant columns are present in the input data frame.")
    }
    
    if (!check_list_time_variant(list_timevar, df_long_col_names)) {
        stop("Not all requested time variant columns are present in the input data frame.")
    }
    
    df_wide <- data.frame(sort(unique(df_long[,colname_subjid])))
    colnames(df_wide) <- colname_subjid
    num_var <- 0
    
    for (v in vector_timeinvar) {
        df <- long_to_wide_one_var(df_long, v, 0, Inf, v, "first", colname_subjid, colname_agedays)
        
        # if ( 0 == nrow(df) || 0 == ncol(df) ) next Commented out this line to make the all-NA returned data
        # frame still appear in the final outcome.
        
        num_var <- num_var + 1
        df_ncol <- ncol(df)
        colnames(df)[2:df_ncol] <- paste0(colnames(df)[2:df_ncol], "_", num_var)
        df_wide <- merge(setDT(df_wide), setDT(df), by = colname_subjid, all = T)
        is.na(df_wide) <- df_wide == "NULL"
    }
    
    for (v in list_timevar) {
        len <- length(v)
        
        if (5 > len) {
            strategy <- "nearest"
        } else {
            strategy <- v[[5]]
            if (4 > len) {
                tolerance <- 15
            } else {
                tolerance <- v[[4]]
                if (3 > len) {
                  target <- 100
                } else {
                  target <- v[[3]]
                }
            }
        }
        iname <- v[[2]]
        oname <- v[[1]]
        
        df <- long_to_wide_one_var(df_long, oname, target, tolerance, iname
            , strategy, colname_subjid, colname_agedays)
        
        # if ( 0 == nrow(df) || 0 == ncol(df) ) next Commented out this line to make the all-NA returned data
        # frame still appear in the final outcome.
        
        num_var <- num_var + 1
        df_ncol <- ncol(df)
        colnames(df)[2:df_ncol] <- paste0(colnames(df)[2:df_ncol], "_", num_var)
        df_wide <- merge(setDT(df_wide), setDT(df), by = colname_subjid, all = T)
        is.na(df_wide) <- df_wide == "NULL"
    }
    
    df_wide <- fill_col(setDF(df_wide), "output_varname_*|input_varname_*|target_*|tolerance_*|strategy_*")
    
    return(df_wide)
}

long_to_wide_analysis <- function(dw_detailed) {
    df <- setDF(dw_detailed)
    dw_analysis <- df[1] # Assuming "SUBJID" is on column 1.
    coliterator <- 0
    vaindex <- 0
    len <- ncol(dw_detailed)
    while (coliterator < len) {
        vaindex <- vaindex + 1
        varange <- grep(paste0("_", vaindex, "$"), colnames(dw_detailed))
        coliterator <- tail(varange, 1)
        dw_analysis <- cbind(dw_analysis, df[varange[2]])
        colnames(dw_analysis)[1 + vaindex] <- df[1, varange[1]]
    }
    return(dw_analysis)
}

long_to_wide <- function(df_long, vector_timeinvar = c(), list_timevar = list(), detailed = FALSE) {

    dw_detailed <- long_to_wide_detailed(df_long, vector_timeinvar, list_timevar)
    if (detailed) 
        return(dw_detailed) 
    else 
        return(long_to_wide_analysis(dw_detailed))
}


