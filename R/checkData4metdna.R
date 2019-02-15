# path <- "V:/workreport/Shen Xiaotao/demo/metDNA/fly20180712/POS"
#' @title checkData4metdna
#' @description checkData4metdna
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param path Absolute directory
#' @return code
#' @export

checkData4metdna <- function(path = "."){

  file.name <- dir(path)

error.code <- NULL
warning.code <- NULL

###upload MS1 data or not
if(any(file.name == "data.csv")){
  data <- readr::read_csv(file.path(path, "data.csv"))
  sample.name1 <- sort(colnames(data)[-c(1:3)])
  ##MV in data
  if(sum(is.na(data)) > 0){
    error.code <- c(error.code, 1)
  }

  ##column 1 is "name" or not?
  if(colnames(data)[1] != "name"){
    error.code <- c(error.code, 3)
  }

  ##column 2 is "mz" or not?
  if(colnames(data)[2] != "mz"){
    error.code <- c(error.code, 4)
  }

  ##column 3 is "rt" or not?
  if(colnames(data)[3] != "rt"){
    error.code <- c(error.code, 5)
  }

  ###max rt < 60 or not
  if(max(as.numeric(data$rt)) <= 60){
    warning.code <- c(warning.code, 9)
  }

  ##any peak with 0 > 50%
  zero.per <- apply(data[,-c(1:3)], 1, function(x){
    sum(x == 0)/(ncol(data)-3)
  })

  if(any(zero.per > 0.5)){
    warning.code <- c(warning.code, 10)
  }

}

###sample information
if(any(file.name == "sample.info.csv")){
  sample.info <- readr::read_csv(file.path(path, "sample.info.csv"))
  sample.info <- as.data.frame(sample.info)
  ##MV in sample.info
  if(sum(is.na(sample.info)) > 0){
    error.code <- c(error.code, 2)
  }

  ##column 1 is "sample.name" or not?
  if(ncol(sample.info) >= 2){
    sample.name2 <- sort(as.character(sample.info[,1]))
    if(colnames(sample.info)[1] != "sample.name"){
      error.code <- c(error.code, 6)
    }

    ##column 2 is "group" or not?
    if(colnames(sample.info)[2] != "group"){
      error.code <- c(error.code, 7)
    }
  }else{
    error.code <- c(error.code, 6, 7)
  }

}

###
if(any(file.name == "sample.info.csv") & any(file.name == "data.csv")){
if(length(sample.name1) != length(sample.name2)){
  error.code <- c(error.code, 8)
}else{
  if(any(sample.name1 != sample.name2)){
    error.code <- c(error.code, 8)
  }
}

}

##
error.code <- sort(unique(error.code))
warning.code <- sort(unique(warning.code))

if(is.null(error.code)){
  error.code <- 0
}

if(is.null(warning.code)){
  warning.code <- 0
}

result <- sort(unique(c(error.code, warning.code)))

result

}



##Error code meaning
##1 Missing values are found in the MS1 peak table
##2 Missing values are found in the sample information
##3 Column name of the first column in MS1 peak table is not "name"
##4 Column name of the second column in MS1 peak table is not "mz"
##5 Column name of the third column in MS1 peak table is not "rt"
##6 Column name of the first column in Sample Information is not "sample.name"
##7 Column name of the second column in Sample Information is not "group"
##8 Sample names in MS1 peak table and Sample Information are different

##Warning code meaning
##9 The maximum value of "rt" is less than 60, the unit of rt may not be "second"
##10 At least one MS1 peak in the table is found to have more than 50% of zero values among its peak abundances