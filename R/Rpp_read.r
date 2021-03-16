#' Read function
#'
#' @description  This function reads data from the selected file, and assign it to a dataframe
#' 
#' @param filename the name of the file to read
#' @param header TRUE if colnames are present FALSE if colnames are not present
#' @param selected the user should input the number of the columns that represent
  #' 	strain-smoothed (gamma), strain rate-smoothed (gamma dot), stress smoothed (tau recon), Elast-Stress (FTtau_e), Visco-Stress (FTtau_v), raw time (time), raw stress (tau), raw strain (gamma)
  #' 	i.e. selected=c(2, 3, 4, 0, 0, 1, 0, 0) means that the second column of your data is the strain rate smoothed, the third column is the stress smoothed, the stress smoothed is the fourth column in the original data,
  #     we do not have data for Elastic-Stress and Visco-Stress, the raw time is the first column in the original data
  #' 	and finally that we do not have data for the raw stress and raw strain
#'@param ... parameters of read.csv
#'
#' @return a dataframe with all the columns assigned
#'
#' @author Giorgio Luciano and Serena Berretta, Simon Rogers Group for Soft Matter (matlab version)
#'  
#' @export 
#' @import pracma




rpp_read <- function(filename, header=TRUE, selected=c(2, 3, 4, 0, 0, 1, 0, 0), ...) {

  if (header == TRUE) {
    dat <- read.csv(filename, na.strings = "NA", dec = ".", header = TRUE, strip.white = TRUE, skipNul = TRUE, ...)
    colnames(dat) <- list()
  }
  else {
    dat <- read.csv(filename, na.strings = "NA", dec = ".", header = FALSE, strip.white = TRUE, skipNul = TRUE, ...)
  }

  istrain <- selected[1]
  istrain_rate <- selected[2]
  istress <- selected[3]
  ielast <- selected[4]
  ivisco <- selected[5]
  iraw_time <- selected[6]
  iraw_stress<- selected[7]
  iraw_strain <- selected[8]

  if (istrain != 0) {
    strain <- dat[istrain]
  }
  else {
    print("strain is missing")
	strain <-  data.frame(numeric(length = dim(dat)[1]))
  }

  if (istrain_rate != 0) {
    strain_rate <- dat[istrain_rate]
  }
  else {
    print("strain_rate_is_missing")
    strainrate <-  data.frame(numeric(length = dim(dat)[1]))
  }

  if (istress != 0) {
    stress <- dat[istress]
  }
  else {
    print("stress is missing")
	stress <-  data.frame(numeric(length = dim(dat)[1]))
  }

  if (ielast != 0) {
    elast_stress <- dat[ielast]
  }
  else {
    print("Elastic-Stress is missing")
    elast_stress <-  data.frame(numeric(length = dim(dat)[1]))
  }

  if (ivisco != 0) {
    visco_stress <- dat[ivisco]
  }
  else {
    print("Visco-Stress is missing")
	visco_stress <- data.frame(numeric(length = dim(dat)[1]))
  }

    if (iraw_time != 0) {
    raw_time <- dat[iraw_time]
  }
  else {
    print("Raw Time is missing")
    raw_time <- data.frame(numeric(length = dim(dat)[1]))
  }

    if (iraw_stress != 0) {
    raw_stress <- dat[iraw_stress]
  }
  else {
    print("Raw Stress is missing")
    raw_stress <- numeric(length = dim(dat)[1])
  }
  
      if (iraw_strain != 0) {
    raw_strain <- dat[iraw_strain]
  }
  else {
    print("Raw Strain is missing")
    raw_strain <- numeric(length = dim(dat)[1])
  }

   ds <- data.frame(strain, strain_rate, stress, elast_stress, visco_stress, raw_time, raw_stress,raw_strain)
   colnames(ds) <- c("strain", "strain_rate", "stress", "elast_stress", "visco_stress", "raw_time", "raw_stress","raw_strain")
 
  return(ds)
}


#' Read function
#'
#' @description  This function reads data from a dataframe
#' @param dat dataframe of input
#' @param selected the user should input the number of the columns that represent
#' 	strain-smoothed (gamma), strain rate-smoothed (gamma dot), stress smoothed (tau recon), Elast-Stress (FTtau_e), Visco-Stress (FTtau_v), raw time (time), raw stress (tau), raw strain (gamma)
#' 	i.e. selected=c(2, 3, 4, 0, 0, 1, 0, 0) means that the second column of your data is the strain rate smoothed, the third column is the stress smoothed, the stress smoothed is the fourth column in the original data,
#     we do not have data for Elastic-Stress and Visco-Stress, the raw time is the first column in the original data
#' 	and finally that we do not have data for the raw stress and raw strain
#'@param ... parameters of read.csv
#'
#' @return a dataframe with all the columns assigned
#'
#' @author Giorgio Luciano and Serena Berretta, Simon Rogers Group for Soft Matter (matlab version)
#'
#' @examples data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#'  
#' @export 
#' @import pracma


rpp_read2 <- function(dat , selected=c(2, 3, 4, 0, 0, 1, 0, 0), ...) {

  istrain <- selected[1]
  istrain_rate <- selected[2]
  istress <- selected[3]
  ielast <- selected[4]
  ivisco <- selected[5]
  iraw_time <- selected[6]
  iraw_stress<- selected[7]
  iraw_strain <- selected[8]
  
  if (istrain != 0) {
    strain <- dat[istrain]
  }
  else {
    print("strain is missing")
    strain <-  data.frame(numeric(length = dim(dat)[1]))
  }
  
  if (istrain_rate != 0) {
    strain_rate <- dat[istrain_rate]
  }
  else {
    print("strain_rate_is_missing")
    strainrate <-  data.frame(numeric(length = dim(dat)[1]))
  }
  
  if (istress != 0) {
    stress <- dat[istress]
  }
  else {
    print("stress is missing")
    stress <-  data.frame(numeric(length = dim(dat)[1]))
  }
  
  if (ielast != 0) {
    elast_stress <- dat[ielast]
  }
  else {
    print("Elastic-Stress is missing")
    elast_stress <-  data.frame(numeric(length = dim(dat)[1]))
  }
  
  if (ivisco != 0) {
    visco_stress <- dat[ivisco]
  }
  else {
    print("Visco-Stress is missing")
    visco_stress <- data.frame(numeric(length = dim(dat)[1]))
  }
  
  if (iraw_time != 0) {
    raw_time <- dat[iraw_time]
  }
  else {
    print("Raw Time is missing")
    raw_time <- data.frame(numeric(length = dim(dat)[1]))
  }
  
  if (iraw_stress != 0) {
    raw_stress <- dat[iraw_stress]
  }
  else {
    print("Raw Stress is missing")
    raw_stress <- numeric(length = dim(dat)[1])
  }
  
  if (iraw_strain != 0) {
    raw_strain <- dat[iraw_strain]
  }
  else {
    print("Raw Strain is missing")
    raw_strain <- numeric(length = dim(dat)[1])
  }
  
  ds <- data.frame(strain, strain_rate, stress, elast_stress, visco_stress, raw_time, raw_stress,raw_strain)
  colnames(ds) <- c("strain", "strain_rate", "stress", "elast_stress", "visco_stress", "raw_time", "raw_stress","raw_strain")
  
  return(ds)
}

