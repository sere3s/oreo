#' Export results of the performed SPP analysis in csv format
#' 
#' @description # This function export the output the SPP analysis (performed via FFT or Numeric Analysis) and export it to csv files
#'
#' @param out  output of the SPP analysis (performed via FFT or Numeric Analysis)
#' @param myfilename name of the file where to save results (csv)
#'
#' @return {No return value}
#'
#' @author Simon Rogers Group for Soft Matter (matlab version), Giorgio Luciano and Serena Berretta (R version)
#' @keywords results export csv
#'  
#' @export
#' @import openxlsx utils

rpp_out_csv <- function(out,myfilename="my_models.xlsx") {
  
  spp_data_in <- out$spp_data_in
  spp_params <- out$spp_params
  spp_data_out <- out$spp_data_out
  fsf_data_out <- out$fsf_data_out
  ft_out <- out$ft_out
  
  write.csv(spp_data_in,paste(myfilename,'_spp_data_in.csv'))
  write.csv(spp_params,paste(myfilename,'_spp_params.csv'))
  write.csv(spp_data_out,paste(myfilename,'_spp_data_out.csv'))
  write.csv(fsf_data_out,paste(myfilename,'_fsf_data_in.csv'))
  write.csv(ft_out,paste(myfilename,'_ft_out.csv'))
}


#' Export results of the performed SPP analysis in xls format
#' 
#' @description # This function export the output the SPP analysis (performed via FFT or Numeric Analysis) and export it to xls files
#'
#' @param out  output of the SPP analysis (performed via FFT or Numeric Analysis)
#' @param myfilename name of the file where to save results in xls format
#'
#' @return {No return value}
#'
#' @author Simon Rogers Group for Soft Matter (matlab version), Giorgio Luciano and Serena Berretta (R version)
#' @keywords  results export xls
#'  
#' @export
#' @import openxlsx
#' 
#' 
rpp_out_excel <- function(out,myfilename="my_models.xlsx") {
  
  spp_data_in <- out$spp_data_in
  spp_params <-  out$spp_params
  spp_data_out <- out$spp_data_out
  fsf_data_out <- out$fsf_data_out
  ft_out <- out$ft_out
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "spp_data_in")
  addWorksheet(wb, "spp_params")
  addWorksheet(wb, "spp_data_out")
  addWorksheet(wb, "fsf_data_out")
  addWorksheet(wb, "ft_out")
  
  writeData(wb,1,spp_data_in,startRow=1, startCol=1)
  writeData(wb,2,spp_params,startRow=1, startCol=1)
  writeData(wb,3,spp_data_out,startRow=1, startCol=1)
  writeData(wb,4,fsf_data_out,startRow=1, startCol=1)
  writeData(wb,5,ft_out,startRow=1, startCol=1)
  
  saveWorkbook(wb, myfilename, overwrite = FALSE)
  #--------------------------------------
  #openXL(wb)
}

#--------------------------------------

