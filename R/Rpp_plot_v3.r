#' Stress-Strain plot
#' 
#' @description create Stress Strain Plot
#' @param stress data the output matrix from fft analysis or numerical differentiation analysis
#' @param strain data the output matrix from fft analysis or numerical differentiation analysis
#' @param stress_in data the input matrix from fft analysis or numerical differentiation analysis
#' @param strain_in data the input matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return  {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @keywords SPP Stress-Strain plot
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' strain= out$spp_data_out$strain
#' stress= out$spp_data_out$stress
#' strain_in= out$spp_data_in$strain
#' stress_in= out$spp_data_in$stress
#' plotStressStrain(stress, strain,strain_in,stress_in)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25 

plotStressStrain <- function(stress, strain,strain_in,stress_in,...){
  plot(strain, stress , main="stress-strain", xlab="Strain [-]", ylab ="Stress [Pa]",type="l", col="red",lwd=2)
  lines(strain_in,stress_in, col="blue")
  legend("topleft",legend=c("raw stress", "reconstructed"),col=c("red", "blue"), lty=1:2, cex=0.6)
  abline(h=0, v=0,lty=2)
}


#' Stress-Rate plot
#' 
#' @description create Stress Rate Plot
#' @param stress data the output matrix from fft analysis or numerical differentiation analysis
#' @param rate data the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @keywords SPP Stress-Strain plot
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' rate= out$spp_data_out$rate
#' stress= out$spp_data_out$stress
#' plotStressRate(stress, rate)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25 

plotStressRate <- function(stress, rate,...){
  plot(stress,rate,main="stress-rate",xlab ="Rate [1/s]", ylab="Stress [Pa]",type="l", col="red",lwd=2)
  abline(h=0, v=0,lty=2)
}


#' Cole-Cole plot
#' 
#' @description create Cole-Cole plot
#' @param Gp_t from the output matrix from fft analysis or numerical differentiation analysis
#' @param Gpp_t from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @keywords SPP Cole-Cole plot
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' Gp_t= out$spp_data_out$Gp_t
#' Gpp_t= out$spp_data_out$Gpp_t
#' plotColeCole(Gp_t,Gpp_t)}
#' 
#' @export
#' @import graphics
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25 
plotColeCole <- function(Gp_t,Gpp_t,...){
  plot(Gp_t,Gpp_t,main="Cole-Cole plot",xlab=expression("G'"[t]*" [Pa]"),ylab=expression("G''"[t]*" [Pa]"),type="l", col="red",lwd=2)
  abline(h=0,v=0, lty=2)
  abline(a=c(0,0), b=c(1,1),lty=2)
}





#' VGP plot
#' 
#' @description create VGP plot
#' @param G_star_t from the output matrix from fft analysis or numerical differentiation analysis
#' @param delta_t from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' G_star_t= out$spp_data_out$G_star_t
#' delta_t= out$spp_data_out$delta_t
#' plotVGP(G_star_t,delta_t)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25 
plotVGP <- function(G_star_t,delta_t,...){
  plot(G_star_t,delta_t,main="VGP plot",xlab=expression("G*"[t]*" [Pa]"),ylab=expression("delta"[t]*" [rad]"),type="l", col="red",lwd=2)
  abline(h=0, v=0,lty=2)}




#' Gp_t_dot vs Gpp_t_dot
#' 
#' @description create Gp_t_dot vs Gpp_t_dot
#' @param Gp_t_dot from the output matrix from fft analysis or numerical differentiation analysis
#' @param Gpp_t_dot from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' 
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' Gp_t_dot= out$spp_data_out$Gp_t_dot
#' Gpp_t_dot= out$spp_data_out$Gpp_t_dot
#' plotGpdot(Gp_t_dot,Gpp_t_dot)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25 
plotGpdot <- function(Gp_t_dot,Gpp_t_dot,...){
  plot(Gp_t_dot,Gpp_t_dot,main=expression("dG'"[t]*"/dt-dG''"[t]*"/dt"), xlab=expression("dG'"[t]*"/dt [Pa/s]"),ylab=expression("dG''"[t]*"/dt [Pa/s]"),type="l", col="red",lwd=2 ) 
}


#' Speed-G'_{t} plot
#' 
#' @description create Speed-G'_{t} plot
#' @param Gp_t from the output matrix from fft analysis or numerical differentiation analysis
#' @param G_speed from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' Gp_t= out$spp_data_out$Gp_t
#' G_speed= out$spp_data_out$G_speed
#' plotSpeedGp(Gp_t,G_speed)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25

plotSpeedGp <- function(Gp_t,G_speed,...){
  plot(Gp_t,G_speed,main=expression("Speed-G'"[t]*"") ,xlab=expression("G'"[t]*" [Pa]"),ylab=expression("Speed-G''"[t]*" [Pa/s]"),type="l", col="red",lwd=2 )}




#' Speed-G''_{t} plot
#' 
#' @description create Speed-G''_{t} plot
#' @param Gpp_t from the output matrix from fft analysis or numerical differentiation analysis
#' @param G_speed from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' G_speed= out$spp_data_out$G_speed
#' Gpp_t= out$spp_data_out$Gpp_t
#' plotSpeedGpp(G_speed,Gpp_t)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25
plotSpeedGpp <- function(G_speed,Gpp_t,...){
  plot(G_speed,Gpp_t,main=expression("Speed-G''"[t]*"") ,xlab=expression("Speed [Pa/s]"),ylab=expression("G''"[t]*" [Pa]"),type="l", col="red",lwd=2 )
}




#' Strain Delta Plot
#' @description create Strain Delta Plot
#' @param strain from the output matrix from fft analysis or numerical differentiation analysis
#' @param delta_t from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' strain= out$spp_data_out$strain
#' delta_t= out$spp_data_out$delta_t
#' plotDeltaStrain(strain,delta_t)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25
plotDeltaStrain <- function(strain,delta_t,...){
  plot(strain,delta_t,main=expression("delta"[t]*"-strain"),xlab=expression("strain [-]"),ylab=expression("delta"[t]*" [rad]"),type="l", col="red",lwd=2 )}



#' Strain Delta Plot
#' @description create Strain Delta Plot
#' @param strain from the output matrix from fft analysis or numerical differentiation analysis
#' @param delta_t_dot from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' strain= out$spp_data_out$strain
#' delta_t_dot= out$spp_data_out$delta_t_dot
#' plotPAV(strain,delta_t_dot)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25
plotPAV  <- function(strain,delta_t_dot,...){
  plot(strain,delta_t_dot,main="PAV-strain",xlab=expression("strain [-]"),ylab=expression("PAV [] (time-normalized)"),type="l", col="red", lwd=2)}




#' Strain Displacement Stress
#' @description Strain Displacement Stress
#' @param strain from the output matrix from fft analysis or numerical differentiation analysis
#' @param disp_stress from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' strain= out$spp_data_out$strain
#' disp_stress= out$spp_data_out$disp_stress
#' plotDisp(strain,disp_stress)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25
plotDisp  <- function(strain,disp_stress,...){
  plot(strain,disp_stress,main="displacement stress-strain",xlab=expression("strain [-]"),ylab=expression("displacement stress [Pa]"),type="l", col="red", lwd=2 )}




#' Strain Gp_t,eq_strain_est
#' @description  Strain Gp_t,eq_strain_est
#' @param Gp_t from the output matrix from fft analysis or numerical differentiation analysis
#' @param eq_strain_est from the output matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' Gp_t= out$spp_data_out$Gp_t
#' eq_strain_est= out$spp_data_out$eq_strain_est
#' plotStrain(Gp_t,eq_strain_est)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25
plotStrain <- function(Gp_t,eq_strain_est,...){
  plot(Gp_t,eq_strain_est,main="est. eq. strain-tan(delta)",xlab=expression("G'"[t]*" [Pa]"),ylab=expression("esp eq. strain[-]"),type="l", col="red", lwd=2 )}


#Waveform Comparison
#' Strain time_wave,strain
#' @description  Strain time_wave, strain
#' @param time_wave time from output data
#' @param strain from the output matrix from fft analysis or numerical differentiation analysis
#' @param time_wave_in time from input data
#' @param strain_in from the input matrix from fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' time_wave= out$spp_data_out$time_wave
#' strain= out$spp_data_out$strain
#' time_wave_in= out$spp_data_in$time_wave
#' strain_in= out$spp_data_in$strain
#' plotTimeStrain(time_wave,strain,time_wave_in,strain_in)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25
plotTimeStrain <- function(time_wave,strain,time_wave_in,strain_in, ...){
  plot(time_wave,strain,main="Strain-time",xlab=expression("Time [s]"),ylab = expression("Strain [-]"),type="l", col="red",xlim=c(0,3), lwd=2)
  lines(time_wave_in,strain_in,lty=2)}


#' Rate, time_wave plot
#' @description create Rate, time_wave plot
#' @param time_wave from the output matrix from fft analysis or numerical differentiation analysis
#' @param rate from the output matrix from fft analysis or numerical differentiation analysis
#' @param time_wave_in raw time from input data
#' @param strain_rate strain rate from input data
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' time_wave= out$spp_data_out$time_wave
#' rate= out$spp_data_out$rate
#' time_wave_in= out$spp_data_in$time_wave
#' strain_rate= out$spp_data_in$strain_rate
#' plotTimeRate(time_wave,rate,time_wave_in,strain_rate)}
#' 
#' @export
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25

plotTimeRate <- function(time_wave,rate,time_wave_in,strain_rate,...){
  plot(time_wave,rate,main="Rate-time",xlab=expression("Time [s]"),ylab = expression("Rate [1/s]"),type="l", col="red",xlim=c(0,3), lwd=2)
  lines(time_wave_in,strain_rate,lty=2)}

#' Stress-Time plot
#' 
#' @description create Stress-Time plot
#' @param time_wave from the output matrix from fft analysis or numerical differentiation analysis
#' @param stress from the output matrix from fft analysis or numerical differentiation analysis
#' @param time_wave_in raw time from input data
#' @param strain_rate strain rate from input data
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' time_wave= out$spp_data_out$time_wave
#' stress= out$spp_data_out$stress
#' time_wave_in= out$spp_data_in$time_wave
#' strain_rate= out$spp_data_in$strain_rate
#' plotTimeStress(time_wave,stress,time_wave_in,strain_rate)}
#' 
#' @export

plotTimeStress <- function(time_wave,stress,time_wave_in,strain_rate,...){
  plot(time_wave,stress,main="Stress-time",xlab=expression("Time [s]"),ylab=expression("Stress [Pa]"),type="l", col="red", xlim=c(0,3), lwd=2)
  lines(time_wave_in,strain_rate,lty=2)}

#' Stress-Time plot
#' 
#' @description create Stress-Time plot
#' @param time_wave_in raw time from input data
#' @param stress_in stress from input data
#' @param time_wave from the output matrix from fft analysis or numerical differentiation analysis
#' @param stress from the output matrix from fft analysis or numerical differentiation analysis
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)
#' time_wave_in= out$spp_data_in$time_wave
#' stress_in= out$spp_data_in$stress
#' time_wave= out$spp_data_out$time_wave
#' stress= out$spp_data_out$stress
#' plotStressTime(time_wave_in,stress_in,time_wave,stress)}
#' 
#' @export

plotStressTime <- function(time_wave_in,stress_in,time_wave,stress){
  plot(time_wave_in,stress_in,xlab=expression("Time[s]"),ylab=expression("Stress [Pa]"),type="l", col="black",lty=2 )
  lines(time_wave,stress,main="Stress-time",type="l", col="red", lwd=2 )}

#' Fourier Harmonic Magnitudes plot
#' 
#' @description create Fourier Harmonic Magnitudes plot
#' @param ft_amp from the output matrix from fft analysis or numerical differentiation analysis
#' @param fft_resp from the output matrix from fft analysis or numerical differentiation analysis
#' @param spp_params input parameters used for the fft analysis or numerical differentiation analysis
#' @param ... parameters of plot()
#' @return {No return value}
#' @author Giorgio Luciano and Serena Beretta, based on the Plotting functions created by Simon Rogers Group for Soft Matter
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- rpp_fft(time_wave,resp_wave,L=1024,omega=3.16 , M=15,p=1)
#' ft_amp= out$ft_out$ft_amp
#' fft_resp= out$ft_out$fft_resp
#' spp_params= out$spp_params
#' plotFft(ft_amp,fft_resp,spp_params)}
#' 
#' @export
plotFft <- function(ft_amp,fft_resp,spp_params,...){
  plot(ft_amp,fft_resp,main="Fourier spectrum",xlab=expression("Number of harmonics [-]"),ylab=expression(paste(I[n],"/",I[1],"[-]")),log="y")
  segments(x0=as.numeric(spp_params[2]), y0=10^-10, x1 = as.numeric(spp_params[2]), y1 = 0.7,col="red",lwd=2)}




