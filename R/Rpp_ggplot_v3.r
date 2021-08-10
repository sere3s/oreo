
# This functions produce figures using the SPP analysis; if specified by
# the user, the figures can also be saved to .jpg files
    #Inputs: spp_data_in = Lx4 matrix of the data input to the analysis,
                #with each row representing a measuring point. The columns
                #are: 1 - time [s]
                # 2 - strain [-]
                # 3 - rate [1/s]
                # 4 - stress [Pa]
            #spp_params = 1x6 vector containing input parameters for the
                #spp analysis (frequency,#harmonics used,#cycles,maximum
                ##harmonics,step size,num mode). All non-applicable
                #values are NaN
            #spp_data_out = Lx15 matrix containing all primary results from
                #the SPP analysis. The columns are:
                # 1 - time [s]
                # 2 - strain [-]
                # 3 - rate [1/s]
                # 4 - stress [Pa]
                # 5 - G'_t [Pa]
                # 6 - G''_t [Pa]
                # 7 - |G*_t| [Pa]
                # 8 - tan(delta_t) []
                # 9 - delta_t [rad]
                # 10 - displacement stress [Pa]
                # 11 - estimated equilibrium strain [-]
                    #(valid for G'_t>>G''_t)
                # 12 - derivative of G'_t [Pa/s]
                # 13 - derivative of G''_t [Pa/s]
                # 14 - speed of G*_t [Pa/s]
                # 15 - Normalized phase angle velocity []
                    #(assumes that strain is sinusoidal)
            #ft_out = Lx2 vector containing the fourier domain filtering
                #results. If numerical differentiation was used, this is
                #NaN. The columns are:
                # 1 - domain
                # 2 - normalized harmonics
            #pf_state = binary value determining whether to save figures
                #as .jpg files
            #analtype = analysis method used (as a string)
            #fname = name of file from which the data originated
            #f_vect = vector that specifies which figures to create
		

#' titolo....
#' 
#' @description cosa fa la funzione...
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 


theme_giolu <- function () { 
    theme_bw(base_size=12, base_family="sans") %+replace% 
        theme(
            panel.background  = element_blank(),
            plot.background = element_rect(fill="transparent", colour=NA), 
            legend.background = element_rect(fill="transparent", colour=NA),
            legend.key = element_rect(fill="transparent", colour=NA),
			panel.grid.major = element_blank(), 
			panel.grid.minor = element_blank()
        )
}


#' titolo....Stress-Strain plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 

ggplotStressStrain <- function(data=spp_data_out,spp_data_in,...){
	 ggplot(data,...) + 
     geom_path(aes(x=strain, y=stress),size=1.1)+
	 geom_path(data=(spp_data_in),aes(x=strain, y=stress),colour="red") + 
	 labs(title="stress-strain",x ="Strain [-]", y = "Stress [Pa]") +
	 geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	 geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	 theme_giolu()}
	 


#' titolo....Stress-Rate plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotStressRate <- function(data=spp_data_out,...){
	  ggplot(data,...) +
	  geom_path(aes(x=stress, y=rate),colour="red",size=1.1) + 
	  labs(title="stress-rate",x ="Rate [1/s]", y = "Stress [Pa]") +
	  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	  geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	  theme_giolu()}
	 





#' titolo....Cole-Cole plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotColeCole <- function(data=spp_data_out,...){
	  ggplot(data,...) + 
	  geom_path(aes(x=Gp_t, y=Gpp_t),colour="red",size=1.1) +
	  labs(title="Cole-Cole plot",x =expression("G'"[t]*" [Pa]"), y = expression("G''"[t]*" [Pa]")) +
	  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	  geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	  theme_giolu()}
	 
	



#' titolo....VGP plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotVGP <- function(data=spp_data_out,...){
	  ggplot(data,...) + 
	  geom_path(aes(x=G_star_t, y=delta_t),colour="red",size=1.1) +
	  labs(title="VGP plot",x =expression("G*"[t]*" [Pa]"), y = expression("delta"[t]*" [rad]")) +
	  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	  geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	  theme_giolu()}
	 




#' titolo....dG"_{t}/dt-dG'_{t}/dt plot 
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotGpdot <- function(data=spp_data_out,...){
	  ggplot(data,...) + 
	  geom_path(aes(x=Gp_t_dot, y=Gpp_t_dot),colour="red",size=1.1) +
	  labs(title=expression("dG'"[t]*"/dt-dG''"[t]*"/dt"),x=expression("dG'"[t]*"/dt [Pa/s]"), y =expression("dG''"[t]*"/dt [Pa/s]")) +
	  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	  geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	  theme_giolu()}
	  




#' titolo....Speed-G'_{t} plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotSpeedGp <- function(data=spp_data_out,...){
      ggplot(data,...) +
	  geom_path(aes(x=Gp_t, y=G_speed),colour="red",size=1.1) +
	  labs(title=expression("Speed-G'"[t]*""),x=expression("G'"[t]*" [Pa]"), y = expression("Speed-G''"[t]*" [Pa/s]")) +
	  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	  geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	  theme_giolu()}
	  





#' titolo....Speed-G"_{t} plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotSpeedGpp <- function(data=spp_data_out,...){
	   geom_path(aes(x=G_speed, y=Gpp_t),colour="red",size=1.1) +
	   labs(title=expression("Speed-G''"[t]*""),x =expression("Speed [Pa/s]"), y = expression("G''"[t]*" [Pa]")) +
	   geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	   geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	   theme_giolu()}
	  
	   



#' titolo....delta_t v strain plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotDeltaStrain <- function(data=spp_data_out,...){
	   ggplot(data,...) + 
	   geom_path(aes(x=strain, y=delta_t),colour="red",size=1.1) +
	   labs(title=expression("delta"[t]*"-strain"),x =expression("strain [-]"), y = expression("delta"[t]*" [rad]")) +
	   geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	   geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	   geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed")+
	   theme_giolu()}
   




#' titolo....PAV v strain plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotPAV <- function(data=spp_data_out,...){
	   ggplot(data,...) +
	   geom_path(aes(x=strain, y=delta_t_dot),colour="red",size=1.1) +
	   labs(title="PAV-strain",x =expression("strain [-]"), y = expression("PAV [] (time-normalized)")) +
	   geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	   geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	   geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed")+
	   theme_giolu()}
	   




#' titolo....displacement stress plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotDisp <- function(data=spp_data_out,...){
	   ggplot(data,...) +
	   geom_path(aes(x=strain, y=disp_stress),colour="red",size=1.1) +
	   labs(title="displacement stress-strain",x =expression("strain [-]"), y = expression("displacement stress [Pa]")) +
	   geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	   geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	   geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed")+
	   theme_giolu()}
	   
	   




#' titolo....estimated eq strain plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotStrain <- function(data=spp_data_out,...){
	   ggplot(data,...) +
	   geom_path(aes(x=Gp_t, y=eq_strain_est),colour="red",size=1.1) +
	   labs(title="est. eq. strain-tan(delta)",x =expression("G'"[t]*" [Pa]"), y =expression("esp eq. strain[-]")) +
	   geom_vline(xintercept = 0, linetype="dashed", color = "black") +
	   geom_hline(yintercept = 0, linetype="dashed", color = "black") +
	   geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed")+
	   theme_giolu()}
	   





#' titolo....Strain-Time plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotTimeStrain <- function(data=spp_data_in,...){
	   ggplot(...) +
	   geom_path(aes(x=time_wave, y=strain),colour="red",size=1.1) +
	   geom_path(data=(spp_data_in),aes(x=time_wave, y=strain)) +
	   labs(title="Strain-time",x = expression("Time [s]"), y = expression("Strain [-]") ) +
	   theme_giolu()}
	   




#' titolo....Rate-Time plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotTimeRate <- function(data=spp_data_in,...){
	   ggplot(data,...) +
	   geom_path(aes(x=time_wave, y=rate),colour="red",size=1.1)+
	   geom_path(data=(spp_data_in),aes(x=time_wave, y=strain_rate)) +
	   labs(title="Rate-time",x = expression("Time [s]"), y = expression("Rate [1/s]")) +
	   theme_giolu()}
	   




#' titolo....Stress-Time plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotTimeStress <- function(data=spp_data_in,...){
	   ggplot(data,...) +
	   geom_path(aes(x=time_wave, y=stress),colour="red",size=1.1) +
	   geom_path(data=(spp_data_in),aes(x=time_wave, y=stress)) +
	   labs(title="Stress-time",x = expression("Time [s]"), y = expression("Stress [Pa]")) +
	   theme_giolu()}

	   



#' titolo....Fourier Harmonic Magnitudes plot
#' 
#' @description cosa fa la funzione...
#' @param data
#' @return 
#' @author 
#' @keywords 
#' @examples \dontest{
#' plot()
#' }
#' @export
#' @references 
ggplotFft <- function(data=ft_out,spp_params,...){
		ggplot(data,...) + 
		geom_segment(aes(x=ft_amp, xend=ft_amp, y=0, yend=fft_resp) , size=1, color="blue") +
		geom_segment(aes(x=as.numeric(spp_params[2]), y = 10^-10, xend = as.numeric(spp_params[2]), yend =  0.7),colour="red",size=2)+
		scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
		labs(title="Fourier Spectrum",x = expression("Number of harmonics [-]"), y = expression(paste(I[n],"/",I[1],"[-]"))) +
		annotation_logticks(sides = "lr") +
	    theme_giolu()}

  

    


	   

    
