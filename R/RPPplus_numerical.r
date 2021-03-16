#' SPP Analysis via numerical differentiation
#'
#' @description  applies the SPP Analysis by means of a numerical differentiation.
#' 
#' @param time_wave Lx1 vector of time at each measurement point
#' @param resp_wave Lx3 matrix of the strain, rate and stress data,with each row representing a measuring point
#' @param L  number of measurement points in the extracted data
#' @param k step size for numerical differentiation
#' @param num_mode numerical method
#'
#' @return a list with the following data frame
#'         spp_data_in= the data frame with the data
#'				 spp_params=spp_params, 
#'				 spp_data_out= Length,frequency,harmonics,cycles,max_harmonics,step_size
#'				 fsf_data_out= Tx,Ty,Tz,Nx,Ny,Nz,Bx,By,Bz coordinates of the trajectory (T=tangent,N=principal Normal,B=Binormal Vectors)
#'				 ft_out=data frame with that includes time_wave,strain,rate,stress,Gp_t,Gpp_t,G_star_t,tan_delta_t,delta_t,disp_stress,eq_strain_est,Gp_t_dot,Gpp_t_dot,G_speed,delta_t_dot)
#'
#'
#' @author  Simon Rogers Group for Soft Matter (matlab version), Giorgio Luciano and Serena Berretta (R version)
#' @keywords SPP numerical differentiation
#'
#' @examples \donttest{ data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- Rpp_num(time_wave, resp_wave , L=1024, k=8, num_mode=1)}
#'  
#' @export  
#' 
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25


        
Rpp_num <- function(time_wave,resp_wave,L,k,num_mode)
{

#Calculate the frequency of the response
	omega = max(abs(resp_wave[,2]))/max(abs(resp_wave[,1])) 

	#Calculate the average timestep size
	ddt = sum(time_wave[seq(2,L)]-time_wave[seq(1,(L-1))])/(L-1) 

	#Initialize result vectors
	rd = as.data.frame(zeros(L,3))
	rdd = as.data.frame(zeros(L,3))
	rddd = as.data.frame(zeros(L,3))

	#Perform Numerical differentiation
	for (p in seq(1,L))
		{
		if (num_mode == 1)
			{
			   #use "standard" differentiation on the response data (does not make
				#assumtions abot the form of the data, uses forward/backward
				#difference at ends and centered derivative elsewhere)
				if (p<=3*k)
				{
				rd[p,] = (-resp_wave[p+2*k,]+4*resp_wave[p+k,]-3*resp_wave[p,])/(2*k*ddt) 
				rdd[p,] = (-resp_wave[p+3*k,]+4*resp_wave[p+2*k,]-5*resp_wave[p+k,]+2*resp_wave[p,])/((k*ddt)^2) 
				rddd[p,] = (-3*resp_wave[p+4*k,]+14*resp_wave[p+3*k,]-24*resp_wave[p+2*k,]+18*resp_wave[p+k,]-5*resp_wave[p,])/(2*(k*ddt)^3) 
				}
				else if (p>=(L-3*k))
				{
				rd[p,] = (resp_wave[p-2*k,]-4*resp_wave[p-k,]+3*resp_wave[p,])/(2*k*ddt) 
				rdd[p,] = (-resp_wave[p-3*k,]+4*resp_wave[p-2*k,]-5*resp_wave[p-k,]+2*resp_wave[p,])/((k*ddt)^2) 
				rddd[p,] = (3*resp_wave[p-4*k,]-14*resp_wave[p-3*k,]+24*resp_wave[p-2*k,]-18*resp_wave[p-k,]+5*resp_wave[p,])/(2*(k*ddt)^3) 
				}
				else
				{
				rd[p,] = (-resp_wave[p+2*k,]+8*resp_wave[p+k,]-8*resp_wave[p-k,]+resp_wave[p-2*k,])/(12*k*ddt) 
				rdd[p,] = (-resp_wave[p+2*k,]+16*resp_wave[p+k,]-30*resp_wave[p,]+16*resp_wave[p-k,]-resp_wave[p-2*k,])/(12*(k*ddt)^2)
				rddd[p,] = (-resp_wave[p+3*k,]+8*resp_wave[p+2*k,]-13*resp_wave[p+k,]+13*resp_wave[p-k,]-8*resp_wave[p-2*k,]+resp_wave[p-3*k,])/(8*(k*ddt)^3) 
				}
			}
		else if (num_mode == 2) 
			{
			# use "looped" differentiation on the response data (assumes steady
				#state oscilatory, uses centered difference everywhere by
				#connecting the data in a loop)
			p3 = p+3*k 
			p2 = p+2*k 
			p1 = p+k 
			pn1 = p-k 
			pn2 = p-2*k 
			pn3 = p-3*k 
			
			if (p3>L)
				{p3 = p3-L}
			if (p2>L)
				{p2 = p2-L} 
			if (p1>L)
			   {p1 = p1-L} 
			if (pn1<1)
			   {pn1 = pn1+L} 
			if (pn2<1)
				{pn2 = pn2+L} 
			if (pn3<1)
				{pn3 = pn3+L} 
			rd[p,] = (-resp_wave[p2,]+8*resp_wave[p1,]-8*resp_wave[pn1,]+resp_wave[pn2,])/(12*k*ddt) 
			rdd[p,] = (-resp_wave[p2,]+16*resp_wave[p1,]-30*resp_wave[p,]+16*resp_wave[pn1,]-resp_wave[pn2,])/(12*(k*ddt)^2) 
			rddd[p,] = (-resp_wave[p3,]+8*resp_wave[p2,]-13*resp_wave[p1,]+13*resp_wave[pn1,]-8*resp_wave[pn2,]+resp_wave[pn3,])/(8*(k*ddt)^3)
			}	
		else
			{
			print('differentiation type not valid')
			}
	}
	

	rd_x_rdd = data.frame(rd[,2]*rdd[,3]-rd[,3]*rdd[,2],rd[,3]*rdd[,1]-rd[,1]*rdd[,3],rd[,1]*rdd[,2]-rd[,2]*rdd[,1])
	rd_x_rd_x_rdd = data.frame(rd[,2]*rd_x_rdd[,3]-rd[,3]*rd_x_rdd[,2],rd[,3]*rd_x_rdd[,1]-rd[,1]*rd_x_rdd[,3],rd[,1]*rd_x_rdd[,2]-rd[,2]*rd_x_rdd[,1]) 
	mag_rd = sqrt(rd[,1]^2+rd[,2]^2+rd[,3]^2) 
	Rec_Tvect = rd 
	mag_rd_x_rdd = sqrt(rd_x_rdd[,1]^2+rd_x_rdd[,2]^2+rd_x_rdd[,3]^2) 
	Rec_Nvect = -rd_x_rd_x_rdd/(mag_rd*mag_rd_x_rdd) 
	Rec_Bvect = rd_x_rdd/mag_rd_x_rdd 
	Gp_t = -rd_x_rdd[,1]/rd_x_rdd[,3] 
	Gpp_t = -rd_x_rdd[,2]/rd_x_rdd[,3] 
	Gp_t_dot = -rd[,2]*(rddd[,1]*rd_x_rdd[,1]+rddd[,2]*rd_x_rdd[,2]+rddd[,3]*rd_x_rdd[,3])/(rd_x_rdd[,3])^2 
	Gpp_t_dot = rd[,1]*(rddd[,1]*rd_x_rdd[,1]+rddd[,2]*rd_x_rdd[,2]+rddd[,3]*rd_x_rdd[,3])/(rd_x_rdd[,3])^2 
	G_speed = sqrt(Gp_t_dot^2+Gpp_t_dot^2) 

	rd_tn = rd/omega 
	rdd_tn = rdd/omega^2 
	rddd_tn = rddd/omega^3 

	G_star_t = sqrt(Gp_t^2+Gpp_t^2) 
	tan_delta_t = Gpp_t/Gp_t 
	is_Gp_t_neg = (Gp_t<0) 
	delta_t = atan(tan_delta_t)+pi*is_Gp_t_neg 
	delta_t_dot = -1*(rd_tn[,3]*(rddd_tn[,3] + rd_tn[,3]))/((rdd_tn[,3])^2+(rd_tn[,3])^2) 

	disp_stress = resp_wave[,3]-(Gp_t*resp_wave[,1]+Gpp_t*resp_wave[,2]/omega) 
	eq_strain_est = resp_wave[,1]-disp_stress/Gp_t 

	spp_data_in = data.frame(time_wave,resp_wave)
	colnames(spp_data_in) <- c("time_wave","strain","strain_rate","stress")
	
	spp_params = data.frame(omega,NaN,NaN,NaN,k,num_mode) 
	colnames(spp_params) <- c("frequency",
						   "harmonics",
						   "cycles",
						   "max_harmonics",
						   "step_size",
						   "num_mode")
	
	spp_data_out = data.frame(time_wave,
							  resp_wave,
							  Gp_t,
							  Gpp_t,
							  G_star_t,
							  tan_delta_t,
							  delta_t,
							  disp_stress,
							  eq_strain_est,
							  Gp_t_dot,
							  Gpp_t_dot,
							  G_speed,
							  delta_t_dot)
colnames(spp_data_out) <- c("time_wave",
							"strain",
							"rate",
							"stress",
							"Gp_t",
							"Gpp_t",
							"G_star_t",
							"tan_delta_t",
							"delta_t",
							"disp_stress",
							"eq_strain_est",
							"Gp_t_dot",
							"Gpp_t_dot",
							"G_speed",
							"delta_t_dot")
								
	fsf_data_out = data.frame(Rec_Tvect,
							  Rec_Nvect,
							  Rec_Bvect)
	colnames(fsf_data_out) <- c("Tx","Ty","Tz","Nx","Ny","Nz","Bx","By","Bz")
	
	ft_out <- NaN

mylist <- list("spp_data_in" = spp_data_in, 
				"spp_params"=spp_params, 
				"spp_data_out"=spp_data_out,
				"fsf_data_out"=fsf_data_out,
				"ft_out"=ft_out)
return(mylist)

}
