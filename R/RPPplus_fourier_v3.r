#' SPP Analysis via fourier
#'
#' @description  applies the SPP Analysis by means of a fourier series.
#' 
#' @param time_wave Lx1 vector of time at each measurement point
#' @param resp_wave Lx3 matrix of the strain, rate and stress data,with each row representing a measuring point
#' @param L  number of measurement points in the extracted data
#' @param omega frequency of oscilation (rad/s)
#' @param M number of harmonics for stress
#' @param p number of cycles
#'
#' @return a list with the following data frame
#'         spp_data_in= the data frame with the data
#'				 spp_params=spp_params, 
#'				 spp_data_out= Length,frequency,harmonics,cycles,max_harmonics,step_size
#'				 fsf_data_out= Tx,Ty,Tz,Nx,Ny,Nz,Bx,By,Bz coordinates of the trajectory (T=tangent,N=principal Normal,B=Binormal Vectors)
#'				 ft_out=data frame with that includes time_wave,strain,rate,stress,Gp_t,Gpp_t,G_star_t,tan_delta_t,delta_t,disp_stress,eq_strain_est,Gp_t_dot,Gpp_t_dot,G_speed,delta_t_dot)
#'
#' @author  Simon Rogers Group for Soft Matter (matlab version), Giorgio Luciano and Serena Berretta (R version)
#' @keywords SPP fourier
#' @examples data(mydata)
#' df <- rpp_read2(mydata , selected=c(2, 3, 4, 0, 0, 1, 0, 0))
#' time_wave <- df$raw_time
#' resp_wave <- data.frame(df$strain,df$strain_rate,df$stress) 
#' out <- rpp_fft(time_wave,resp_wave,L=1024,omega=3.16 , M=15,p=1)
#' @export 
#' @import fftwtools pracma spectral
#' 
#' @references Simon A. Rogersa, M. Paul Letting, A sequence of physical processes determined and quantified in large-amplitude oscillatory shear (LAOS): Application to theoretical nonlinear models
#' Journal of Rheology 56:1, 1-25



rpp_fft<- function(time_wave,resp_wave,L,omega,M,p){

time_wave_o <- time_wave

W = round(L/(2*p))

#fft_resp = mvfft(as.matrix(resp_wave))
fft_resp = mvfftw(as.matrix(resp_wave))
sign_fft_resp = fft_resp/L
f_domain = linspace(0,W,W+1)
FWave = fft_resp[seq(1,L/2),]


k = rep(0,(W+1))
for (n in seq(1:W+1))
	{
    k[n]=p*(n-1)+1
	}

#Normalize size of fourier harmonics
sign_fft_resp_s = sign_fft_resp[,3]
ft_amp_sign_fft_resp_s=2*abs(sign_fft_resp_s[k])
ft_amp_sign_fft_resp_s=ft_amp_sign_fft_resp_s/ft_amp_sign_fft_resp_s[2]

#separate real and imaginary components of the fourier response
FWreal = Re(FWave)
FWimag = Im(FWave)

#Calculate Fourier harmonics of strain, rate, and stress
An1_n=2*FWreal[,1]/L
An1_n[1]=0 
Bn1_n=-2*FWimag[,1]/L
An1_r=2*FWreal[,2]/L
An1_r[1]=0 
Bn1_r=-2*FWimag[,2]/L
An1=2*FWreal[,3]/L
An1[1]=0 
Bn1=-2*FWimag[,3]/L

Delta=atan(An1_n[p+1]/Bn1_n[p+1])
if (Bn1_n[p+1]<0)
	{
    Delta=Delta+pi
	}


dt=2*pi/omega/L
time_wave_new=dt*(seq(0,(L-1)))
time_wave=time_wave+Delta/omega

#Compensate for ofset in harmonics

J=length(An1_n)
An_n = rep(0,J)
Bn_n = rep(0,J)
An_r = rep(0,J)
Bn_r = rep(0,J)
An = rep(0,J)
Bn = rep(0,J)

J=length(An1_n)
for (nn in seq(1,(J-1)))
	{
    An_n[nn+1]=An1_n[nn+1]*cos(Delta/(p)*nn)-Bn1_n[nn+1]*sin(Delta/(p)*nn)
    Bn_n[nn+1]=Bn1_n[nn+1]*cos(Delta/(p)*nn)+An1_n[nn+1]*sin(Delta/(p)*nn)
	}

J=length(An1_r)
for ( nn in seq (1,(J-1) ) ) 
	{
    An_r[nn+1]=An1_r[nn+1]*cos(Delta/(p)*nn)-Bn1_r[nn+1]*sin(Delta/(p)*nn)
    Bn_r[nn+1]=Bn1_r[nn+1]*cos(Delta/(p)*nn)+An1_r[nn+1]*sin(Delta/(p)*nn)
	}
	
J=length(An1)
for (nn in seq (1, (J-1) ) )
	{
    An[nn+1]=An1[nn+1]*cos(Delta/(p)*nn)-Bn1[nn+1]*sin(Delta/(p)*nn)
    Bn[nn+1]=Bn1[nn+1]*cos(Delta/(p)*nn)+An1[nn+1]*sin(Delta/(p)*nn)
	}

	
Recon_Wave = data.frame(ones(L,1)*An_n[1],ones(L,1)*An_r[1],ones(L,1)*An[1])
Recon_Wave_dot = data.frame(zeros(L,3))
Recon_Wave_ddot = data.frame(zeros(L,3))
Recon_Wave_dddot = data.frame(zeros(L,3))

#Find fourier series for strain

for (n in seq(1,1,2))
	{##
    Recon_Wave[,1] = Recon_Wave[,1] + An_n[p*n+1]*cos(n*omega*time_wave_new) + Bn_n[p*n+1]*sin(n*omega*time_wave_new)
    Recon_Wave_dot[,1] = Recon_Wave_dot[,1] - n*omega*An_n[p*n+1]*sin(n*omega*time_wave_new) + n*omega*Bn_n[p*n+1]*cos(n*omega*time_wave_new)
    Recon_Wave_ddot[,1] = Recon_Wave_ddot[,1] - n^2*omega^2*An_n[p*n+1]*cos(n*omega*time_wave_new) - n^2*omega^2*Bn_n[p*n+1]*sin(n*omega*time_wave_new)
    Recon_Wave_dddot[,1] = Recon_Wave_dddot[,1] + n^3*omega^3*An_n[p*n+1]*cos(n*omega*time_wave_new) - n^3*omega^3*Bn_n[p*n+1]*sin(n*omega*time_wave_new)
	}

#Find fourier series for rate
for (n in seq(1,1,2) )
	{
    Recon_Wave[,2] = Recon_Wave[,2] + An_r[p*n+1]*cos(n*omega*time_wave_new)+ Bn_r[p*n+1]*sin(n*omega*time_wave_new)
    Recon_Wave_dot[,2] = Recon_Wave_dot[,2] - n*omega*An_r[p*n+1]*sin(n*omega*time_wave_new) + n*omega*Bn_r[p*n+1]*cos(n*omega*time_wave_new)
    Recon_Wave_ddot[,2] = Recon_Wave_ddot[,2] - n^2*omega^2*An_r[p*n+1]*cos(n*omega*time_wave_new) - n^2*omega^2*Bn_r[p*n+1]*sin(n*omega*time_wave_new)
    Recon_Wave_dddot[,2] = Recon_Wave_dddot[,2] + n^3*omega^3*An_r[p*n+1]*cos(n*omega*time_wave_new) - n^3*omega^3*Bn_r[p*n+1]*sin(n*omega*time_wave_new)
	}

#Find fourier series for results
for (n in seq(1,M,2))
	{
    #Stress
    Recon_Wave[,3] = Recon_Wave[,3] + An[p*n+1]*cos(n*omega*time_wave_new)+ Bn[p*n+1]*sin(n*omega*time_wave_new)
    Recon_Wave_dot[,3] = Recon_Wave_dot[,3] - n*omega*An[p*n+1]*sin(n*omega*time_wave_new) + n*omega*Bn[p*n+1]*cos(n*omega*time_wave_new)
    Recon_Wave_ddot[,3] = Recon_Wave_ddot[,3] - n^2*omega^2*An[p*n+1]*cos(n*omega*time_wave_new) - n^2*omega^2*Bn[p*n+1]*sin(n*omega*time_wave_new)
    Recon_Wave_dddot[,3] = Recon_Wave_dddot[,3] + n^3*omega^3*An[p*n+1]*sin(n*omega*time_wave_new) - n^3*omega^3*Bn[p*n+1]*cos(n*omega*time_wave_new)
	}

rd = Recon_Wave_dot
rdd = Recon_Wave_ddot
rddd = Recon_Wave_dddot

rd_tn = rd/omega
rdd_tn = rdd/omega^2
rddd_tn = rddd/omega^3

rd_x_rdd = data.frame(rd[,2]*rdd[,3]-rd[,3]*rdd[,2],
					  rd[,3]*rdd[,1]-rd[,1]*rdd[,3],
					  rd[,1]*rdd[,2]-rd[,2]*rdd[,1])
rd_x_rd_x_rdd = data.frame(rd[,2]*rd_x_rdd[,3]-rd[,3]*rd_x_rdd[,2],
						   rd[,3]*rd_x_rdd[,1]-rd[,1]*rd_x_rdd[,3],
						   rd[,1]*rd_x_rdd[,2]-rd[,2]*rd_x_rdd[,1])

mag_rd = sqrt(rd[,1]^2+rd[,2]^2+rd[,3]^2);

Rec_Tvect = rd

mag_rd_x_rdd = sqrt(rd_x_rdd[,1]^2+rd_x_rdd[,2]^2+rd_x_rdd[,3]^2)

Rec_Nvect = -rd_x_rd_x_rdd/(mag_rd*mag_rd_x_rdd)

Rec_Bvect = rd_x_rdd/mag_rd_x_rdd

Gp_t = -rd_x_rdd[,1]/rd_x_rdd[,3]
Gpp_t = -rd_x_rdd[,2]/rd_x_rdd[,3]

Gp_t_dot = -rd[,2]*((rddd[,1]*rd_x_rdd[,1])+(rddd[,2]*rd_x_rdd[,2])+(rddd[,3]*rd_x_rdd[,3]))/((rd_x_rdd[,3])^2)
Gpp_t_dot = rd[,1]*((rddd[,1]*rd_x_rdd[,1])+(rddd[,2]*rd_x_rdd[,2])+(rddd[,3]*rd_x_rdd[,3]))/((rd_x_rdd[,3])^2)


G_speed = sqrt(Gp_t_dot^2+Gpp_t_dot^2)

G_star_t = sqrt(Gp_t^2+Gpp_t^2)
tan_delta_t = Gpp_t/Gp_t

#-----
is_Gp_t_neg = (Gp_t<0)
delta_t = atan(tan_delta_t)+pi*is_Gp_t_neg
delta_t_dot = -1*(rd_tn[,3]*((rddd_tn[,3] + 
    rd_tn[,3]))/((rdd_tn[,3])^2+(rd_tn[,3])^2))

disp_stress = Recon_Wave[,3]-(Gp_t*Recon_Wave[,1]+Gpp_t*Recon_Wave[,2]/omega)
eq_strain_est = Recon_Wave[,1]-disp_stress/Gp_t

spp_data_in = data.frame(time_wave,resp_wave)
colnames(spp_data_in) <- c("time_wave","strain","strain_rate","stress")
spp_params = data.frame(omega,M,p,W,NaN,NaN)
colnames(spp_params) <- c("frequency",
						   "harmonics",
						   "cycles",
						   "max_harmonics",
						   "step_size",
						   "num_mode")
						   
spp_data_out = data.frame(time_wave_new,
						Recon_Wave,
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
							
fsf_data_out = data.frame(Rec_Tvect,Rec_Nvect,Rec_Bvect)
colnames(fsf_data_out) <- c("Tx","Ty","Tz","Nx","Ny","Nz","Bx","By","Bz")
							
							
ft_out = data.frame(f_domain,c(0,ft_amp_sign_fft_resp_s))
colnames(ft_out) <- c("ft_amp","fft_resp")

mylist <- list("spp_data_in" = spp_data_in, 
				"spp_params"=spp_params, 
				"spp_data_out"=spp_data_out,
				"fsf_data_out"=fsf_data_out,
				"ft_out"=ft_out
				)
return(mylist)
}
