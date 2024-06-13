f=(-160:2:159)'/(2*160)*2.5e6;
k_radar0=2*pi*2*ch_fradar/v_lightspeed;
p_om0=k_radar0*sqrt(2*v_Boltzmann*p_T0/(p_m0(1)*v_amu));
p_om=2*pi*f/p_om0;
