if d_rcprog<4.3
  f=(-1152:2:1151)'/(2*1152)*2.5e6;
else
  f=(-1440:3:1439)'/(2*1440)*3e6;
end
k_radar0=2*pi*2*ch_fradar/v_lightspeed;
p_om0=k_radar0*sqrt(2*v_Boltzmann*p_T0/(p_m0(1)*v_amu));
p_om=2*pi*f/p_om0;
