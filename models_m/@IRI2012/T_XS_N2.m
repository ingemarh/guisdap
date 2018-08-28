function [ T_XS_N2 ] = T_XS_N2( EP )
%T_XS_N2 calculates the N2 total photoionization cross section
%::::::::::::::::::::: T_XS_N2 :::::::::::::::::::::::::::
%.... This function calculates the N2 total photoionization
%.... cross section. P. Richards 2003-10-04
%      REAL FUNCTION T_XS_N2(EP)

%      IMPLICIT NONE
%      REAL EP   !... photon energy
%      REAL ESAVE
%      DATA ESAVE/0.0/

  %.. Wavelength < 20 A, Auger ionization
  if(EP >= 600.0)
    T_XS_N2=0.5E-18;
  %.. Wavelength < 31 A, Auger ionization
  elseif(EP >= 400.0)
    T_XS_N2=1.0E-18;
  %.. Wavelength 31.62 to 23.70 A
  elseif(EP >= 392.0)
    T_XS_N2=exp(7.9864*log(EP)-91.6604);
  %.. Wavelength 225 to 125 A
  elseif(EP >= 55.09)
    T_XS_N2=exp(-2.3711*log(EP)-29.8142);
  %.. Wavelength > 225 A
  else
    T_XS_N2=exp(-1.1077*log(EP)-34.8787);
  end

  %..IF(NINT(10*EP).NE.NINT(10*ESAVE)) WRITE(6,'(2F8.1,1P,2E10.2)') 
  %..> 12394.224/EP,EP, T_XS_N2/(3.39E-17*EXP(-0.0263*EP)), T_XS_N2
  %       ESAVE=EP

  %.. old parameterization
  %..T_XS_N2=3.39E-17*EXP(-0.0263*EP)


end

