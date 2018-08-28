function [ T_XS_OX ] = T_XS_OX( EP )
%T_XS_OX calculates the OX total photoionization cross section
%::::::::::::::::::::: T_XS_OX :::::::::::::::::::::::::::
%.... This function calculates the OX total photoionization
%.... cross section. P. Richards 2003-10-04
%.... Samson and Pareek Phys. Rev. A, 31, 1470, 1985
%
%      REAL FUNCTION T_XS_OX(EP)

%      IMPLICIT NONE
%      REAL EP   !... photon energy
%      REAL ESAVE
  %persistent ESAVE;
  %if isempty(ESAVE)
  %  ESAVE = 0.0;
  %end

  %.. NEW parameterization
  if(EP >= 500.0)
    %.. Wavelength shorter than 25 A, Auger ionization
    T_XS_OX=0.5E-18;
  elseif(EP >= 165.26)
    %.. Wavelength shorter than 75 A
    T_XS_OX=exp(-2.5209*log(EP)-28.8855);
  elseif(EP >= 55.09)
    %.. Wavelength between 78 and 256.26 A
    T_XS_OX=exp(-1.7871*log(EP)-32.6335);
  else
    %.. Wavelength longer than 256.26 A
    T_XS_OX=exp(-1.3077*log(EP)-34.5556);
  end

  %..IF(NINT(10*EP).NE.NINT(10*ESAVE)) WRITE(6,'(2F8.1,1P,2E10.2)') 
  %..> 12394.224/EP,EP, T_XS_OX/(27.2E-18*EXP(-3.09E-2*EP)), T_XS_OX
   %ESAVE=EP;

  %.. old parameterization
  %.. T_XS_OX=27.2E-18*EXP(-3.09E-2*EP)

end

