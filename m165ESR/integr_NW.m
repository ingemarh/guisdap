% integr_NW: Interface to Nigel Wade's integration package (mex call)
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
% 
% Interface to Nigel Wade's integration package (mex call)
% Parameters:
% a_integdeffile : the integration definition file
% OK  : if true, the integration was succesful
% EOF : if true, the end of file was found during integration
% global parameters:
% d_parbl : the parameter block returned by the integration program
% d_data : the integrated data vector
% d_var1 d_var2: data variances
%
% See also: an_start integr_data integrate
%
% function [OK,EOF]=integr_NW(a_integdeffile)
%
%
% NOTE: This is a copy of the EISCAT interface to NW:s package
% Please check that the call sequence has not changed
% and change this file accordingly
% Then comment the following line

function [OK,EOF]=integr_NW(a_integdeffile)

global d_parbl d_data d_var1 d_var2 a_control a_integdeffile

help integr_NW, error('')

if ~isempty(a_integdeffile)
  [status,d1,N_averaged,par,d_parbl,d_data,d_var1,d_var2,scan]=...
          integrate(['-id ',a_integdeffile]); 
  a_integdeffile=[];
else
  [status,d1,N_averaged,par,d_parbl,d_data,d_var1,d_var2,scan]=integrate;
end 
  

    if status==0,  OK=1; EOF=0;
elseif status>0,   OK=1; EOF=1; 
elseif status<0,   OK=0; EOF=0; fprintf(' Error in integration\n'); return, end

% If no files found, the routine returns empty matrices, exit here
if length(N_averaged)==0, OK=0; return; end

d_data=d_data(:);
d_var1=d_var1(:)-d_data.*d_data/N_averaged;
d_var2=d_var2(:)-d_data.*conj(d_data)/N_averaged;

if a_control(4)==1 & N_averaged<2,
  fprintf(' One file is not enough for variance determination\n')
  fprintf(' Skipping this integration period\n')
  fprintf(' command ''analysis_control(4)=2'' in the startup file will enable the analysis\n')
  OK=0;
  return
elseif a_control(4)==1 & N_averaged<5,
  fprintf(' *******************    WARNING    **********************************\n')
  fprintf(' %.0f files may not be enough for reliable variance determination\n',N_averaged)
  fprintf(' *******************    WARNING    **********************************\n')
end
