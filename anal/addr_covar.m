% addr_covar.m: Covariance of two result memory addresses.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% A low level routine.
% This matlab version is slow. The routine is available also as a mex-version.
% This routine is called by adgr_covar and adgr_var, through which the use
% of addr_covar is recommended.
% Input parameters: 
% addr1, addr2 : result memory addresses
% vc_signal    : signal strength as calculated by function calc_vcsignal
% lp_XXX       : lag profile parameters
% Output parameters
% covarRe : Covariance between the real parts of addr1 and addr2
% covarIm : Covariance between the imaginary parts of addr1 and addr2
% See also adgr_covar, adgr_var, calc_vcsignal
%function [covarRe,covarIm]=addr_covar(addr1,addr2,vc_signal,lp_vc,lp_dt,lp_ra,...
%         lp_ri,lp_nt,lp_t1,lp_t2,lp_dec,lp_nfir,lp_fir)
function [covarRe,covarIm]=addr_covar(addr1,addr2,vc_signal,lp_vc,lp_dt,lp_ra,...
         lp_ri,lp_nt,lp_t1,lp_t2,lp_dec,lp_nfir,lp_fir)

[len,Nvc]=size(vc_signal);

% Find the lag profiles which contribute to address addr1
lps1=find( lp_ra<=addr1 & addr1<=lp_ra+((lp_nt-1).*lp_ri) ...
            & round((addr1-lp_ra)./lp_ri)==(addr1-lp_ra)./lp_ri );

% Find the lag profiles which contribute to address addr2
lps2=find( lp_ra<=addr2 & addr2<=lp_ra+((lp_nt-1).*lp_ri) ...
            & round((addr2-lp_ra)./lp_ri)==(addr2-lp_ra)./lp_ri );

cov1=0; cov2=0;
for lp1=lps1  % This is a loop over all the lag profiles in lps1
  dt1=lp_dt(lp1); % integer
  for lp2=lps2  % This is a loop over all the lag profiles in lps2
    dt2=lp_dt(lp2); % integer
    if lp_vc(lp1)==lp_vc(lp2) % Products correlate only if virtual channels are equal
      vc=lp_vc(lp1); 

      apu=lp_dec(lp1)*dt1*(addr1-lp_ra(lp1))./lp_ri(lp1);  
      time1=lp_t1(lp1)+apu; % sampling time for the first product in lp1
      time2=lp_t2(lp1)+apu; % sampling time for the second product in lp1
      apu=lp_dec(lp2)*dt2*(addr2-lp_ra(lp2))./lp_ri(lp2);
      tau1=lp_t1(lp2)+apu; % sampling time for the first product in lp2
      tau2=lp_t2(lp2)+apu; % sampling time for the second product in lp2
                                      
      for itime=0:lp_nfir(lp1)-1 % All the filter coeffs in lp1 are treated here
        for itau=0:lp_nfir(lp2)-1 % All the filter coeffs in lp2 are treated here
          ero1=abs(tau1+itau*dt2-(time1+itime*dt1)); % 
      				ero2=abs(time2+itime*dt1-(tau2+itau*dt2));
      				if ero1<len & ero2<len
            cov1=cov1+lp_fir(itime+1,lp1)*lp_fir(itau+1,lp2)*...
                      vc_signal(ero1+1,vc)*vc_signal(ero2+1,vc);
      				end

          ero3=abs(tau2+itau*dt2-(time1+itime*dt1));
      				ero4=abs(time2+itime*dt1-(tau1+itau*dt2));
      				if ero3<len & ero4<len
            cov2=cov2+lp_fir(itime+1,lp1)*lp_fir(itau+1,lp2)*...
                      vc_signal(ero3+1,vc)*vc_signal(ero4+1,vc);
      				end
      		end
      end

    end % End-if of virtual channels
  end  % End-loop over lps2
end  % End-loop over lps2

covarRe=(cov1+cov2)/2;
covarIm=(cov1-cov2)/2;
