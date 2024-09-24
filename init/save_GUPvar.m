% save_GUPvar: Script to save GUP variables to a file
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% See also: design path_expr

GUP_iniver=str2num(strtok(GUP_ver));

if max(lp_nfir)==1 | isa(lp_fir,'int8')
  lp_firsto=lp_fir;
elseif all(all(ismember(lp_fir,[-1 0 1])))
  lp_firsto=int8(lp_fir);
else
  lp_firsto=sparse(diff([zeros(size(lp_vc));lp_fir]));
end
clear lp_fir

if ~exist('apustr'), apustr=''; end
GUPvarfile=[path_expr name_expr name_site apustr 'GUPvar.mat'];

%lp_vc=int32(lp_vc); lp_dt=int32(lp_dt); lp_ra=int32(lp_ra); lp_ri=int32(lp_ri);
%lp_nt=int32(lp_nt); lp_t1=int32(lp_t1); lp_t2=int32(lp_t1); lp_dec=int32(lp_dec);
%lp_nfir=int32(lp_nfir); lpb_bcs=int32(lp_bcs); lp_code=int32(lp_code);
%vc_ch=int32(vc_ch);

save(GUPvarfile,'GUP_iniver','ch_fradar','ch_gain','p_dtau','p_rep','p_ND',...
 'p_XMITloc','p_RECloc','vc_ch','vc_env','vc_envo','vc_p','vc_adcint',...
 'vc_sampling','lp_dec','lp_firsto','lp_nfir','lp_T','lp_bcs','lp_code',...
 'lp_dt','lp_h','lp_nt','lp_ra','lp_ri','lp_t1','lp_t2','lp_vc','-V7.3')

if isempty(local.fn.gzip)
  gzip([GUPvarfile '.mat']);
else
  gupsystem([local.fn.gzip ' -f9 ' GUPvarfile '.mat']);
end
disp([GUPvarfile ' saved']);
