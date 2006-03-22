% COR_init.m: initializes the lag profile variables to a sufficient size
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Input parameters:
% size: length of the matrices (optional, 5000 assumed)
% nfir: maximum filter coefficient length (optional, 1 assumed)
%
% See also: design, COR_end
%
%function COR_init(nlp,nfir)
function COR_init(nlp,nfir)

global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt 
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind 
global ra_next ra_prev bm_next vc_next vc_routine local

lp_ind=0;
ra_next=0;
ra_prev=0;
bm_next=1;
vc_next=1;
vc_routine=cellstr(' ');

if nargin<1, nlp=5000; end
if nargin<2, nfir=1; end
lp_t1=zeros(1,nlp);
lp_t2=zeros(1,nlp);
lp_h=zeros(1,nlp);
lp_ra=zeros(1,nlp);
lp_nfir=zeros(1,nlp);
try
 lp_fir=zeros(nfir,nlp);
catch
 if local.matlabversion>=14
  lp_fir=zeros(nfir,nlp,'int8');
 else
  lp_fir=repmat(int8(zeros(1,nlp)),nfir,1);
 end
 fprintf('Warning: Too large matrix -- trying with integer fir coeffs\n')
end
lp_dec=zeros(1,nlp);
lp_T=zeros(1,nlp);
lp_dt=zeros(1,nlp);
lp_nt=zeros(1,nlp);
lp_vc=zeros(1,nlp);
lp_ri=zeros(1,nlp);
lp_bcs=zeros(1,nlp);
lp_code=zeros(1,nlp);
