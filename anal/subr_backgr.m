% subr_backgr.m:  estimates and subtracts the background component in the data
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% uses the stored background lag profile group numbers in 'lpg_bac'
% Tries to include the subraction effect also in the variance estimate
%
% See also: an_start
%function subr_backgr
function subr_backgr

global d_data d_var1 d_var2 lpg_bac lpg_nt lpg_background ADDR_SHIFT a_code lpg_bcs lpg_code

data=d_data;
var1=d_var1;
var2=d_var2;

%*********************** SUBTRACTING BACKGROUND ***************************
lpgs=find(lpg_bcs);
if ~isempty(a_code)
  lpgs=lpgs(find(ismember(lpg_code(lpgs),unique(a_code))));
end

%bacs=lpg_bac(lpg_bac(lpgs)>0);
bacs=lpg_bac(lpgs(find(lpg_bac(lpgs)>0)));
bacs=diff_val(bacs);  % find all different values

for bac=bacs
  addr=lpg_addr(bac)+ADDR_SHIFT; % To change from radar to Matlab addressing
  background=meddan(data(addr));
  variance1=mean(var1(addr))/lpg_nt(bac);
  variance2=mean(var2(addr))/lpg_nt(bac);
  for lpg=find(lpg_bac==bac)
    addr=lpg_addr(lpg)+ADDR_SHIFT; % To change from radar to Matlab addressing
    lpg_background(lpg)=background;
    d_data(addr)=data(addr)-background;
    d_var1(addr)=var1(addr)+variance1;
    d_var2(addr)=var2(addr)+variance2;
  end
end

function m=meddan(x)
m=median(real(x))+i*median(imag(x));
