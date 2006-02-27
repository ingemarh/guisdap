% subr_backgr.m: function to estimate and subtract the background component in the data
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
%
% uses the stored background lag profile group numbers in 'lpg_bac'
% Tries to include the subraction effect also in the variance estimate
%
% See also: an_start
%function subr_backgr
function subr_backgr

global d_data d_var1 d_var2 lpg_bac lpg_nt lpg_lag lpg_background ADDR_SHIFT

data=d_data;
var1=d_var1;
var2=d_var2;

%*********************** SUBTRACTING BACKGROUND ***************************
bacs=lpg_bac(lpg_bac>0);
bacs=diff_val(bacs);  % find all different values

for bac=bacs,
  addr=lpg_addr(bac)+ADDR_SHIFT; % To change from radar to Matlab addressing
  background=mean(data(addr));
   variance1=mean(var1(addr))/lpg_nt(bac);
   variance2=mean(var2(addr))/lpg_nt(bac);
  for lpg=find(lpg_bac==bac)
    if lpg_lag(lpg)==0 & lpg_nt(lpg)==lpg_nt(bac)
      addr=lpg_addr(lpg)+ADDR_SHIFT; % To change from radar to Matlab addressing
      addb=lpg_addr(bac)+ADDR_SHIFT; % To change from radar to Matlab addressing

      bind=(51:lpg_nt(bac))';
%      A=[(addb(bind)').^2, addb(bind)',ones(size(bind))];
      A=[ addb(bind)',ones(size(bind))];
      aaa=A\d_data(addb(bind));
      index=find(d_data(addb(bind))<A*aaa*1.05);
      index=index+min(bind)-1;
%      A=[(addb(index)').^2, addb(index)',ones(size(index))];
      A=[ addb(index)',ones(size(index))];
      aaa=A\d_data(addb(index));
%      A=[(addb(bind)').^2, addb(bind)',ones(size(bind))];
      A=[addb(bind)',ones(size(bind))];
%      plot(addb,d_data(addb+1),addb(bind),A*aaa)
%	  axis([-inf inf 90 120])
	  backgr=data(addb);
      backgr(bind)=A*aaa;
      var1(bind)=ones(size(bind))*mean(var1(addb(bind)))/length(bind);
      var2(bind)=ones(size(bind))*mean(var2(addb(bind)))/length(bind);
      d_data(addr)=data(addr)-backgr;      
      d_var1(addr)=d_var1(addr)+var1(addb);      
      d_var2(addr)=d_var2(addr)+var2(addb); 
      fprintf('Range dependent background for lpg %.0f\n',lpg)
    else    
      addr=lpg_addr(lpg)+ADDR_SHIFT; % To change from radar to Matlab addressing
      lpg_background(lpg)=background;
      d_data(addr)=data(addr)-background;
      d_var1(addr)=var1(addr)+variance1;
      d_var2(addr)=var2(addr)+variance2;
    end
  end
end
