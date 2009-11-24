% simul_dump: produces a simulated dump for a model ionosphere
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% IH:include site dep cal temp
% Sorry, if this code is uncomprehensible
%
% The purpose is to produce a complete radar dump for the ionosphere
% defined by ionosperic model in 'ionomodel'
%
% See also: ionomodel dirthe find_om_grid
% 
fprintf('Producing a simulated dump \n')

global p_m0 lpg_Ap

ch=1;
dump=zeros(length(ad_range),1);
for code=diff_val(lpg_code)
  fprintf(' Processing code %.0f\n', code)
  lpgs=find(lpg_code==code & lpg_bcs=='s'); %all signal lpgs belonging to the code
  addrs=lpg_addr(lpgs); %all addresses beloging to the code
  ranges=ad_range(addrs+ADDR_SHIFT);
  [ranges,ind]=sort(ranges);
  while length(ranges)>0
    index=find(ranges-ranges(1)<1);
    addr=addrs(ind(index));
    lpgs=ad_lpg(addr+ADDR_SHIFT);
    param=ionomodel(range_to_height(ranges(1),ch_el(ch)));
    param=real_to_scaled(param);
    coeff=[ad_coeff(addr+ADDR_SHIFT),ad_coeff(addr+ADDR_SHIFT)]';
    kd2=k_radar(ch)^2*p_D0^2;  
    womega=[real(lpg_womscaled(lpgs,:));imag(lpg_womscaled(lpgs,:))];
    [s_womega,s_p_om]=find_om_grid(param,womega,kd2,p_om,pldfvv,0);
    b_womega=[real(lpg_Ap(lpgs));imag(lpg_Ap(lpgs))];
    theo=dirthe(param,coeff,s_womega,kd2,s_p_om,pldfvv,p_m0,b_womega);
    len=length(index);
    dump(addr+ADDR_SHIFT)=complex(theo(1:len),theo(len+1:2*len));
    ranges(index)=[];
    ind(index)=[];
  end
end
lpgs=find(lpg_bcs=='c' & lpg_lag==0);
addr=lpg_addr(lpgs);
dump(addr+ADDR_SHIFT)=a_simul(8)*ones(length(addr),1);
lpgs=find(lpg_lag==0 & lpg_bac~=0);
addr=lpg_addr(lpgs);
dump(addr+ADDR_SHIFT)=dump(addr+ADDR_SHIFT)+a_simul(4)*ones(length(addr),1);

d_data=dump;
d_var1=zeros(size(d_data));
d_var2=zeros(size(d_data));
