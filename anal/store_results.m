% store_results: Internal routine to print and plot analysis results after each gate
% GUISDAP v.1.80 2002 Copyright EISCAT, Huuskonen&Lehtinen
%
% See also: half_prof
% function store_results(aa,meas,var,result,alpha,chi2,status,kd2,p_coeffg,f_womega,p_om,pldfvv)
function store_results(aa,meas,var,result,alpha,chi2,status,kd2,p_coeffg,f_womega,p_om,pldfvv)
 
global a_priori a_priorierror p_RECloc
global ch_el di_fit g_ind
global lpg_dt p_dtau di_figures di_results
global r_range r_status r_param r_dp r_error r_res r_apriori r_apriorierror r_ind p_m0 r_h r_Offsetppd
global name_site

% Scale residual and Xfer results to physical units

len=length(meas)-length(aa);
lr=length(result);
nmeas=length(find(var(1:len)~=0));
chi2=chi2/nmeas;
err=covm2vec(alpha);
res=scaled_to_real(result);
er=err;er(1:lr)=scaled_to_real(err(1:lr));
height=range_to_height(r_range(r_ind),ch_el(1));
%if status==3 & ~any(physlim(result-err(1:lr),p_m0)) & ~any(physlim(result+err(1:lr),p_m0))
%  status=0;
%end
if p_m0(1)==16, comp=1-sum(result(6:end));
else comp=result(find(p_m0==16)+4);
end
if isempty(comp), comp=0; end

if di_results | isempty(r_h)
 % Print results to the console
 if rem(r_ind,20)==1
   fprintf(' alt   Ne/1e11     Ti      Te/Ti    coll/1e3    vel   [O+] resid status\n')
 end
 fprintf('%5.1f',height)
 fprintf(' %4.2f:%4.2f',res(1)/1e11,er(1)/1e11)
 fprintf(' %4.0f:%3.0f',res(2),er(2))
 fprintf(' %4.2f:%4.2f',res(3),er(3))
 fprintf(' %4.1f:%4.1f',res(4)/1e3,er(4)/1e3)
 fprintf(' %4.0f:%3.0f',res(5),er(5))
 fprintf(' %4.2f %5.2f',comp,chi2)
 if status==0, str='OK';
 elseif status==1, str='Max iter';
 elseif status==2, str='No fit';
%elseif status==3, str='Nonphys';
 elseif status==3, str='Fail';
 end
%fprintf(' %d',len)
 fprintf(' %3s\n',str)
end

% Store results to result variables
r_param(r_ind,:)=res;
r_dp(r_ind,:)=comp;
r_error(r_ind,:)=er;
r_res(r_ind,:)=[chi2,sqrt(2/nmeas)];
r_status(r_ind,:)=status;
r_apriori(r_ind,:)=scaled_to_real(a_priori(g_ind,:));
r_apriorierror(r_ind,:)=scaled_to_real(a_priorierror(g_ind,:));

if di_figures(3) | name_site=='K' | name_site=='S'
  theo=dirthe(result,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0);
  indr=1:len/2;
  sig_err=sqrt(var);
  sig_r=meas(indr); err_r=sig_err(indr); fitted_r=theo(indr);
  if name_site=='K' | name_site=='S'
    [ii,jj]=max(conv(sig_r,flipud(fitted_r)));
    iii=conv(err_r,abs(flipud(fitted_r)));
    if ii/iii(jj)>1 & ~status
      r_Offsetppd(r_ind)=(len/2-jj)*mean(lpg_dt)*p_dtau;
    else
      r_Offsetppd(r_ind)=NaN;
    end
    fprintf('Offset: %d us  ',r_Offsetppd(r_ind))
  end

  if di_figures(3)
    indi=len/2+indr;
    indp=len+find(er(1:lr));
    sig_i=meas(indi);err_i=sig_err(indi);
    sig_p=meas(indp);err_p=sig_err(indp);
    fitted_i=theo(indi);
    indi(find(var(indi)==0))=NaN; indi=indi-len/2;
    indp=indp-len;
    fitted_p=result(indp)';
    res_err=err(indp)';
 
    drawnow, figure(di_figures(3)),%clf
 
    subplot('Position',[.1 .1 .7 .8])
    indr=indr-.05; indi=indi+.05;
    plot(indr,sig_r,'ro',indr,fitted_r,'g-',...
         indi,sig_i,'bo',indi,fitted_i,'g-',...
         [indr;indr],[sig_r-err_r,sig_r+err_r]','r-',...
         [indi;indi],[sig_i-err_i,sig_i+err_i]','b-')
    set(get(gca,'Children'),'MarkerSize',4)
    if name_site=='K' | name_site=='S'
      [ii,jj]=max(conv(sig_r,flipud(fitted_r)));
      title(['Data (o) and fit (-) results  Offset: ',num2str(r_Offsetppd) '\mus'])
    else
      title('Data (o) and fit results (solid line)')
    end
    ylabel('Power [K]'); xlabel('# of data point')
    set(gca,'ylim',min([max([get(gca,'ylim');-1000 -1000]);10000 10000]));
 
    subplot('Position',[.85 .1 .12 .8])
    plot(indp-0.15,sig_p,'ro',indp+0.15,fitted_p,'go',...
        [indp;indp]-0.15,[sig_p-err_p,sig_p+err_p]','r:',...
        [indp;indp]+0.15,[fitted_p-res_err,fitted_p+res_err]','g-')
    set(get(gca,'Children'),'MarkerSize',4)
    axlim=[0.5 max(indp)+.5 -1 max([5,ceil(fitted_p')])];
    if all(~isnan(axlim))
      axis(axlim);
    end
    XTickLabel=['N';'T';'r';'c';'v';'p';'p'];
    set(gca,'Xtick',indp,'XTickLabel',XTickLabel(indp))
    title('parameters')
    drawnow
  end
end
