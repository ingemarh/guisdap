% half_prof: Main routine for the gated analysis 
% GUISDAP v.1.80 2002-01-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% Main analysis routine, NOT documented yet, sorry
%function half_prof 
function half_prof 

global a_addr a_adstart a_adend a_control ad_lpg ad_coeff ad_range ADDR_SHIFT 
global d_data d_var1 d_var2 d_time p_rep p_dtau ch_el di_fit
global lpg_ND lpg_lag lpg_womscaled lpg_bac lpg_cal lpg_background lpg_Ap
global a_priori a_priorierror p_ND
global r_range r_status r_ind g_ind
global pldfvv p_D0 p_N0 p_T0 p_om k_radar k_radar0 p_m0

r_ind=0;
NP=size(a_priori,2);
physlim=[-ones(1,NP)*1e20;ones(1,NP)*1e20];
nion=length(p_m0);
physlim(:,1:5+nion)=[1e6 1   .01  1   -2e4 -.01*ones(1,nion-1) -100
                    1e14 2e4 100  1e9  2e4 1.01*ones(1,nion-1) 1e4];
physlim=real_to_scaled(physlim);

for g_ind=1:length(a_adstart)
  r_ind=r_ind+1;
  % ADDR_SHIFT is added to result memory addresses 
  % in order to transfer from radar indexing to Matlab indexing
  addr=a_addr(a_adstart(g_ind):a_adend(g_ind)); 
  lpgs=ad_lpg(addr+ADDR_SHIFT); % These are the lag profile groups of the data points

  f_womega=[real(lpg_womscaled(lpgs,:));imag(lpg_womscaled(lpgs,:))];
  fb_womega=[real(lpg_Ap(lpgs));imag(lpg_Ap(lpgs))];
  p_coeffg=[ad_coeff(addr+ADDR_SHIFT),ad_coeff(addr+ADDR_SHIFT)]';

  signal_acf=[real(d_data(addr+ADDR_SHIFT));imag(d_data(addr+ADDR_SHIFT))];  
  measurement=[signal_acf;col(a_priori(g_ind,:))];

  % Take starting point from the a priori model, or from the previous gate
  % (if previous fit was successful and gate above the present one).  
  aa=a_priori(g_ind,:); 
  afree=find(a_priorierror(g_ind,:)>0);
  if r_ind>1
    if mean(ad_range(addr+ADDR_SHIFT))>=r_range(r_ind-1) & status==0
      err=sqrt(diag(ralpha(afree,afree)))';
      if all(result(afree)-err>physlim(1,afree)) & all(result(afree)+err<physlim(2,afree))
        aa(afree)=mean([result(afree);aa(afree)]);
      end
    end
  end

  if a_control(4)==1 % Using variance estimates calculated from data
    signal_var=real([d_var1(addr+ADDR_SHIFT)+d_var2(addr+ADDR_SHIFT);...
                     d_var2(addr+ADDR_SHIFT)-d_var1(addr+ADDR_SHIFT)])/2;  
    diag_var=signal_var;
    variance=[signal_var; (a_priorierror(g_ind,:)').^2];
  elseif a_control(4)>=2 % calculating variance estimates from ambiguity functions
    % The variance scaling is calculated from the integration time here
    % In fact, it should be based on the loop counter value stored to the data dump
    Int_time=diff(tosecs(d_time));
    Var_scale=(p_rep*p_dtau*1e-6/Int_time);
    if Var_scale>1, Var_scale=1; end

    lpgbac=lpg_bac(diff_val(lpg_cal(lpgs)));
    ind=find(lpg_lag(lpgbac)==0);
    Tback=mean(lpg_background(lpgbac(ind)));

    if a_control(4)==2 % variances only
      [covRe,covIm]=adgr_var(addr,Tback,aa);
      ND2=p_ND*(lpg_ND(lpgs).^2);
      signal_var=[covRe./ND2,covIm./ND2]'.*Var_scale;
      diag_var=[covRe./ND2,covIm./ND2]'.*Var_scale;
      variance=[signal_var; (a_priorierror(g_ind,:)').^2];
    elseif a_control(4)==3 % covariances as well
      [covRe,covIm]=adgr_covar(addr,addr,Tback,aa);
      ND2=p_ND*(lpg_ND(lpgs)'*lpg_ND(lpgs));
      signal_var=[covRe./ND2,zeros(size(covRe));...
                  zeros(size(covRe)),covIm./ND2]'.*Var_scale;
      diag_var=diag(signal_var);
      % The variance for zero lag imaginary parts is zero. These must be corrected
      % to some non-zero value in order the program to work.
%     ind=find(diag_var==0); mindiag=min(diag_var(diag_var>0));
%     for ii=ind'; signal_var(ii,ii)=mindiag; end
      [M,N]=size(signal_var);
      variance=[signal_var zeros(M,NP);zeros(NP,N) diag((a_priorierror(g_ind,:)').^2)];
    end
  end
  reind=1:length(addr); % Real parts and diagonal only needed in altitude calculation!
  r_range(r_ind)=sum(ad_range(addr+ADDR_SHIFT)./diag_var(reind)')/sum(1 ./diag_var(reind));

  ch=1;  kd2=k_radar(ch)^2*p_D0^2; Fscale=k_radar0(ch)/k_radar(ch); % hyi hyi
  if any(afree==nion+6)
    small_f_womega=f_womega; small_p_om=p_om;
  else
    [small_f_womega,small_p_om]=find_om_grid(aa,f_womega,kd2,Fscale*p_om,pldfvv);
  end

  errorlim=a_control(1); status=0;
  if errorlim>0 & errorlim<10000 % To prevent unnecessary error estimation
    % Check if the error of Ne larger than given limit when the fit is started
    [error,correl,alpha]=error_estimate(aa,variance,kd2,p_coeffg,small_f_womega,small_p_om,pldfvv,fb_womega);
    if error(1)/aa(1) > errorlim, result=aa; chi2=inf; status=2; end % No fit done
  end
  if status==0 % Now proceed to the fitting routine
    tol=a_control(2); maxiter=a_control(3);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if a_control(4)<=2
      [result,chi2,iter,alpha]=mrqmndiag(aa,measurement,variance,tol,maxiter,...
              kd2,p_coeffg,small_f_womega,small_p_om,pldfvv,p_m0,physlim,fb_womega);
    else
      [result,chi2,iter,alpha]=mrqmn(aa,measurement,variance,tol,maxiter,...
              kd2,p_coeffg,small_f_womega,small_p_om,pldfvv,fb_womega);
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    status=(iter>=maxiter);
    if status==0 & (any(result(afree)-eps<=physlim(1,afree)) | any(result(afree)+eps>=physlim(2,afree)))
      status=3;
    end
  end
  % Forward only the variances (not covariances) to store_results
  if min(size(variance))>1; variance=diag(variance); end
  ralpha=real(alpha);
  store_results(aa,measurement,variance,real(result),ralpha,chi2,status,...
                kd2,p_coeffg,small_f_womega,small_p_om,pldfvv,fb_womega,lpgs);
end
