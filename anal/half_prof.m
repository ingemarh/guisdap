% half_prof: Main routine for the gated analysis 
% GUISDAP v.1.80 2002-01-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% Main analysis routine, NOT documented yet, sorry
%function half_prof 
function half_prof 

global a_addr a_adstart a_adend a_control ad_lpg ad_coeff ad_range ad_w ADDR_SHIFT 
global d_data d_var1 d_var2 d_time p_rep p_dtau ch_el di_fit
global lpg_ND lpg_lag lpg_womscaled lpg_bac lpg_cal lpg_background lpg_Ap
global a_priori a_priorierror p_ND a_gating
global r_range r_status r_w
global pldfvv p_D0 p_N0 p_T0 p_om k_radar k_radar0 p_m0 fit_altitude

r_ind=0;
nion=length(p_m0);
physlim=real_to_scaled(fit_altitude(:,7:8)');

for g_ind=1:length(a_adstart)
  r_ind=r_ind+1;
  % ADDR_SHIFT is added to result memory addresses 
  % in order to transfer from radar indexing to Matlab indexing
  addr=a_addr(a_adstart(g_ind):a_adend(g_ind));
  lpgs=ad_lpg(addr+ADDR_SHIFT); % These are the lag profile groups of the data points

  aa=a_priori(g_ind,:); 
  a_priorivar=a_priorierror(g_ind,:)'.^2;
  p_coeffg=[ad_coeff(addr+ADDR_SHIFT),ad_coeff(addr+ADDR_SHIFT)]';
  signal_acf=[real(d_data(addr+ADDR_SHIFT));imag(d_data(addr+ADDR_SHIFT))];
  measurement=[signal_acf;col(aa)];

  % Take starting point from the a priori model, or from the previous gate
  % (if previous fit was successful and gate above the present one).  
  afree=find(a_priorivar>0);
  if r_ind>1
    if mean(ad_range(addr+ADDR_SHIFT))>=r_range(r_ind-1) & status==0
      err=sqrt(diag(ralpha(afree,afree)))';
      if all(result(afree)-err>physlim(1,afree)) & all(result(afree)+err<physlim(2,afree))
        aa(afree)=mean([result(afree);aa(afree)]);
      end
    end
  end

  if a_control(4)==1, %Using variance estimates from data
    diag_var=real([d_var1(addr+ADDR_SHIFT)+d_var2(addr+ADDR_SHIFT);...
                   d_var2(addr+ADDR_SHIFT)-d_var1(addr+ADDR_SHIFT)])/2;  
    variance=[diag_var;a_priorivar];
  elseif a_control(4)>=2, %variance estimates from ambiguity functions
    % The variance scaling is calculated from the integration time here
    % it should be based on the loop counter value stored to the data dump
    Var_scale=p_ND/min(1,p_rep*p_dtau*1e-6/diff(tosecs(d_time)));
    lpgbac=lpg_bac(diff_val(lpg_cal(lpgs)));
    ind=find(lpg_lag(lpgbac)==0);
    Tback=mean(lpg_background(lpgbac(ind)));
    if a_control(4)==2, %variances only
      [covRe,covIm]=adgr_var(addr,Tback,aa);
      ND2=Var_scale*lpg_ND(lpgs).^2;
      diag_var=[covRe./ND2 covIm./ND2]';
      variance=[diag_var;a_priorivar];
    elseif a_control(4)==3, %covariances as well
      [covRe,covIm]=adgr_covar(Tback,aa,addr);
      ND2=Var_scale*(lpg_ND(lpgs)'*lpg_ND(lpgs));
      signal_var=blkdiag(covRe./ND2,covIm./ND2);
      diag_var=diag(signal_var);
      variance=blkdiag(signal_var,diag(a_priorivar));
    end
  end
  lpg=unique(lpgs); nlpg=length(lpg); nlpgs=length(lpgs);
  if a_gating & nlpg<nlpgs & any(size(variance)==1)
    M=zeros(nlpg*2,1); V=M; P=M;
    for i=1:nlpg
      d=find(lpgs==lpg(i));
      pc=p_coeffg(d);
      for j=(0:1)*nlpg+i
        mv=variance(d)./pc;
        if any(mv) & a_gating>1
          mv=sum(1./mv)./mv;
          M(j)=sum(measurement(d).*mv);
	  V(j)=sum(variance(d).*mv.^2);
          P(j)=sum(pc.*mv);
        else
          M(j)=sum(measurement(d)); P(j)=sum(pc); V(j)=sum(variance(d));
        end
        d=d+nlpgs;
      end
    end
    measurement=[M;col(a_priori(g_ind,:))]; variance=[V;a_priorivar];
    p_coeffg=P; lpgs=lpg;
  end
  f_womega=[real(lpg_womscaled(lpgs,:));imag(lpg_womscaled(lpgs,:))];
  fb_womega=[real(lpg_Ap(lpgs));imag(lpg_Ap(lpgs))];

  reind=1:length(addr); %Only real parts and diagonal needed in altitude calculation!
  r_range(r_ind)=sum(ad_range(addr+ADDR_SHIFT)./diag_var(reind)')/sum(1 ./diag_var(reind));
  r12=([1;1]*ad_range(addr+ADDR_SHIFT)+[-1;1]*ad_w(addr+ADDR_SHIFT)/2);
  mr12=[min(r12(1,:));max(r12(2,:))];
  r12=1+round(r12-mr12(1));
  rangest=max(r12(2,:));
  rangesw=zeros(1,rangest);
  for i=reind
    rangesw(r12(1,i):r12(2,i))=rangesw(r12(1,i):r12(2,i))+1/diag_var(i);
  end
  rangest=1:rangest;
  rangest=(rangest-sum(rangesw.*rangest)/sum(rangesw));
  r_w(r_ind,:)=[4*sum(abs(rangesw.*rangest))/sum(abs(rangesw)) ...
                2*sqrt(sum(rangesw.*rangest.^2)/sum(abs(rangesw))) ...
                diff(mr12)]; % 3 range resolution estimates...

  ch=1; kd2=k_radar(ch)^2*p_D0^2; Fscale=k_radar0(ch)/k_radar(ch); % hyi hyi
  [small_f_womega,small_p_om]=find_om_grid(aa,f_womega,kd2,Fscale*p_om,pldfvv,any(afree==nion+6));

  errorlim=a_control(1); status=0;
  if errorlim>0 % Check if the error of Ne larger than given limit when the fit is started
    [error,correl,alpha]=error_estimate(aa,variance,kd2,p_coeffg,small_f_womega,small_p_om,pldfvv,fb_womega,fit_altitude(:,6));
    if error(1)/aa(1)>errorlim, result=aa; chi2=Inf; status=2; end % No fit done
  end
  if status==0 % Now proceed to the fitting routine
    tol=a_control(2); maxiter=a_control(3);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    MRQMN='mrqmndiag';
    if a_control(4)>2 | exist(MRQMN)~=3, MRQMN='mrqmn'; end
    [result,chi2,iter,alpha]=feval(MRQMN,aa,measurement,variance,tol,maxiter,...
     kd2,p_coeffg,small_f_womega,small_p_om,pldfvv,p_m0,physlim,fb_womega,...
     fit_altitude(:,6));
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    status=(iter>=maxiter);
    if status==0 & (any(result(afree)-eps<=physlim(1,afree)) | any(result(afree)+eps>=physlim(2,afree)))
      status=3;
    end
  end
  % Forward only the variances (not covariances) to store_results
  if all(size(variance)>1), variance=diag(variance); end
  ralpha=real(alpha);
  store_results(aa,measurement,variance,real(result),ralpha,chi2,status,kd2,...
          p_coeffg,small_f_womega,small_p_om,pldfvv,fb_womega,lpgs,r_ind,g_ind);
end
