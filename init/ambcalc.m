% ambcalc.m: Ambiguity function calculations xxx
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
Nvc=length(vc_ch);
lenenv=length(vc_env(:,1));
lenp=length(vc_p(:,1));
vc_Aenv=zeros(lenenv,Nvc);
vc_penv=zeros(lenenv+lenp-1,Nvc);
vc_Apenv=zeros(lenenv+lenp-1,Nvc);
vc_penvabs=zeros(lenenv+lenp-1,Nvc);
vc_Ap=zeros(lenp,Nvc);

fprintf('#\n#\n# Ambcalc: \n')
fprintf('# calculating ACF''s of transmission envelopes\n')
fprintf('# calculating ACF''s of filter impulse responses\n')
fprintf('# calculating effective pulseforms etc\n#\n')

for group=diff_val(vc_group)
  vcs=find(vc_group==group);
  vc=vcs(1);

  fprintf('#Virtual channel group %.0f, formed by channels:', group);
  fprintf(' %.0f',vcs); fprintf('\n')

  ind=1:max(find(vc_env(:,vc)~=0));env=vc_env(ind,vc);
  ind=1:max(find(vc_p(:,vc)~=0));  imp=vc_p(ind,vc);
  Aenv=autocorr(env);
  len=ceil(length(Aenv)/2);
  vc_Aenv(1:len,vcs)=Aenv(len:2*len-1)*ones(size(vcs));
  plot((-(len-1):len-1)*p_dtau,Aenv); title(' ACF of X-mission envelope')
  drawnow

  Ap=autocorr(imp);
  len=ceil(length(Ap)/2);
  vc_Ap(1:len,vcs)=Ap(len:2*len-1)*ones(size(vcs));
%  plot((-(len-1):len-1)*p_dtau,Ap), title(' ACF of filter impulse response')
  drawnow

  penv=conv(imp,env);
  Apenv=autocorr(penv);
  len=length(penv);
  vc_penv(1:len,vcs)=penv*ones(size(vcs));
  vc_Apenv(1:len,vcs)=Apenv(len:2*len-1)*ones(size(vcs));
%  plot((0:len-1)*p_dtau,penv); title(' effective pulse form')
  drawnow

% calculate the same using absolute values of env and p; used later
% for ambiguity function support calculations
  penv=conv(abs(imp),abs(env));
  len=length(penv);
  vc_penvabs(1:len,vcs)=penv*ones(size(vcs));
  vc_penvo(vcs)=vc_envo(vcs)+1;

end
fprintf('#\n#pulseforms calculated\n#\n#\n'),
clear Nvc vc len Apenv Aenv Ap penv env imp ind lenenv lenp group
