function forward_spec(data,var,lpgs,f_womega)
global lpg_lag p_dtau r_spec r_freq r_lag r_acf r_ace di_spectra
weight=sum(abs(f_womega),2);
nd=length(weight);
weight(nd/2+1:end)=weight(1:nd/2);
wt=var(1:nd)./weight;
lags=round(lpg_lag(lpgs));
lag=sort(unique(lags)); acf=zeros(size(lag)); err=acf;
for l=1:length(lag)
 ad=find(lags==lag(l));
 adi=ad+nd/2;
 acf(l)=sum(data(ad)./weight(ad)./wt(ad))/sum(1../wt(ad));
 err(l)=sum(1../weight(ad)./wt(ad))/sum(1../wt(ad));
 if wt(adi)~=0
  acf(l)=acf(l)+j*sum(data(adi)./weight(adi)./wt(adi))/sum(1../wt(adi));
 end
end
%[err(1:5)/mean(err) std(err)/mean(err)]
%keyboard
if di_spectra==-1
 if isempty(r_acf)
  r_acf=acf';
  r_ace=err';
  r_lag=lag'*p_dtau*1e-6;
 else
  lrf=size(r_lag,1); lf=length(lag);
  if lrf<lf
   r_lag(end+1:lf,:)=NaN;
   r_acf(end+1:lf,:)=NaN;
   r_ace(end+1:lf,:)=NaN;
  elseif lrf>lf
   lag(end+1:lrf)=NaN;
   acf(end+1:lrf)=NaN;
   err(end+1:lrf)=NaN;
  end
  r_lag(:,end+1)=lag'*p_dtau*1e-6; r_acf(:,end+1)=acf'; r_ace(:,end+1)=err';
 end
else
 dt=median(diff(lag));
 l=find(err>mean(err)+3*abs(di_spectra)*std(err)); acf(l)=[]; lag(l)=[];
 if di_spectra>0
  ml=max(lag); nl=round(ml/dt); l=(0:nl)/nl*ml; fl=fliplr(find(lag));
  aend=0; if imag(acf(end))==0, aend=1; end
  acf=interp1([-lag(fl) lag],[conj(acf(fl)) acf],l,'spline');
  if aend, acf(end)=real(acf(end)); end
  [s,f]=acf2spec(acf.',l*p_dtau*1e-6);
 else
  err(l)=[];
  [s,f]=acf2spec_wlsq(acf,err,lag*p_dtau*1e-6);
 end
 if isempty(r_freq)
  r_freq=f'; r_spec=s;
 else
  lrf=size(r_freq,1); lf=length(f);
  if lrf<lf
   r_freq(end+1:lf,:)=NaN;
   r_spec(end+1:lf,:)=NaN;
  elseif lrf>lf
   f(end+1:lrf)=NaN;
   s(end+1:lrf)=NaN;
  end
  r_freq(:,end+1)=f'; r_spec(:,end+1)=s;
 end
%plot(f,s),drawnow
end


function [f,w]=acf2spec(acf,lag,m)
[k,l]=size(acf);
if nargin<3
 m=1:l;
end
% add window
for mm=m
 kkk=find(acf(:,mm)==0);
 if isempty(kkk), kk=k; else, kk=kkk(1); end
 wind=hamming(2*kk); acf(1:kk,mm)=acf(1:kk,mm).*wind(kk+1:end);
end
acf(1,m)=real(acf(1,m));
if all(imag(acf(k,m))==0)
 sp=real(fft([acf(1:k-1,m);conj(acf(k:-1:2,m))]));
 k=k-1;
else
 sp=real(fft([acf(1:k,m);zeros(1,length(m));conj(acf(k:-1:2,m))]));
end
f=[sp(k+1:2*k,:);sp(1:k,:)];
if nargin>1
 df=1/mean(diff(lag)); w=(-k:k-1)*df/(2*k); f=f/df;
end

function w=hamming(n)
w=.54-.46*cos(2*pi*(0:n-1)'/(n-1));

function [Spec,f]=acf2spec_wlsq(acf,acf_var,lag,n_freq,alpha_Tikhonov)
% acf2spec_wlsq ACF-to-Power-spectra for irregular samplings
% This function accurately transforma an irregularly sampled ACF to
% the corresponding power-spectra using explcitit calculation of
% the DFT-matrix for irregularly sampled ACF to a power-spectra on
% a regular frequency-grid.
% 
% The FFT-like transform ("like" since irragularly sampled) is done
% with SVD and feather-light 0th-order Tikhonov regularization.
% 
% Calling:
%  [Spec,f]=acf2spec_wlsq(acf,acf_var,lag,n_freq,alpha_Tikhonov)
% Input:
%  acf
%  acf_var
%  lag
%  n_freq
%  alpha_Tikhonov
% 
%  Copyright © Gustavsson and Vierinen 20190909,
%  bjorn.gustavsson@uit.no, juha.vierinen@uit.no
%  This is free software, licensed under GNU GPL version 2 or later

persistent U V invZ prev_lags % To facilitate speed-up
Tlsq_inv = 1;

% If no acf-variance is given use a constant variance
if isempty(acf_var)
  acf_var = ones(size(acf));
end

% Determine time-resolution
dt = median(diff(lag));

% If no specific number of frequencies are requested use the
% theoretical maximum - SVD and Tikhonov-regularization should
% automatically determine the intrinsic spectral resolution based
% on the available lags and lag-variance.
if nargin < 4 || isempty(n_freq)
  if dt < 1e-6 % This means we're looking at plasma-line-wide spectra
    n_freqs = numel(acf); % So half the theoretical spectral
                          % resolution is reasonable.
  else
    n_freqs = numel(acf)*2; % for the ionline we need to fly closer
                            % to the sun
    Tlsq_inv = 1;
  end
end

% This is a good default value for Tikhonov, if modifying this be
% sensible and keep the value between 1e-1 and 100. An alpha of 1
% is a guarantee that the conversion does not amplify noise. See
% for example Tarantola.
if nargin < 5 || iempty(alpha_Tikhonov)
  alpha_Tikhonov = 1;
end

% Ensure that the number of frequencies are odd, a bit superflous
n_freqs = floor(n_freqs/2)*2-1;
% Neat frequency array, ensure that a theory matrix for an evenly
% sampled ACF would be orthogonal.
f = fftfreq(dt,n_freqs);

% Re-Normalize acf-estimates (assuming they are uncorrelated)
acf = acf./sqrt(acf_var);
% Create theory-matrix re-normalized with the measurement variances
M = [cos(2*pi*lag(:)*f);sin(2*pi*lag(:)*f)]./repmat(sqrt(acf_var(:)),2,numel(f));
% These two steps gives the correctly weighted
% least-square-estimate of the power-spectra. The theory-matrix
% expresses the discretized Fourier-transform-relation between the
% real-valued power-spectrum and the complex-conjugate-symmetric
% ACF, for an ACF at unevenly sampled lags.
%
% Since we cash the SVD-decomposition of M it might be preferable
% to use an unweigthed least-square estimate. This assumes that the
% acf_var does not vary signifficantly from one range to the next
% over the ranges where the cashing works (i.e. identical or
% near-identical lags)
try
  if Tlsq_inv
    % For the robust inverse use SVD and 0th-order Tikhonov inverse.
    if numel(lag)==numel(prev_lags) && max(abs(lag-prev_lags))<eps(max(lag))
      % Just use previous SVD-inverse
    else
      [U,S,V] = svd(M);
      LAMBDA = diag(S);
      % The Tikhonov-regularization is done by damping - that only
      % significantly affects singular-values smaller that
      % alpha_Tikhonov.
      invZ = diag(LAMBDA./(LAMBDA.^2 + alpha_Tikhonov));
      prev_lags = lag;
    end
    Spec = V(:,1:size(invZ,2))*invZ*U(:,1:size(invZ,1))'*[real(acf(:));imag(acf(:))];
  else
    Spec = M\[real(acf(:));imag(acf(:))];
  end
catch
  % Tooo many comments?
  Spec = nan(size(f));
end

function f = fftfreq(dt,n)
% FFTFREQ - fft-frequency-array
%   
% Calling:
%   f = fftfreq(dt,n)
% Input:
%   dt - sampling duration (s), double scalar
%   n  - number of frequencies,  scalar int
% Output:
%   f - frequency array (Hz), dobule array

if rem(n,2) == 0
  f = [0:n/2-1,-n/2:-1] / (dt*n); %   if n is even
else
  f = [0:(n-1)/2,-(n-1)/2:-1] / (dt*n); %   if n is odd
end

f = fftshift(f);
