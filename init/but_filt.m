% butfilt.m: Interpolates the butterworth filter impulse response function
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% from the values measured by Jussi Markkanen
%
% input parameters:
% BW     : desired bandwidth in kHz (EISCAT definition)
% ch     : specifies the channel numbers
% p_dtau : global time unit in us (global parameter)
% output parameters:
% impresp: the impulse response
% ch_p   : impulse resposes for channels given by ch (global)
%
% See also: lin_filt, REC_impresp
%
% function impresp=but_filt(BW,ch);
  function impresp=but_filt(BW,ch);

  global ch_p p_dtau

% 
% measured impulse response of a 25 kHz Butterworth filter.
% values given for each us.
  p_m=[ -0.002 -0.003 -0.002 -0.001  0.001  0.006  0.013  0.023  0.036 ;
         0.053  0.072  0.095  0.120  0.148  0.176  0.204  0.233  0.260 ;
         0.285  0.307  0.327  0.342  0.353  0.360  0.362  0.360  0.353 ;
         0.342  0.327  0.309  0.287  0.263  0.236  0.208  0.179  0.150 ;
         0.121  0.093  0.065  0.040  0.016 -0.006 -0.025 -0.041 -0.055 ;
        -0.067 -0.075 -0.081 -0.084 -0.085 -0.084 -0.081 -0.077 -0.070 ;
        -0.063 -0.055 -0.047 -0.038 -0.029 -0.020 -0.012 -0.004  0.004 ;
         0.010  0.016  0.021  0.025  0.028  0.030  0.031  0.032  0.031 ;
         0.030  0.029  0.026  0.024  0.021  0.018  0.014  0.011  0.008 ;
         0.004  0.001 -0.002 -0.004 -0.006 -0.008 -0.010 -0.011 -0.012 ;
        -0.012 -0.013 -0.012 -0.012 -0.011 -0.010 -0.009 -0.008 -0.006 ;
        -0.005 -0.003 -0.002  0.000  0.001  0.002  0.004  0.005  0.005 ;
         0.006  0.007  0.007  0.007  0.007  0.007  0.007  0.006  0.006 ;
         0.005  0.004  0.004  0      0      0      0      0      0];
  p_m=col(p_m');
  len = length(p_m);

  dt  = 25/BW;  % time step for the desired bandwidth

% interpolation
  p_mx= ((0:(len-1))*dt)';
  px  = (0:p_dtau:(len-1)*dt)';
  p = spline(p_mx,p_m,px);
  
% scale to get unit area and
% assign to all channels specified at input
  
  impresp=p/sum(p);
  if nargin==2,
   ch_p(1:length(px),ch) = impresp*ones(1,length(ch));
  end
