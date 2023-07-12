% GUISDAP v1.50   94-03-10 Copyright Markku Lehtinen, Asko Huuskonen 
%
start_GUP

name_expr='CP4B';  % Experiment name 
name_site='V';     % Site
data_path=['/geo/gmt/nickgjg/CP4Btest/']; % data directory
result_path=['/tmp/askoh/'];      % result directory

analysis_start=[1992   12    7  20  55   0]; % Start time
analysis_end=  [1992   12    7  24  10   0]; % End time

analysis_altit=[100 600]; analysis_classic=1;

display_figures=[1 1 1 1];
% display_figures(1) : data dump shown
% display_figures(2) : raw electron density shown
% display_figures(3) : data and fitted curve for each fit shown
% display_figures(4) : results shown after each dump

an_start
