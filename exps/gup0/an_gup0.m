% 
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen

start_GUP
HOME=getenv('HOME');
if length(HOME)>0, HOME=[HOME,'/']; end

% Experiment name and site and paths
name_expr='gup0';     name_site='L';
data_path=[HOME 'datat/esrmar17/'];   % data folder
result_path=[path_tmp];   % result folder

% Time interval to analyze in form
%               Year Month Day Hour Min Sec
analysis_start=[1996   3    17  15 15  00];
analysis_end=  [1996   3    17  15 20  00];

% postintegration strategy defines how many seconds first to skip and integrate.
% the vectors are used cyclically
analysis_integr=[600];
analysis_skip  =[  0];  % Optional, assumed to be zero if not specified

% The range gate definitions
rangeLP=[150:12:330 330:36:690 690:72:1000];lenLP=length(rangeLP);

% Analyze each long pulse separate, combining the two halfs of IPP together
analysis_range=[rangeLP rangeLP rangeLP rangeLP];
analysis_code=[kron([13 24 57 68],ones(1,lenLP))];

% Special arrangement to drop the clutter contaminated first gates from each long pulse
%analysis_range=[rangeLP];
%analysis_code=[57*ones(1,6), 5724*ones(1,5), 132457*ones(1,lenLP-11)];

analysis_control=[1e5 .01 20 1];
display_graphics=[0 0 0 0];
an_start
