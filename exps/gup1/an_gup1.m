% 
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen

start_GUP
HOME=getenv('HOME');
if length(HOME)>0, HOME=[HOME,'/']; end

% Experiment name and site and paths
name_expr='gup1';     name_site='L';
data_path=[HOME 'datat/esrmar25/'];   % data folder
result_path=path_tmp;   % result folder

% Time interval to analyze in form
%               Year Month Day Hour Min Sec
analysis_start=[1996   3    25  10  49  00];
analysis_end=  [1996   3    25  12  59   0];

% postintegration strategy defines how many seconds first to skip and integrate.
% the vectors are used cyclically
analysis_integr=[600];
analysis_skip  =[  0];  % Optional, assumed to be zero if not specified

% Range gate definitions for the alternating codes and long pulses
%rangeAC=[120:6:210, 210:12:330 330:36:690];lenAC=length(rangeAC);
rangeAC=[120:6:210, 210:12:330];lenAC=length(rangeAC);
rangeLP=[210:12:330 330:36:690];lenLP=length(rangeLP);

% Following lines give alternative ways to combine the data in the analysi
% Select one pair of analysis_range and analysis_code

% Analyse all alternating codes together (no long pulses)
%analysis_range=[rangeAC];
%analysis_code=[5678*ones(1,lenAC)];

% Analyse all long pulses together (no alternating codes)
%analysis_range=[rangeLP];
%analysis_code=[1234*ones(1,lenLP)];

% Analyse all alternating codes together first and all long pulses together after that
%analysis_range=[rangeAC rangeLP];
%analysis_code=[5678*ones(1,lenAC),1234*ones(1,lenLP)];

% Divide data so that first and seconds half of IPP are kept separate
%analysis_range=[rangeAC rangeAC rangeLP rangeLP];
%analysis_code=[56*ones(1,lenAC),78*ones(1,lenAC),12*ones(1,lenLP),34*ones(1,lenLP)];

% Analyze first long pulses of both IPPs together and then the second pulses together
analysis_range=[rangeLP rangeLP];
analysis_code=[13*ones(1,lenLP),24*ones(1,lenLP)];

% Analyze first alternating code pulse of both IPP together and then the second pulses together
%analysis_range=[rangeLP rangeLP];
%analysis_code=[57*ones(1,lenLP),68*ones(1,lenLP)];

analysis_control=[1e5 .01 20 1];
display_graphics=[0 0 0 0];
an_start
