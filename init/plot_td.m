%	function plot_td(arg1,arg2,arg3,...)
%
% A function for plotting the pulse schemes used in GUISDAP experiments.
% plot_td  can be called with or without arguments. With  no arguments,
% it plots one cycle of the experiment's transmission/reception intervals
% on all channels used. With arguments, plot_td can be used to plot all
% or part of the pulse scheme in given sets of  channels. In this second
% mode, a series of panels is produced according to the input arguments.
% plot_td can read from either the older td_variables, which store the
% relevant information, or the newer vc_variables which are based on the
% virtual channel concept. If both are present, plotting is performed
% from the vc_variables. To obtain these respective sets of variables,
% "path_GUP" must first point to the GUISDAP function directory. Then:
%
%	td_variables		vc_variables
%	============		============
%	glob_EISCAT		glob_GUP
%	name_expr='NAME'	name_expr='NAME'
%	name_site='X'		name_site='X'
%	load_PS			load_GUPvar
%
% 1) To obtain the default plot, enter:
%		plot_td
% The X-axis shows time in  milliseconds  after the beginning
% of the cycle. The Y-axis gives the respective channel number.
%
% 2) To call up set of panels, you must declare for each panel the
% channels to appear in the panel and the time limits. Using the 
% keyword 'panel', each panel is specified by 2 row arrays.
% An empty array in place
% of either of these causes the default values to be inserted later
% on. If not empty, the time limit array should contain either the
% start and end time for panel in microseconds from the beginning of
% the cycle, or these two limits plus the major and minor tickmark
% spacing (also in microseconds). A value of "Inf" is translated as
% a) the whole channel set or b) the min/max time limit.
%
% Example 1: CP1H, Troms{\o}, 3 panels
%	plot_td('panel',[1 2 3 4],[0 p_rep], ...
%		'panel',[6 7],[1000 2000 200 100], ...
%		'panel',[8 1 2],[500 inf])
%
% Example 2:	%% Default time limits ....
%	plot_td('panel',[1 2 3 4],[],'panel',[6 7],[],'panel',[8 1 2])
%
% Example 3:	%% Default channels for each panel ...
%	plot_td('panel',[],[0 p_rep],[],[1000 2000 200 100],[],[500 inf])
%
% 3) To obtain a plot of several cycles, type the keyword 'cycles':
%	plot_td('cycles',N')
%
% where N is the number of cycles which should be plotted.  To do
% this, plot_td makes use of the GUISDAP global variable, "p_rep".
% This input may be combined with any of the other input. There is
% no point defining more than N=1 cycles is in other input you limit
% the time to be plotted!
%
% Example 4:	%% Set the font details explicitly ...
%	plot_td('Fontname','Helvetica', ...
%		'Fontsize',11,	   ...
%		'Fontweight','Bold');
%
% The keywords 'Fontname', 'Fontsize', and 'Fontweight' can be used
% in the input arguments to specify the font used for he axis text.
%
% 4) After plotting, the top panel is made the "current" axes object
% so that a title can be added by typing:
% 	title('mytitle')
%
% Additional modifications can be made by accessing individual panels'
% handles, which are stored in the figure's UserData field e.g.
%	for i=get(gcf,'UserData'), axes(i), grid on, end
%
% 5) To obtain a hardcopy use the MATLAB "print" command. Please do not
% make your own alterations. In the event of difficulty, send e-mail to:
%	nickgjg@eiscat.ag.rl.ac.uk	OR	n.gazey@rl.ac.uk


function plot_td(arg1,  arg2,  arg3,  arg4,  arg5,  ...
		 arg6,  arg7,  arg8,  arg9,  arg10, ...
		 arg11, arg12, arg13, arg14, arg15, ...
		 arg16, arg17, arg18, arg19, arg20, ...
		 arg21, arg22, arg23, arg24, arg25, ...
		 arg26, arg27, arg28, arg29, arg30, ...
		 arg31, arg32, arg33, arg34, arg35, ...
		 arg36, arg37, arg38, arg39, arg40, ...
		 arg41, arg42, arg43, arg44, arg45, ...
		 arg46, arg47, arg48, arg49, arg50)

	%% Variables belonging to an older version of GUISDAP.
	global  td_am td_ch td_t1 td_t2

	%% Newer GUISDAP variables (virtual channel concept)
	global  vc_ch vc_env vc_envo vc_sampling

	%% Global variables  to be used with both
	global  p_rep p_dtau
if get(gcf,'color')==[1 1 1]
 white='k';
else
 white='w';
end

	%% Use the newer GUISDAP variables if present.
	if (~isempty(vc_ch)   & ~isempty(vc_env) & ...
	    ~isempty(vc_envo) & ~isempty(vc_sampling))
		%% Using  GUISDAP variables ...
		disp('Note: Using GUISDAP variables...')
		use_vc = 1;
	elseif (~isempty(td_ch) & ~isempty(td_am) & ...
		~isempty(td_t1) & ~isempty(td_t2))
		%% Using td_variables ...
		disp('Note: Using td_variables ... vc_variables not available')
		use_vc = 0;
	else
		error('No complete set of variables found to use ...')
	end

	%% Run a check on the 2 global variables need for both options ...
	if isempty(p_rep)
		error('Cannot run without global variable p_rep ...')
	elseif isempty(p_dtau)
		warning('Global variable p_dtau empty; setting p_dtau = 1')
		p_dtau = 1;
	end


	%% Some preliminary values ...
	channels  = [];		%% Default value for "channels" ...
	times     = [];		%% Default value for "times"...
	N         = 1;		%% Default value for "N" (no. cycles) ...
	goodgap   = 0.08;	%% Gap around/between panels ...
	font_name = 'Times';	%% Default font ...
	font_size = 10;		%% Default font size ...
	font_wght = 'Normal';	%% Default font weight ...


	%% Default channel numbers and times limits (1 cycle)
	if (use_vc)
		all_channels = sort(vc_ch)';
	else
		all_channels = sort(td_ch)';
	end
	all_channels(find(diff(all_channels)==0))=[];
	all_times = [0 p_rep]' .* p_dtau;


	%% The first stage is to get the arguments and check
	%% them for validity, or provide defaults in case the
	%% function is called without any ...................
 
	if (nargin==0)

		%% Use default channel numbers and times limits ...
		channels = all_channels;
		times    = all_times;

		%% Other default settings
		npanels   = 1;
		panels    = zeros(npanels,1);
		timecheck = size(times,1);

		% Additional numbers required by the programme ... for N=1
		if (use_vc)
			%% There are nrc reception intervals. Both
			%% reception and calibaration intervals are
			%% obtained using nrc.
			nrc = size(vc_sampling,1);
		else
			%% There are nvc reception, transmission and
			%% calibration intervals.  Transmitted pulse
			%% forms have been decomposed into pieces if
			%% the pulses are coded.
			nvc = length(td_ch);
		end

	else

		%% Parse the input arguments ...
		j=1; c=[]; t=[];
		while (j <= nargin)
			if  eval(['strcmp(arg' int2str(j) ',''panel'')'])
				%% Each panel specified separately ...
				j = j+1;
				if (j+1 > nargin)
					error('Usage of (2)')
				else
					if eval(['isempty (arg' int2str(j) ')'])
						c = gup_pad(c,inf,inf);
					else
						eval(['c=gup_pad(c,arg' int2str(j) ''',NaN);'])
					end
					j = j+1;
					if eval(['isempty (arg' int2str(j) ')'])
						t = gup_pad(t,ones(2,1)*inf);
					else
						eval(['t=gup_pad(t,arg' int2str(j) ''',NaN);'])
					end
				end

			elseif eval(['strcmp(arg' int2str(j) ',''cycles'')'])
				%% Number of cycles to be plotted ...
				j = j+1; if (j > nargin)
					error('Usage of (3)')
				else
					eval(['N=arg' int2str(j) ';'])
				end

			elseif  eval(['strcmp(arg' int2str(j) ',''Fontsize'')'])
				%% Set the fontsize explicitly ...
				j = j+1; if (j > nargin)
					error('Usage of Fontsize')
				else
					eval(['font_size=arg' int2str(j) ';'])
				end

			elseif  eval(['strcmp(arg' int2str(j) ',''Fontweight'')'])
				%% Set the fontweight explicitly ...
				j = j+1; if (j > nargin)
					error('Usage if Fontweight')
				else
					eval(['font_wght=arg' int2str(j) ';'])
				end

			elseif  eval(['strcmp(arg' int2str(j) ',''Fontname'')'])
				%% Set the fontname explicitly ...
				j = j+1; if (j > nargin)
					error('Usage of Fontname')
				else
					eval(['font_name=arg' int2str(j) ';'])
				end

			end
			j = j + 1;
		end

		channels = c;
		times    = t;
		if ((N==0) | (rem(N,1) > 0)), N=1; end

		%% Is it necessary to duplicate the time variables
		%% in order to plot several cycles? ..............
		if (N > 1)
			warning('Do not interrupt this process!!!')
			if (use_vc)
				vc_sampling_copy = vc_sampling;
				vc_envo_copy     = vc_envo;
				vc_env_copy      = vc_env;
				vc_ch_copy       = vc_ch;
				
				vc_envo      = dupcol2(vc_envo,N,p_rep);
				vc_env       = dupcol2(vc_env,N);
				vc_ch        = dupcol2(vc_ch,N);
				vc_sampling  = duprow2(vc_sampling,N);
				vc_sampling(:,1)     = duprow2(vc_sampling_copy(:,1),N,max(vc_sampling_copy(:,1)));
				vc_sampling(:,[3 4]) = duprow2(vc_sampling_copy(:,[3 4]),N,p_rep);
			else
				td_ch_copy = td_ch;
				td_am_copy = td_am;
				td_t1_copy = td_t1;
				td_t2_copy = td_t2;
		
				td_ch = dupcol2(td_ch,N);
				td_am = dupcol2(td_am,N);
				td_t1 = dupcol2(td_t1,N,p_rep);
				td_t2 = dupcol2(td_t2,N,p_rep);
			end
		end
		if (use_vc)
			nrc = size(vc_sampling,1);
		else
			nvc = length(td_ch);
		end

		%% A) Do channels and times correspond to the same number
		%% of panels?
		if (size(channels,2) ~= size(times,2))
			error(['Number of columns in channels matrix ' ...
			      'must equal number of columns in times matrix'])
		end
		npanels = size(channels,2);

		%% B) Either start and end time can be given, or all of
		%% start time, end time, major and minor tickmark spacing
		%% and label spacing. Defaults are provivded if start time
		%% and end time are provivded. Otherwise all must be given.
		timecheck = size(times,1);
		if (timecheck < 2)
			error('Start and end time for each panel must be specified')
		elseif ((timecheck==3) | (timecheck > 4))
			warning(['Either supply only start and end times,' ...
			 'or start and end time plus major and minor' ...
			 'tickmark spacing (no other combination)'])
			times(3:timecheck,:) = [];
			timecheck = size(times,1);
		end

		%% C) If the value 'Inf' occurred anywhere, then
		%%    defaults are to be supplied in its place.
		if any(any(isinf(channels)))
			%% I) Replace any column of the channel array
			%%    containing an 'inf' with the default set
			if (size(channels,1)==1)
				infset =  isinf(channels);
				notinf = ~isinf(channels);
			else
				infset =  any(isinf(channels));
				notinf = ~any(isinf(channels));
			end
			if any(infset)
				r1  = size(channels,1);
				c1  = size(channels,2);
				r2  = size(all_channels,1);
				tmp = zeros(r2,c1);
				tmp(1:r1,1:c1) = channels;
				tmp(:,infset)  = ...
				    dupcol(all_channels,length(find(infset)));
				if any(notinf)
				    tmp(r1+1:r2,notinf) = ...
					dupcol(duprow(NaN,r2-r1),length(find(notinf)));
				end
			end
			channels = tmp;
		end
		
		if any(any(isinf(times)))
			%%  II) Replace 'inf' lower limit with all_times(1) ...
			tmp = find(isinf(times(1,:)));
			if ~isempty(tmp)
				times(1,tmp) = dupcol(all_times(1),length(tmp));
			end
			%% III) Replace 'inf' upper limit with all_times(2) ...
			tmp = find(isinf(times(2,:)));
			if ~isempty(tmp)
				times(2,tmp) = dupcol(all_times(2),length(tmp));
			end
			%%  IV) Replace 'inf' tickmarkangaben with NaN's ...
			if (timecheck >2)
				tmp = find(isinf(times(3,:)));
				if ~isempty(tmp)
					times(3,tmp) = dupcol(NaN,length(tmp));
				end
				%% V)   Replace 'inf' tickmarkangaben with NaN's ...
				tmp = find(isinf(times(4,:)));
				if ~isempty(tmp)
					times(4,tmp) = dupcol(NaN,length(tmp));
				end
			end
		end

		%% a) Sort out invalid channel and time limits. Applies
		%% to genuine user input. If the user supplied both times
		%% and channels numbers the default values will not have
		%% been calculated, and so need to be calculated here.
		if (use_vc)
			expch = sort(vc_ch)';
		else
			expch = sort(td_ch)';
		end
		expch(find(diff(expch)==0))=[];
		ti = 0;
		tf = N * p_rep * p_dtau;
		togo = [];
		for i = 1:npanels
			testch = channels(:,i);
			testch(find(isnan(testch)))=[];
			testti = times(1,i);
			testtf = times(2,i);

			%% Check each panel's channel information .....
			if any(~any(dupcol(expch,length(testch))==duprow(testch',length(expch))))
				warning(['Channel selection ' num2str(testch) ' is invalid'])
				togo = [togo i]; %% Any columns to go ...

			%% Check each panel's time information .....
			elseif ((testtf > tf) | (testti < ti))
				warning(['Times ' num2str(testti) ' and ' num2str(testtf) ' are invalid'])
				togo = [togo i]; %% Any columns to go ...
			end
		end

		%% Is it necessary to revise the number of panels?
		if ~isempty(togo)
			npanels = npanels - length(togo);
			if (npanels < 1)
				error('no valid input information')
			else
				channels(:,togo) = [];
				times(:,togo)    = [];
			end
		end

		%% Space for panels handles ...
		panels = zeros(npanels,1);

	end

	%% If only start and end times are given, supply the rest.
	%% If a times array is given which is gup_padded out with NaNs
	%% then timecheck will not detect that fact. So run all
	%% time arrays through this loop..........................
	tstore = times;
	times  = zeros(4,npanels);
	for i = 1 : npanels
		temp1 = tstore(:,i);
		temp2 = temp1(~isnan(temp1)); 
		if (size(temp2,1) < 4)
			tdiff = temp2(2)-temp2(1);
			tdl=log10(tdiff); tdf=10^(rem(tdl,1)),
			if tdf>5
				dt1=0.5; dt2=0.1;
			elseif tdf>3
				dt1=0.2; dt2=0.1;
			elseif tdf>1
				dt1=0.1; dt2=0.05;
			else
				dt1=0.05; dt2=0.01;
			end
			tdf=10^floor(tdl);
			dt1=max(dt1*tdf,1); dt2=max(dt2*tdf,1);
			times(:,i) = [temp2(1);temp2(2);dt1;dt2];
		else
			times(:,i) = temp1;
		end
	end

	%% 2) Plot the picture ....
	figure(gcf), clf, set(gcf,'Resize','On','Name','plot_td')
	for i = 1 : npanels

		%% Channels to be plotted in this panel ...
		panelchannels = channels(:,i);
		panelchannels = panelchannels(~isnan(panelchannels));

		%% Time information pertaining to this panel ...
		paneltimes    = times(:,i);
		paneltimes    = paneltimes(~isnan(paneltimes));

		%% No. channels required in this panel ....
		nch = length(panelchannels);

                %% Underlying time units .....
                tgrid   = (paneltimes(1):paneltimes(2))';

                %% Used for placing major tickmarks .....
                tmajor  = tgrid(find(rem(tgrid,paneltimes(3))==0));

                %% Used for placing minor tickmarks .....
                tminor  = tgrid(find(rem(tgrid,paneltimes(4))==0));

		%% Time axis labels ..... in ms, not us.
		xlabels = makelabels(tmajor/1000);

		%% In td_ch, channel 0 actually represents
		%% calibration, which is applied to all 
		%% channels at once in EISCAT experiments.
		clabels = makelabels(panelchannels,0,'mf');

		%% Indices for the channel numbers put in. These
		%% are used for placing different patches on the
		%% plot. A channel may be plotted several times.
		cindex = find(~isnan(panelchannels));

		%% The latest panel ...
		dy = (1-((npanels+1)*goodgap))/npanels;
		y0 = (1 - ((goodgap + dy)*i));
		panels(i) = axes(...
		  'Units','normal',...
		  'Position',[goodgap y0 1-(2*goodgap) dy],...
        	  'Box','On',...
		  'Fontname',font_name,...
		  'Fontsize',font_size,...,
		  'Fontweight',font_wght,...
        	  'TickLength',[0 0],...
		  'Visible','On',...
		  'xtick',tmajor,...
		  'xlim',[paneltimes(1) paneltimes(2)],...
		  'xticklabel',str2mat(xlabels),...
		  'ytick',cindex+0.5,...
		  'ylim',[min(cindex) max(cindex)+1],...
		  'yticklabel',num2str(clabels));

		if (use_vc)
			%% An array used for matching up selected
			%% channels against actual channels.
			CH = duprow(vc_ch(vc_sampling(:,1)'),nch);

			%% An array which records whether a given
			%% reception interval is associated with
			%% calibaraion, background measurement or
			%% signal reception.
			CL = duprow(vc_sampling(:,2)',nch);

			%% Array which records whether a chosen
			%% channel is present in the data.
			selected = (CH==dupcol(panelchannels,nrc));
		else
			CH  = duprow(td_ch,nch);
			AM  = duprow(td_am,nch);
			selected = (CH==dupcol(panelchannels,nvc));
		end

		if (use_vc)

			%% 1) RECEPTION ...
			found = (selected)';
			riech = sum(found);
			index = rem(find(found),nrc);
			if ~isempty(index)
				%% There were reception intervals in the
				%% selected intervals ...................
				if (any(index==0))
					%% Fill in gaps where rem gave 0!!!
					tmp = find(index==0);
					index(tmp) = dupcol(nrc,length(tmp));
				end
				t1 = vc_sampling(index,3);
				t2 = vc_sampling(index,4);
				yaxis = dupcol(cindex',riech) + 0.5;
				recep_x = [t1 t2 t2 t1]' .* p_dtau;
				recep_y = [yaxis; yaxis; (yaxis+0.35); (yaxis+0.35)];
			else
				disp(['Note: No reception intervals in panel ' int2str(i) ' ...'])
			end
		
	
			%% 2) CALIBRATION ... Calibration is applied to all
			%% channels at once in EISCAT experiments. However,
			%% calibaration information is not stored in GUISDAP
			%% variables, and must be inferred from reception
			%% intervals marked with the appropriate flag. This
			%% means that the calibration interval which appears
			%% may not be of the right length!!
	
			%% Any calibaration intervals in selected channels
			%% found = (CL==2 & selected)';
			%% Any calibration intervals on any channel ...
			found = (CL==2)';
			riech = sum(found);
			index = rem(find(found),nrc);
			if ~isempty(index)
				%% There were transmission intervals on the
				%% selected channels ...................
				if (any(index==0))
					%% Fill in gaps where rem gave 0!!!
					tmp = find(index==0);
					index(tmp) = dupcol(nrc,length(tmp));
				end
				t1 = vc_sampling(index,3);
				t2 = vc_sampling(index,4);
				calib_x = [t1 t2 t2 t1]' .* p_dtau;
				calib_y = dupcol([min(cindex) min(cindex) max(cindex)+1 max(cindex)+1]',length(index));
			else
				disp(['Note: No calibration intervals in panel ' int2str(i) ' ...'])
			end
	
			%% 3) TRANSMISSION ... GUISDAP variables store the
			%% transmitted pulse patterns, and their start times.
			%% The relevant channel must be inferred from those
			%% reception intervals associated with signal reception.
			%% The form the the transmitted signal must then be
			%% transformed into a suitable patch for plotting.

			found = (CL==3 & selected)';
			riech = sum(found);
			index = rem(find(found),nrc);
			if ~isempty(index)
				%% There were transmission intervals on the
				%% selected channels ......................
				if (any(index==0))
					%% Fill in gaps where rem gave 0!!!
					tmp = find(index==0);
					index(tmp) = dupcol(nrc,length(tmp));
				end
				yaxis = dupcol(cindex',riech) + 0.5;
				%% We now have the virtual channel numbers
				%% associated with signal reception.......
				vcs = vc_sampling(index,1);
				env = vc_env(:,vcs); %% Chosen subset of pulses
				stm = vc_envo(vcs);  %% The pulses' start times
				%% Convert the function form of the transmitted
				%% pulses into suitable patches by finding  the
				%% places where the pulse phase changed........
				%% r = maximum pulse length....................
				%% c = number of virtual channels chosen.......
				[r,c]      = size(env);
				denv       = [env(1,:) ; diff(env); -env(r,:)];
				[a,b,s]    = find(denv~=0);
				anychange  = sparse(a,b,s,r+1,c);
				trans_x = []; trans_y = [];
				for k = 1 : c	%% For each chosen vc
					ind     = find(anychange(:,k));
					%% Padded with NaN's (see drawing stage)
					trans_x = gup_pad(trans_x,[stm(k); duprow(stm(k)+ind,2); stm(k)+r],NaN);
					trans_y = gup_pad(trans_y,[duprow(yaxis(k)+env(1,k),2); duprow(yaxis(k)+(0.35*env(ind,k)),2)],NaN);
				end
				%% Convert GUISDAP time units to us
				trans_x = trans_x .* p_dtau;
			else
				disp(['Note: No transmission intervals in panel ' int2str(i) ' ...'])
			end

		else	%% USE TD_VARIABLES .....

			%% 1) CALIBRATION ... Calibration is applied to all
			%% channels at once in EISCAT experiments.Therefore
			%% the corresponding elements of the  td_varaiables
			%% can be found particularly simply.
	
			index = find(td_ch==0);
			if ~isempty(index)
				t1      = td_t1(index);
				t2      = td_t2(index);
				calib_x = [t1' t2' t2' t1']' .* p_dtau;
				calib_y = dupcol([min(cindex) min(cindex) max(cindex)+1 max(cindex)+1]',length(index));
			else
				disp('Note: No calibration intervals ...')
			end
	
			%% 2) TRANSMISSION ... (Note: td_am may be +1 or -1)
	
			found = (abs(AM)==1 & CH>0 & selected)';
			riech = sum(found);
			index = rem(find(found),nvc);
			if ~isempty(index)
				%% There were transmission intervals on the
				%% selected channels ...................
				if (any(index==0))
					%% Fill in gaps where rem gave 0!!!
					tmp = find(index==0);
					index(tmp) = dupcol(nvc,length(tmp));
				end
				t1 = td_t1(index);
				t2 = td_t2(index);
				yaxis = []; for c=1:length(panelchannels)
					yaxis = [yaxis dupcol(cindex(c),riech(c))];
				end
				yaxis   = yaxis + 0.5;
				trans_x = [t1' t2' t2' t1']' .* p_dtau;
				trans_y = [yaxis; yaxis; (yaxis+(td_am(index)*0.35)); (yaxis+(td_am(index)*0.35))];
			else
				disp(['Note: No transmission intervals in panel ' int2str(i) ' ...'])
			end
	
			%% 3) RECEPTION ...
	
			found = (AM==2 & selected)';
			riech = sum(found);
			index = rem(find(found),nvc);
			if ~isempty(index)
				%% There were reception intervals in the
				%% selected intervals ...................
				if (any(index==0))
					%% Fill in gaps where rem gave 0!!!
					tmp = find(index==0);
					index(tmp) = dupcol(nvc,length(tmp));
				end
				t1 = td_t1(index);
				t2 = td_t2(index);
				yaxis = []; for c=1:length(panelchannels)
					yaxis = [yaxis dupcol(cindex(c),riech(c))];
				end
				yaxis   = yaxis + 0.5;
				recep_x = [t1' t2' t2' t1']' .* p_dtau;
				recep_y = [yaxis; yaxis; (yaxis+0.35); (yaxis+0.35)];
			else
				disp(['Note: No reception intervals in panel ' int2str(i) ' ...'])
			end
		end
	
		%% Now create the major tick marks ...
		%% 1) Lines within the panel ...
		gap = paneltimes(3)/1000;
		xx1 = duprow(dupcol(min(tmajor)/1000 + gap : gap : max(tmajor)/1000 - gap,length(cindex)),2)*1000;
		yy1 = [dupcol2(min(cindex)+0.5:max(cindex)+0.5,length(tmajor)-2) + 0.10;...
		       dupcol2(min(cindex)+0.5:max(cindex)+0.5,length(tmajor)-2) - 0.10];

		%% 2) Lines at the edge of the panel ...
		if (nch < 3)
			xx2 = duprow(dupcol(min(tmajor)/1000 + gap : gap : max(tmajor)/1000 - gap,2),2)*1000;
			yy2 = dupcol2([min(cindex) max(cindex) + 1 ; min(cindex) + 0.10 max(cindex) + 1 - 0.10],length(tmajor)-2);
		end

		plot_minor_tickmarks = (paneltimes(4) < paneltimes(3));

		%% Now create the minor tick marks ... 
		if (plot_minor_tickmarks)
			%% 3) Lines within the panel ...
			gap = paneltimes(4)/1000;
			xx3 = duprow(dupcol(min(tminor)/1000 + gap : gap : max(tminor)/1000 - gap,length(cindex)),2)*1000;
			yy3 = [dupcol2(min(cindex)+0.5:max(cindex)+0.5,length(tminor)-2) + 0.05;...
			       dupcol2(min(cindex)+0.5:max(cindex)+0.5,length(tminor)-2) - 0.05];

			%% 4) Lines at the edge of the panel ...
			if (nch < 3)
				xx4 = duprow(dupcol(min(tminor)/1000 + gap : gap : max(tminor)/1000 - gap,2),2)*1000;
				yy4 = dupcol2([min(cindex) max(cindex) + 1 ; min(cindex) + 0.05 max(cindex) + 1 - 0.05],length(tminor)-2);
			end
		end

		%% Draw the patches in first as they are opaque ...
		patch(calib_x,calib_y,[.9 .9 .9],'EdgeColor',white)
		patch(recep_x,recep_y,[.5 .5 .5],'EdgeColor',white)
%		patch(calib_x,calib_y,'y','EdgeColor',white)
%		patch(recep_x,recep_y,'m','EdgeColor',white)
		if (use_vc)
			%% NB. patch does not accept sparse matrices ....
			%% Nor may there be extraneous 0's, and the presence
			%% of NaNs (for gup_padding) causes nothing to be plotted.
			for k = 1 : c	%% For each chosen virtual channel
				x=full(trans_x(:,k)); x = x(~isnan(x));
				y=full(trans_y(:,k)); y = y(~isnan(y));
				patch(x,y,white,'EdgeColor','None')
			end
		else
			patch(trans_x,trans_y,white,'EdgeColor',white)
		end

		%% Draw in horizontal lines which represent channels ...
		line(duprow2([paneltimes(1) paneltimes(2)],length(cindex))',dupcol(cindex+0.5,2)','Color',white)
 
		%% Major tickmarks within plot .....
		line(xx1,yy1,'Color',white)
		if (nch < 3)
			%% Major tickmarks at edge of picture .....
			line(xx2,yy2,'Color',white)
		end
		if (plot_minor_tickmarks)
			%% Minor tickmarks within plot .....
			line(xx3,yy3,'Color',white)
			if (nch < 3)
				%% Plot tickmarks at edge of picture .....
				line(xx4,yy4,'Color',white)
			end
		end

		%% Time axis label .....
		if (i==npanels), xlabel('ms'), end
	end

	%% Make the top panel the current one so that the user
	%% can add a title .....
	axes(panels(1))

	%% Store the panels handles for subsequent use ....
	disp(['Note: Individual panel handles are obtained using: ' ...
	      'get(gcf,''UserData'')'])
	set(gcf,'UserData',panels);


	%% 5) If several repetitions were plotted, return the variables
	%% to normal ..................................................

	if (N > 1)
		if (use_vc)
			vc_ch       = vc_ch_copy;
			vc_envo     = vc_envo_copy;
			vc_env      = vc_env_copy;
			vc_sampling = vc_sampling_copy;
		else
			td_ch       = td_ch_copy;
			td_am       = td_am_copy;
			td_t1       = td_t1_copy;
			td_t2       = td_t2_copy;
		end
		disp('Note: You may now use interrupts again .....')
	end
%end

