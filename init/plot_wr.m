%	texthandles = plot_wr(arg1,arg2,arg3,...);
%
% plot_wr plots lag profile group range ambiguity functions At present
% plot_wr only plots signal range ambiguity functions.  plot_wr can be
% called with or without arguments.  With no arguments, it plots the range
% ambiguity functions of all signal lag profile groups.  With arguments,
% plot_wr can be used to plot some or all of the signal lag profile group
% range ambiguity functions over a selected time range from the beginning
% of transmission.  In this second mode, a series of panels is produced
% according to the input arguments.  To obtain the necessary variables,
% "path_GUP" must first point to the GUISDAP function directory, and the
% following calls must have been made:
% 
%	glob_GUP
%	name_expr='NAME';
%	name_site='X';
%	load_GUPvar
%	load_initfile
%
% The various ambiguity functions must be calculated upon the first
% execution of plot_wr, unless they are present in a file called
% wr_NAME.mat, where NAME is the name of the experiment.  If this
% information is not available anywhere, it will be stored after the
% first execution of plot_wr() in the file:
%
%	path_tmp/wr_NAME.mat
%
% The directory path_tmp is also where this information is sought.
%
% 1) To obtain the default plot, enter:
%		plot_wr;
%
% The X-axis shows time in  milliseconds  after the beginning of
% transmission. Next to each ambiguity function is the number of
% the lag profile group which it comes from.
%
% 2) To call up set of panels, you must declare for each panel 
% 	i)  The numbers of the lag profile groups whose range
% 	    ambiguity functions are to appear in it
% 	ii) The panel's time limits.
% Using the keyword 'panel', the lpg numbers and panel time limits are
% each specified by a simple array.  An empty array in place of either the
% l.p.g number array or the r.a.f array causes default values to be
% inserted.  If not empty, the time limits array should contain either the
% start and end time for the panel in microseconds from the beginning of
% transmission, or these two limits plus the tickmark spacing (also in
% microseconds).  A value of "Inf" is translated:
% 	A) As the whole lag profile set (if inserted into the lpg number
% 	   array) or
% 	B) as the min/max time limit (if inserted into the time limits
% 	   array). 
% 
% Example 1: CP1H, 2 panels
%	plot_wr('panel',95:120,[600 inf],'panel',28:39,[]);
%
% plots 2 panels; in the first, ambiguity functions for lpgs
% 95:120 are plotted between 600us after transmission and the
% maximum time limit with default tickmark spacing; in the
% second, the ambiguity functions for lpgs 28:39, with default
% time limits, appear.
%
% Example 2:
%	plot_wr('panel',[],[],'panel',[39 42],[]);
%
% plots 2 panels; in the first, ambiguity functions for all
% lpgs are plotted between default time limits; in the second,
% the ambiguity functions for (signal) lpgs 39 and 42, again with
% default time limits.
%
% Example 3:
%	texthandles = plot_wr('Fontname','Helvetica', ...
%			      'Fontsize',11,	      ...
%			      'Fontweight','Bold');
%
% The keywords 'Fontname', 'Fontsize', and 'Fontweight' can be used
% in the input arguments to specify the font used for both the axis
% text and the lag profile group numbers. The font used for the lag
% profile group numbers can subsequently be altered, using the 
% returned array "texthandles" e.g.
%
%	set(texthandles,'Fontname','Times', ...
%			'Fontsize',11,	    ...
%			'Fontweight','Normal')
%
% The keyword 'clear' causes the internally stored ambiguity function
% information for a given experiment to be cleared.  This is in case it is
% desired to recreate the file wr_NAME.mat. 
%
% Example 4: 
%	plot_wr(......,'clear');
% 
% Additional modifications can be made by accessing individual panels' 
% handles, which are stored in the figure's UserData field e.g.
% 	for h=get(gcf,'UserData'), axes(h), grid on, end
%
% For certain experiments, the calculation of all the range ambiguity
% functions takes considerable time.  To force the calculation of only
% those range ambiguity functions given explicitly with the argument
% 'panel', the user may specify the argument 'limit' in the argument list. 
% A limited set is created ONLY if the all the signal lpg range ambiguity
% functions are NOT ALREADY AVAILABLE as a complete set in the file
% wr_NAME.mat.  If 'limit' is specified, and they CAN be loaded from
% wr_NAME.mat, then they are (since it is not possible for plot_wr to tell
% whether the global variable wr_NAME contains the full set anymore, or a
% limited set from the last call!).  Therefore to be sure to create the
% limited set only, include 'clear' somewhere amongst the arguments as
% well. Not that since the programme handles only signal range ambiguity
% functions, limited sets are not saved to the file wr_NAME.mat, to save
% indexing becoming a major headache!
%
% Example 5:
%	plot_wr(......,'limit');
%
% To obtain a hardcopy use the MATLAB "print" command.  Do not make
% your own alterations.  In the event of difficulty, send e-mail to:
%	nickgjg@eiscat.ag.rl.ac.uk OR  n.gazey@rl.ac.uk
% See also: plot_td
% See also: plot_fit

function texthandles = plot_wr( arg1,  arg2,  arg3,  arg4,  arg5,  ...
				arg6,  arg7,  arg8,  arg9,  arg10, ...
				arg11, arg12, arg13, arg14, arg15, ...
				arg16, arg17, arg18, arg19, arg20, ...
				arg21, arg22, arg23, arg24, arg25, ...
				arg26, arg27, arg28, arg29, arg30)

	%% GUISDAP variables ...
	global lpg_bcs lpg_dt lpg_nt lpg_h p_dtau

	%% GUISDAP variables ....
	global name_expr path_tmp
if get(gcf,'color')==[1 1 1]
 white='k';
else
 white='w';
end

 	%% If certain necessary variables are empty, we cannot proceed
	if (isempty(lpg_bcs) | isempty(lpg_dt) | isempty(p_dtau))
	 	error(['Variable set not complete ... ' ...
			' try typing "load_initfile" and "load_GUPvar"'])
	end

	%% wr_NAME is a global variable used by the programme, which
	%% stores the lag profile groups' range ambiguity functions.
	%% This information may be time-consuming to produce, and its
	%% production may be a serious handicap in running the programme.
	%% The argument "limit" causes a limited set of lpg raf's to be
	%% calculated and subsequently stored in  wr_file. Note that
	%% only complete sets of lpg raf's are ever stored to wr_file.

	if isempty(name_expr)
		error('''name_expr'' is blank! Is it global? Should be..')
	else
		eval(['wr_name = ''' 'wr_' name_expr ''';'])
		eval(['wr_file = ''' path_tmp wr_name '.mat'';'])
		eval(['global ' wr_name])
	end

	%% Find the numbers of the signal lag profile groups ...
	all_lpgs = find(lpg_bcs=='s' | lpg_bcs=='x' | lpg_bcs=='o')';

	%% In case of mistakes in the  global variable  wr_NAME, the
	%% user may wish to recreate the wr_NAME array, by including
	%% the argument  'clear' in the argument list. Check for the
	%% argument  'limit'  in the process, since we  need to know 
	%% if only a limited set of lpg raf's are to be calculated.
	clean=0; limit=0;
	for a=1:nargin
		if eval(['strcmp(arg' int2str(a) ',''clear'')'])
			disp(['Note: Clearing variable ' wr_name ' ...'])
			eval([wr_name '=[];'])
			clean = 1;
		elseif eval(['strcmp(arg' int2str(a) ',''limit'')'])
			limit = 1;
			clean = 1;
		end
	end

	%% Special condition to empty the global variable wr_name
	%% if it seems to have the wrong dimensions ...
	if eval(['(~isempty(' wr_name ') & ' 			...
		 '(size(' wr_name ',2) < length(all_lpgs)) & '	...
		 '(limit==0))'])
		warning(['The global variable ' wr_name ' is possibly incomplete'])
		replace = minput('Do you want to create it afresh','n',1);
		if strcmp(replace,'y')
			disp(['Note: Clearing variable ' wr_name ' ...'])
			eval([wr_name '=[];'])
		end
	end

	%% Now create the data used by the programme ...
	if eval(['isempty(' wr_name ')'])
		%% This may happen upon startup, or if the argument
		%% "clear" has been used. If this information is
		%% available from wr_file, it is loaded whether
		%% or not the flag "limit" is set, since loading it
		%% is a fast operation. If wr_file is not available,
		%% and limit=0, a complete set of signal lpg raf's
		%% is calculated and saved to wr_file. If wr_file is
		%% not available and limit=1, the calculation of the
		%% limited set is deferred until the required lpg
		%% numbers are known, and these are NOT saved to
		%% wr_file (this ensures that only complete sets of
		%% signal lpg raf's are saved to wr_file).
		if eval(['(exist(''' wr_file ''')==2)'])
			disp(['Note: Loading ' wr_file ' ...'])
			load(wr_file)
		else
			if (limit==1)
				to_calculate=[];  %% defer till later 
			else
				to_calculate=all_lpgs;
			end
			k=0; total=length(to_calculate);
			for i = to_calculate'
				k = k + 1;
				disp(['Profile ' int2str(k)                 ...
				      ' (Lag profile group ' int2str(i) ')' ...
				      ' out of ' int2str(total) ' .....'])
				%% Takes forever if stored directly as a
				%% sparse array, so store as a full one
				%% instead (See documentation to gup_pad.m)
				eval(['wr_' name_expr ...
				      ' = gup_pad(wr_' name_expr ',wrlpg(i),0);'])
			end
			if (limit==0)	%% Full set of signal raf's ...
				disp(['Note: saving range ambiguity functions '...
				      'to file ' wr_file ' ...'])
				eval(['save ' wr_file '  ' wr_name])
			end
		end

	else
		%% Global range ambiguity function information is available  ...
		disp(['Note: Using stored lag profile information ..... ' ...
		      'to start again:'])
		disp(['****: Include the argument ' ...
		      '''clear'' amongst the input arguments.'])
		disp(['****: Or, remove the file ' wr_file])
	end
	
	%% Subsets of the arrays lpg_dt, lpg_nt and lpg_h corresponding
	%% to the signal lag profile groups. We will use these to determine
	%% the natural time limits ...
	master_dt = lpg_dt(all_lpgs);
	master_nt = lpg_nt(all_lpgs);
	master_h  = lpg_h(all_lpgs);

	%% Time limits of plot. Units are not in us till multiplied by p_dtau
	all_tlim = [0 max(master_h + (master_nt.*master_dt))]' .* p_dtau;

	texthandles = [];	%% For subsequent alterations to lpg nos.
	font_name   = 'Helvetica';	%% Font details ...
	font_size   = 10;
	font_wght   = 'Normal';


	if ((nargin==0) | (nargin==1 & clean==1))

		%% The solitary argument 'clean' is the same as not argument ...
		lpgs = all_lpgs;
		tlim = all_tlim;

		npanels   = 1;
		panels    = 0;
		timecheck = size(tlim,1);

	else

		%% Parse the input arguments ...
		j=1; l=[]; t=[];
		while (j <= nargin)
			if  eval(['strcmp(arg' int2str(j) ',''panel'')'])
				%% Each panel specified separately ...
				j = j+1; if (j+1 > nargin)
					error('Usage of (2)')
				else
					if eval(['isempty (arg' int2str(j) ')'])
						l = gup_pad(l,inf,inf);
					else
						eval(['l=gup_pad(l,arg' int2str(j) ''',NaN);'])
					end
					j = j+1;
					if eval(['isempty (arg' int2str(j) ')'])
						t = gup_pad(t,ones(2,1)*inf);
					else
						eval(['t=gup_pad(t,arg' int2str(j) ''',NaN);'])
					end
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
					error('Usage of Fontweight')
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

			elseif  eval(['strcmp(arg' int2str(j) ',''limit'')'])
				%% The flag has already been set...
				j = j+1;

			end
			j = j + 1;
		end

		lpgs = l;
		tlim = t;

		%% A) Do lpgs and tlim correspond to the same number of panels?
		if (size(lpgs,2) ~= size(tlim,2))
			error(['Number of columns in lpg array ' ...
			      'must equal number of columns in time matrix'])
		end
		npanels = size(lpgs,2);

		%% B) Either start and end time can be given, or all of
		%% start time, end time, major and minor tickmark spacing
		%% and label spacing. Defaults are provided if start time
		%% and end time are provivded. Otherwise all must be given.
		%% Any value of 'Inf' should by now have been replaced with
		%% the corresponding limit value .....
		timecheck   = size(tlim,1);
		if (timecheck < 2)
			error(['Start and end time for each panel '  ...
			      'must be specified'])
		elseif ((timecheck==3) | (timecheck > 4))
			warning(['Either supply only start and end times,' ...
			 'or start and end time plus major and minor ' ...
			 'tickmark spacing (no other combination)'])
			tlim(3:timecheck,:) = [];
			timecheck   = size(tlim,1);
		end

		%% C) Replace the value 'Inf' with the corresponding defaults.
		%%    When the lpg numbers have been finalised, a limited set may
		%%    be calculated and stored in wr_NAME. When the set of lpgs to
		%%    be stored is known, the default time limits may be revised
		%%    if they are inappropriate for the ambiguity functions shown
		if any(any(isinf(lpgs)))
			%% I) Replace any column of the lpg number matrix
			%%    containing an 'inf' with the default set ...
			if (size(lpgs,1)==1)
				infset =  isinf(lpgs);
				notinf = ~isinf(lpgs);
			else
				infset =  any(isinf(lpgs));
				notinf = ~any(isinf(lpgs));
			end
			if any(infset)
				r1  = size(lpgs,1);
				c1  = size(lpgs,2);
				r2  = size(all_lpgs,1);
				tmp = zeros(r2,c1);
				tmp(1:r1,1:c1) = lpgs;
				tmp(:,infset)  = ...
				    dupcol(all_lpgs,length(find(infset)));
				if any(notinf)
				    tmp(r1+1:r2,notinf) = ...
					dupcol(duprow(NaN,r2-r1),length(find(notinf)));
				end
			end
			lpgs = tmp;
		end

		list = lpgs(~isnan(lpgs(:)));	%% All lpg numbers ...

		%% In case i) wr_file was not available AND  ii) limit==1
		%% recreate wr_NAME such that it only contains a limited set.
		%% This is done here so that any Inf values supplied  as lpg
		%% arguments have been replaced with the defaults, and before
		%% the time axis is clipped to the maximum for a limited set
		%% of range ambiguity functions, and also so that the settings
		%% are valid for all panels. The limited set is NOT saved.
		%% If a complete set of raf's is available from wr_file, then
		%% wr_NAME is "complete" so that limit should be set to 0.
		if (limit==1)
			if eval(['(exist(''' wr_file ''')==2)'])
				disp(['Note: No need to limit ' wr_name ...
				      ' since ' wr_file ' is available  ...'])
				disp(['Note: Loading ' wr_file ' ...'])
				load(wr_file)
				limit=0;	%% No further need for this ...
			else			
				disp(['Note: Clearing variable ' wr_name ' ...'])
				eval([wr_name '=[];'])
				total = length(list);
				disp('**** Limited lpg range ambiguity function set ...')
				k=0; for i = list'
					k = k + 1;
					disp(['Profile ' int2str(k)                 ...
					      ' (Lag profile group ' int2str(i) ')' ...
					      ' out of ' int2str(total) ' .....'])
					%% Takes forever if stored directly as a
					%% sparse array, so store as a full one
					%% instead (See documentation to gup_pad.m)
					eval(['wr_' name_expr ...
					      ' = gup_pad(wr_' name_expr ',wrlpg(i),0);'])
				end
			end
		end

		if any(any(isinf(tlim)))	%% Time limits ...
			%%  II) Replace 'inf' lower limit with all_tlim(1) ...
			tmp = find(isinf(tlim(1,:)));
			if ~isempty(tmp)
				tlim(1,tmp) = dupcol(all_tlim(1),length(tmp));
			end

			%% III) Replace 'inf' upper limit with all_tlim(2) ...
			%% If any panel does not contain the full set of signal
			%% and the uper time limit is set to Inf, revise the
			%% variable all_tlim before using it as a default.
			if (any(tlim(2,:)==inf))
				if  (length(list)<length(all_lpgs))
					ti = 0;
					tf = max(lpg_h(list) + (lpg_nt(list).*lpg_dt(list)));
					all_tlim = [ti tf]' .* p_dtau;
				end
			end
			%% Now replace any Inf elements with the corresponding
			%% "all_tlim" value ..................................
			tmp = find(isinf(tlim(2,:)));
			if ~isempty(tmp)
				tlim(2,tmp) = dupcol(all_tlim(2),length(tmp));
			end

			%%  IV) Replace 'inf' tickmarkangaben with NaN's ...
			if (timecheck >2)
				tmp = find(isinf(tlim(3,:)));
				if ~isempty(tmp)
					tlim(3,tmp) = dupcol(NaN,length(tmp));
				end
				%% V)   Replace 'inf' tickmarkangaben with NaN's ...
				tmp = find(isinf(tlim(4,:)));
				if ~isempty(tmp)
					tlim(4,tmp) = dupcol(NaN,length(tmp));
				end
			end
		end

		%% d) Sort out invalid channel and time limits. Applies
		%% to genuine user input. If the user supplied both times
		%% and lpg numbers, the default values will not have been
		%% calculated, and so need to be calculated here.
		togo = [];
		for i = 1 : npanels
			testlpg = lpgs(:,i);
			testlpg(find(isnan(testlpg)))=[];

			%% Check each panel's lpg information ...
			if any(~any(dupcol(all_lpgs,length(testlpg))==duprow(testlpg',length(all_lpgs))))
				warning(['Lag profile group selection ' int2str(i) ' is invalid'])
				togo = [togo i];	%% Any columns of go?

			%% Check each panel's initial time limit ...
			%% The upper time limit is not strictly enforced
%			elseif ((tlim(1,i) < all_tlim(1)) | (tlim(2,i) > all_tlim(2)))
			elseif (tlim(1,i) < all_tlim(1))
				warning(['Time limits ' num2str(tlim(1,i)) ' and ' num2str(tlim(2,i)) ' are invalid'])
				togo = [togo i]; 	%% Any columns to go?
			end
		end
		if ~isempty(togo)
			npanels = npanels - length(togo);
			if (npanels < 1)
				error('no valid input information')
			else
				lpgs(:,togo) = [];
				tlim(:,togo) = [];
			end
		end

		%% Space for panels handles ...
		panels = zeros(npanels,1);
	end

	%% If only start and end times are given, supply the rest.
	%% If a times array is given which is gup_padded out with NaNs
	%% then timecheck will not detect that fact. So run all
	%% time arrays through this loop..........................
	tstore = tlim;
	tlim  = zeros(4,npanels);
	for i = 1 : npanels
		temp1 = tstore(:,i);
		temp2 = temp1(~isnan(temp1)); 
		if (size(temp2,1) < 4)
			tdiff = temp2(2)-temp2(1);
			if     (tdiff > 1000000)
				dt1 = 100000;
				dt2 =  20000;
			elseif (tdiff > 100000)
				dt1 = 5000;
				dt2 = 1000;
			elseif (tdiff > 50000)
				dt1 = 2000;
				dt2 =  500;
			elseif (tdiff > 1000)
				dt1 = 500;
				dt2 = 100;
			else
				dt1 = 100;
				dt2 =  20;
			end
			tlim(:,i) = [temp2;dt1;dt2];
		else
			tlim(:,i) = temp1;
		end
	end

	if (limit==1), special=0; end	%% Unfortunate measure ...

	%% 2) Plot the picture ....
	goodgap = 0.04;
	figure(gcf), clf, set(gcf,'Resize','On','Name','plot_wr')
	for i = 1 : npanels

		%% Lpgs to be plotted in this panel ...
		panellpgs = lpgs(:,i);
		panellpgs = panellpgs(~isnan(panellpgs));
		panel_ind = find(panellpgs);

		%% Time information pertaining to this panel ...
		paneltlim = tlim(:,i);
		paneltlim = paneltlim(~isnan(paneltlim));

                %% Underlying time units .....
                tgrid   = (paneltlim(1):paneltlim(2))';

                %% Used for placing major tickmarks .....
                tmajor  = tgrid(find(rem(tgrid,paneltlim(3))==0));

                %% Used for placing minor tickmarks .....
                tminor  = tgrid(find(rem(tgrid,paneltlim(4))==0));

		%% Time axis labels ..... in ms, not us.
		xlabels = makelabels(tmajor/1000);			

		%% panel_lpg: all chosen lpg ambiguity functions........
		%% panel_dt : corresponding gating intervals
		%% panel_nt : corresponding number of gates
		%% panel_h  : range (time units) of middle of amb func.
		ind=[]; for k = 1:length(panellpgs)
			ind = [ind ; find(all_lpgs==panellpgs(k))];
		end
		panel_dt   = master_dt(ind)';
		panel_nt   = master_nt(ind)';
		panel_h    = master_h(ind)';
		if (limit==1)
			%% We have to search within the limited wr_NAME ...
			%% This happens in case a limited set was desired
			%% and the whole set was not available from file.

			% PROBLEM: LIST MAY CONTAIN DOUBLE REFERENCES
			% IN WHICH CASE find(list==panellpgs(k)) GETS
			% TOO MANY REFERENCES AND SPOILS THE SPACING.
			% USE THIS BODGED SOLUTION, SINCE WE MUST USE
			% AN INDEXING METHOD SIMILAR TO THAT USED FOR
			% THE FULL SET!
			% ind=[]; for k = 1:length(panellpgs)
			%	ind = [ind ; find(list==panellpgs(k))];
			% end

			ind=[]; ind = special + find(panellpgs); special=max(ind);
			eval(['panel_lpg  = ' wr_name '(:,ind);']);
		else
			%% We are using all signal lpgs ==> can use ind ...
			%% This can have happened even if limit=1 initially.
			eval(['panel_lpg  = ' wr_name '(:,ind);']);
		end

		%% Work out the spacings on the unseen y-scale .........
		ygap = ceil(min(max(panel_lpg)));
		if (ygap > 50), ygap=50; end
		ymin = 0;
		ymax = 	sum(ceil(max(panel_lpg)))       + ...
			abs(sum(floor(min(panel_lpg)))) + ...
			((size(panel_lpg,2)+1) * ygap);

		%% Specify the tickmark lengths ....
		mjlen = ymax * 0.02;	%% Major tick marks
		mnlen = ymax * 0.01;	%% Minor tick marks
		mtlen = ymax * 0.01;	%% Gating

		%% Add on a bit more than the height of the major
		%% tick marks on to 'ymax'. Plotting will start at
		%% ymin + (ymax*0.025) so that the first ambiguity
		%% function in each panel is not obscured by major
		%% tick marks ...
		incr = full(ymax*0.025);
		ymax = full(ymax + incr);

		%% Major tick marks ..... in ms
		gap = paneltlim(3)/1000;
		xmj = duprow(dupcol(min(tmajor)/1000 : gap : max(tmajor)/1000 ,2),2)*1000;
		ymj = dupcol2([ymin ymax ; ymin+mjlen  ymax-mjlen], length(tmajor));

		plot_minor_tickmarks = (paneltlim(4) < paneltlim(3));

		%% Minor tick marks ..... in ms
		if (plot_minor_tickmarks)
			gap = paneltlim(4)/1000;
			xmn = duprow(dupcol(min(tminor)/1000 : gap : max(tminor)/1000,2),2)*1000;
			ymn = dupcol2([ymin ymax ; ymin+mnlen  ymax-mnlen], length(tminor));
		end

		dx = (1-((npanels+1)*goodgap))/npanels;
		x0 = (i-1)*dx + i*goodgap;
		panels(i) = axes(...
		  'Units','normal',...
		  'Position',[x0 2*goodgap dx 1-(3*goodgap)],...
        	  'Box','On',...
		  'Visible','On',...
		  'Fontname',font_name,...
		  'Fontsize',font_size,...,
		  'Fontweight',font_wght,...
        	  'TickLength',[0 0],...
		  'Xtick',tmajor,...
		  'Xlim',[paneltlim(1) paneltlim(2)],...
		  'Ylim',[ymin ymax],...
		  'Xticklabels',str2mat(xlabels),...
		  'YTickLabels',[]);

		line(xmj,ymj,'Color',white)
		if (plot_minor_tickmarks)
			line(xmn,ymn,'Color',white)
		end

		%% Start plotting slightly above the x-axis ....
		yt=[]; y=ymin + incr;
		for k = panel_ind'
			%% The relevant ambiguity function .....
			w = full(panel_lpg(:,k));

			%% The function limits along the x-axis .....
			x = find(w~=0);
			xmin = min(x);
			xmax = max(x);

			%% The corresponding parts of x and w .....
			x = (xmin : xmax)';
			w = w(xmin : xmax);

			%% The y-coordinate on the plot .....
			y = y + ygap + abs(floor(min(w)));
			if (k>1), y = y + ceil(max(panel_lpg(:,k-1))); end

			%% Record this ylevel as one of the y-axis
			%% major tick marks (in case a grid should
			%% be desired.
			yt = [yt y];

			%% Local tick marks indicating gating .....
			tmp=(panel_h(k) + ((0:panel_nt(k)) * panel_dt(k)));
			tmp(find(tmp > paneltlim(2))) = [];
			xtm = duprow(tmp,2); lenxtm = size(xtm,2); clear tmp
			ytm = dupcol([y-mtlen ; y],lenxtm);

			%% The patch has the shape of the function .....
			patch([x; min(x)],[y+w; y+w(1)],white);

			%% The line is to make the time limits clear .....
			line([xmin xmax],[y y],'Linestyle','-','Color',white)

			%% Local gating tick marks .....
			line(xtm,ytm,'Linestyle','-','Color',white)

			%% The text gives the lag profile group number .....
			t=text(	max(x(1),  ...
			    paneltlim(1)+(paneltlim(2)-paneltlim(1))*0.033),...
			    y,						    ...
			    int2str(panellpgs(k)));
			set(t,'VerticalAlignment','Baseline',...
			      'HorizontalAlignment','Right' ,...
			      'Fontsize',font_size,...
			      'Fontweight',font_wght, ...
			      'Fontname',font_name);
			texthandles = [texthandles t];
			xlabel('ms')
		end
		%% Useful in case the grid is put on ...
		set(gca,'Ytick',yt)
	end

	%% Store the panels handles for subsequent use ....
	disp('Note: The axes'' handles are stored in figure''s "UserData"')
	disp('****: Use these to make further modifications to the figure')
	disp('****: Use the returned texthandles to modify the lpg numbers')
	set(gcf,'UserData',panels);

%end
