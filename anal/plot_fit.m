%	function plot_fit(arg1,arg2,arg3,...)
%
% Function for plotting the results of the GUISDAP fitting process. 
% plot_fit reads GUISDAP results file(s), and plots any of the fitted
% parameters Ne, Te, Te/Ti, [O+] and Vi, each in a separate panel. Files
% can be named explicitly in the argument list, otherwise data is read
% from the workspace.  Parameters are specified using a mask of 1's and
% 0's and the order of the parameters is as above.  Missing elements at
% the end of this mask are provided by the programme. 
%
% Example 1:
%	plot_fit('panel',[1 1 1 0 1],[])
%
% takes GUISDAP result data from the workspace, and plots the fit values
% of Ne, Te, Te/Ti and Vi for all heights at which data was analysed.
% Data is found on the "workspace" if:
%	1) a results file has been explicitly loaded OR 
%	2) after one set of data has been fitted and
%	   before the next set has been fitted!
%
% Example 2:
%	plot_fit('panel',[1 1 1],[400 800 25])
%
% plots the fit values of Ne,Te, and Te/Ti inside the height range 400-800
% km with tickmarks every 25 km.  A default tick mark spacing is provided,
% so this element of arg2 is optional. 
%
% Example 3:
%	plot_fit('panel',[],[400 800],'files','file.mat')
%
% plots all parameters in the range 400-800 km with default tickmark
% spacing, and data taken from "file.mat". 
%
% Example 4:
%	plot_fit('panel',[],[],'files','file1.mat','file2.mat')
%
% plots all parameters over the maximum height range with default spacing,
% and data is taken from the 2 files specified.  The data from each is
% specified in a different colour. 
%
% The keywords 'Fontname', 'Fontsize', and 'Fontweight' can be used in the
% input arguments to specify the font used for he axis text.  The keyword
% 'Markersize' is used to specify the size of the marker used for each
% value.  The default marker size is 4.  The default font is Times-Roman,
% 10pt, Normal weight.  Other fontnames are for example Helvetica,
% Palatino, Courier, Schumacher, Lucida ... 
%
% Example 4:	%% Set the font details explicitly ...
%	plot_fit('Fontname','Helvetica',...
%		 'Fontsize',11,	   	...
%		 'Fontweight','Bold',	...
%		 'MarkerSize',7);
%
% Example 5:	%% Switching off the text output ...
%	plot_fit(...,'NoText',...);
%
% If several files' data are to be plotted, then all points are plotted
% in the same colour (for speed) unless the option 'colour' is specifed
% in which case each file's data comes up in a different colour. The
% latter is also slower.
%
% Example 6:
%	plot_fit(...,'Colour',...);
%
% Additional modifications to the plot can be made directly by accessing
% individual panels' handles, which are stored in the figure's UserData
% field e.g. 
%	for i=get(gcf,'UserData'), axes(i), grid on, end
%
% To create output with aspect ratio 2, do, for example,
%	for i=get(gcf,'UserData'),axes(i),set(gca,'AspectRatio',[2 NaN]),end
%
% To obtain a hardcopy use the MATLAB "print" command. Do not make
% your own alterations. In the event of difficulty, contact:
%	nickgjg@eiscat.ag.rl.ac.uk	OR	n.gazey@rl.ac.uk

function plot_fit(arg1,  arg2,  arg3,  arg4,  arg5,  ...
		  arg6,  arg7,  arg8,  arg9,  arg10, ...
		  arg11, arg12, arg13, arg14, arg15, ...
		  arg16, arg17, arg18, arg19, arg20)

global r_param r_error r_dp r_status r_h ch_el d_time
lr=size(r_param,2);
	

global name_expr name_site
if sum(get(gcf,'color'))>2
 white='k'; 
else
 white='w'; 
end

%% Except for composition, the order of the other
%% fitted parameters is as in the array r_param.
%% If composition is desired, the information in
%% the 4th column of r_param is replaced by the
%% composition information, and the various masks
%% are used to access the correct column(s)......
NE = [1 0 0 0 0];	%% Electron density
TI = [0 1 0 0 0];	%% Ion temperature
TR = [0 0 1 0 0];	%% Te/Ti
CP = [0 0 0 1 0];	%% Composition
VI = [0 0 0 0 1];	%% Ion velocity
maxpars = 5;
labels  = str2mat('N_e/10^{11}',...
		  'T_i',...
		  'T_e/T_i',...
		  '[O^+]/N_e',...
		  'V_i');
colours = ['y'; 'm'; 'c'; 'r'; 'g'; 'b'; white];
mask = [];
hght = [];
file = [];
font_name = 'Times';	%% Default font ...
font_size = 10;		%% Default font size ...
font_wght = 'Normal';	%% Default font weight ...
marker_size = 4;	%% Default marker size ...
write_text  = 1;	%% Whether text is written ...
colour = 0;		%% Faster, plain colour option is the default ...

if (nargin==0)

	mask    = ones(1,maxpars);
	%% hght array not specified yet ...

	npanels = maxpars;
	panels  = zeros(npanels,1);
	nfiles  = 1;
	use_ws  = 1;	%% Use workspace variables

else

	%% Parse the input arguments ...
	j=1; m=[]; h=[]; f=[];
	while (j <= nargin)
		if  eval(['(strcmp(arg' int2str(j) ',''panel'')' ...
			' | strcmp(arg' int2str(j) ',''Panel''))'])
			%% Each panel specified separately ...
			j = j+1; if (j+1 > nargin)
				error('Usage of (1)')
			else
				eval(['m=arg' int2str(j) ';'])
				j = j+1;
				eval(['h=arg' int2str(j) ''';'])
			end

		elseif eval(['(strcmp(arg' int2str(j) ',''files'')' ...
			   ' | strcmp(arg' int2str(j) ',''Files'')' ...
			   ' | strcmp(arg' int2str(j) ',''file'')' ...
			   ' | strcmp(arg' int2str(j) ',''File''))'])
			%% Files to be read ...
			j = j+1;  while (j <= nargin)
				if (j > nargin)
					break
				else
					eval(['f=str2mat(f,arg' int2str(j) ');'])
				end
				j = j+1; if (j <= nargin)
					if eval(['(isstr(arg' int2str(j) ') ' 			...
						'& (strcmp(arg' int2str(j) ',''Files'') '	...
						' | strcmp(arg' int2str(j) ',''Fontsize'') '	...
						' | strcmp(arg' int2str(j) ',''FontSize'') '	...
						' | strcmp(arg' int2str(j) ',''fontsize'') '	...
						' | strcmp(arg' int2str(j) ',''Fontweight'') '	...
						' | strcmp(arg' int2str(j) ',''FontWeight'') '	...
						' | strcmp(arg' int2str(j) ',''fontweight'') '	...
						' | strcmp(arg' int2str(j) ',''Fontname'') '	...
						' | strcmp(arg' int2str(j) ',''FontName'') '	...
						' | strcmp(arg' int2str(j) ',''fontname'') '	...
						' | strcmp(arg' int2str(j) ',''Notext'') '	...
						' | strcmp(arg' int2str(j) ',''NoText'') '	...
						' | strcmp(arg' int2str(j) ',''notext'') '	...
						' | strcmp(arg' int2str(j) ',''Markersize'') '	...
						' | strcmp(arg' int2str(j) ',''MarkerSize'') '	...
						' | strcmp(arg' int2str(j) ',''markersize'') '	...
						' | strcmp(arg' int2str(j) ',''Colour'') '  	...
						' | strcmp(arg' int2str(j) ',''colour'') '  	...
						' | strcmp(arg' int2str(j) ',''Color'') '  	...
						' | strcmp(arg' int2str(j) ',''color'') '  	...
						' | strcmp(arg' int2str(j) ',''Panel'') '	...
						' | strcmp(arg' int2str(j) ',''panel'')))'])
							j = j-1;
							break
					end
				end
			end
			%% Note that str2mat() gives "file" an extra
			%% row! Get rid of it here before it disrupts
			%% later on.
			f(1,:) = [];

		elseif  eval(['(strcmp(arg' int2str(j) ',''Colour'')' ...
			    ' | strcmp(arg' int2str(j) ',''colour'')' ...
			    ' | strcmp(arg' int2str(j) ',''color'')' ...
			    ' | strcmp(arg' int2str(j) ',''Color''))'])
			%% Flag to allow the slower colour plotting version ...
			colour = 1;

		elseif  eval(['(strcmp(arg' int2str(j) ',''Markersize'')' ...
			    ' | strcmp(arg' int2str(j) ',''MarkerSize'')' ...
			    ' | strcmp(arg' int2str(j) ',''markersize''))'])
			%% Set the fontsize explicitly ...
			j = j+1; if (j > nargin)
				error('Usage of Markersize')
			else
				eval(['marker_size=arg' int2str(j) ';'])
			end

		elseif  eval(['(strcmp(arg' int2str(j) ',''NoText'')' ...
			    ' | strcmp(arg' int2str(j) ',''Notext'')' ...
			    ' | strcmp(arg' int2str(j) ',''notext''))'])
			write_text=0;

		elseif  eval(['(strcmp(arg' int2str(j) ',''Fontsize'')' ...
			    ' | strcmp(arg' int2str(j) ',''FontSize'')' ...
			    ' | strcmp(arg' int2str(j) ',''fontsize''))'])
			%% Set the fontsize explicitly ...
			j = j+1; if (j > nargin)
				error('Usage of Fontsize')
			else
				eval(['font_size=arg' int2str(j) ';'])
			end

		elseif  eval(['(strcmp(arg' int2str(j) ',''Fontweight'')' ...
			    ' | strcmp(arg' int2str(j) ',''FontWeight'')' ...
			    ' | strcmp(arg' int2str(j) ',''fontweight''))'])
			%% Set the fontweight explicitly ...
			j = j+1; if (j > nargin)
				error('Usage of Fontweight')
			else
				eval(['font_wght=arg' int2str(j) ';'])
			end

		elseif  eval(['(strcmp(arg' int2str(j) ',''Fontname'')' ...
			    ' | strcmp(arg' int2str(j) ',''FontName'')' ...
			    ' | strcmp(arg' int2str(j) ',''fontname''))'])
			%% Set the fontname explicitly ...
			j = j+1; if (j > nargin)
				error('Usage of Fontname')
			else
				eval(['font_name=arg' int2str(j) ';'])
			end

		end
		j = j + 1;
	end

	mask = m;
	hght = h;
	file = f;

	%% Check parameter mask ...
	if isempty(mask)
		mask = ones(1,maxpars);
		disp('Note: Supplying default parameter mask .....')

	elseif (length(mask) > maxpars)
		%% Truncate to maximum mask length ...
		mask(maxpars+1:length(mask)) = [];
		disp(['Note: Using parameter mask ' int2str(mask) ' .....'])

	elseif (length(mask) < maxpars)
		%% Fill out missing elements ...
		mask(length(mask)+1:maxpars) = zeros(1,maxpars - length(mask));
		disp(['Note: Using parameter mask ' int2str(mask) ' .....'])
	end
	npanels = length(find(mask));
	panels  = zeros(npanels,1);

	%% Check that any specified files exist ... there may
	%% be no data on the workspace, so do this first.  If
	%% the files exist, assume initially that they are valid.
	if ~isempty(file)
		togo=[]; for k = 1:size(file,1)
			if (fopen(canon(file(k,:),0)) < 0)
				warning(['Cannot open ' file(k,:) '... '])
				togo = [togo k];
			end
		end
		if ~isempty(togo), file(togo,:)=[]; end
	end
	%% If invalid files were specified, or no files were
	%% specified at all, then we use workspace variables
	if isempty(file)
		use_ws = 1;
		nfiles = 1;
	else
		use_ws = 0 ;
		nfiles = size(file,1);
	end

	%% Check the height array dimensions. Actual values
	%% are checked and default values given only after
	%% the initial argument checking loop, once it is
	%% clear whence the variables are to come.
	if ~isempty(hght)
		hghtcheck1 = size(hght(~isnan(hght)),1);
		if (hghtcheck1 < 2)
			error(['Upper and lower height ' ...
			      'limits for each panel must be specified'])
		elseif (hghtcheck1 > 3)
			warning(['Programme requires only upper and lower ' ...
			 'height limits, and an optional tickmark spacing'])
			hght(3:size(hght,1),:) = [];
		end
		hghtcheck2  = size(hght,2);
		if (hghtcheck2 > 1)
			disp('Note: height array should be 1-D ...')
			hght(:,2:hghtcheck2) = [];
		end
	end
end	%% End of argument checks .... see below for hght check

%% If data comes from files, construct the data variables first
if (~use_ws)
	disp('Note: Obtaining data variables from specified file(s)')
	param=[]; err=[]; status=[]; h=[]; dp=[]; el=[]; time=[];
	name=[]; site=[];
	for k = 1 : nfiles
		load(canon(file(k,:),0))
		param  = pad(param,r_param,NaN);
		err    = pad(err,r_error(:,1:lr),NaN);
		status = pad(status,r_status,NaN);
		h      = pad(h,r_h,NaN);
		dp     = pad(dp,r_dp,NaN);
		el     = pad(el,r_el,NaN);
		time   = [time;r_time];
		name   = str2mat([name; name_expr]);
		site   = str2mat([site; name_site]);
	end
else
	if (isempty(r_param) | isempty(r_error) | isempty(r_status) | ...
	    isempty(r_h)     | isempty(r_dp)    | isempty(d_time))
		disp('Note: There are no result variables on the workspace')
		return
%	else
%		disp('Note: Using variables on the workspace')
	end
	param  = r_param;
	err    = r_error(:,1:lr);
	status = r_status;
	h      = r_h;
	dp     = r_dp;
	el     = ch_el(1);
	time   = d_time;
	name   = name_expr;
	site   = name_site;
end

%% Check that variables have been declared as global outside
%% this function i.e. whether they are really available!
if (isempty(param) | isempty(err)  | isempty(dp) | isempty(status) | ...
    isempty(h) | isempty(el) | isempty(d_time))
	disp('Error: (Global) variable set incomplete .....')
	disp('*****: Check r_param,r_error,r_status,r_dp,r_h,ch_el & d_time')
	return
end
check = size(param,1);	%% Check their dimensions ....
if ((size(err,1)   ~= check) | ...
    (size(h,1)     ~= check) | ...
    (size(status,1)~= check) | ...
    (size(dp,1)    ~= check))
	error(['r_error, r_h, r_status, r_dp and r_param ' ...
	      'must have the same no. rows']) 
end
%% Scale NE if necessary ... and replace fourth column of r_param
%% with composition information if the latter is desired ....
if (use_ws)
	if any(mask & NE)
		param(:,1) = param(:,1)/1e11;
		err(:,1) = err(:,1)/1e11;
	end
	if any(mask & CP)
		%% Replace fourth column (coll. freq.)
		%% with composition information.
		param(:,4) = dp;
		err(:,4) = duprow(NaN,length(dp));
	end
else
	if any(mask & NE)
		ind = dupcol2(1,nfiles,5);
		param(:,ind) = param(:,ind)/1e11;
		err(:,ind) = err(:,ind)/1e11;
	end
	if any(mask & CP)
		%% Replace fourth column (coll. freq.)
		%% with composition information.
		ind = dupcol2(4,nfiles,5);
		[r,c] = size(dp);
		param(:,ind) = dp;
		err(:,ind) = ones(r,c).*NaN;
	end
end

%% Round altitude limits up and down to nearest 10 km ...
tmp = r_h(~isnan(r_h));
hi  = min(min(tmp)); hi = hi - rem(hi,10);
hf  = max(max(tmp)); hf = hf - rem(hf,10) + 10;
all_hght = [hi hf]';

%% Supply default height values here, in case hght is
%% still empty. Otherwise, sieve out Inf's and replace
%% with the respective maximum/minimum height limit.
if isempty(hght)

	hght = all_hght;

elseif any(any(isinf(hght)))

	[hghtcheck1,hghtcheck2] = size(hght);

	%% 1) Replace 'inf' lower limit with all_hght(1) ...
	tmp = find(isinf(hght(1,:)));
	if ~isempty(tmp)
		hght(1,tmp) = dupcol(all_hght(1),length(tmp));
	end
	%% 2) Replace 'inf' upper limit with all_hght(2) ...
	tmp = find(isinf(hght(2,:)));
	if ~isempty(tmp)
		hght(2,tmp) = dupcol(all_hght(2),length(tmp));
	end
	%% 3) Replace 'inf' tickmarkangaben with NaN's ...
	if (hghtcheck1 > 2)
		tmp = find(isinf(hght(3,:)));
		if ~isempty(tmp)
			hght(3,tmp) = dupcol(NaN,length(tmp));
		end
	end
end

if (length(hght(~isnan(hght))) < 3)
	hdiff = hght(2)-hght(1);
	if (hdiff > 1000)
		dh1 = 500;
	elseif (hdiff > 100)
		dh1 = 200;
	elseif (hdiff > 10)
		dh1 = 20;
	else
		dh1 = 2;
	end
	hght = [hght(1);hght(2);dh1];	
end

%% Determine which elements of each matrix are real fits
%% and the corresponding ranges. If data is pulled from
%% files, then the fits in each file, and the corresponding
%% range values may all be different.
if (use_ws)
	%% Indices of fits and no-fits ...
	fits   = find(status==0 & h>=hght(1) & h<=hght(2));
	nofits = find(status==1 & h>=hght(1) & h<=hght(2));
	%% Heights of fitted and unfitted data ...
	hf = h(fits);
	hn = h(nofits);
	%% Data ... (all)
	err_plus  = param + err;
	err_minus = param - err;
else
	fits   = find(status==0 & h>=hght(1) & h<=hght(2));
	nofits = find(status==1 & h>=hght(1) & h<=hght(2));
	%% Heights of fitted and unfitted data ...
	hf = h(fits);
	hn = h(nofits);
	%% Data ... (all)
	err_plus  = param + err;
	err_minus = param - err;
end

%% Underlying range units .....
hgrid   = (hght(1):hght(2))';

%% Used for placing range major tickmarks .....
hmajor  = hgrid(find(rem(hgrid,hght(3))==0));

%% Range labels ... in km.
hlabel = makelabels(hmajor);

%% Now plot the picture
goodgap = 0.05;
gupfigure(gcf), clf, set(gcf,'Resize','On','Name','plot_fit')


if (~use_ws)
	%% Since we cannot use sparse matrices to index
	%% other arrays, input from several files is
	%% handled quickly by decomposing the "param"
	%% array into a single column matrix, and finding
	%% the right indices, knowing that param has lr
	%% columns, and what the desired parameters are.
	q  = size(status,1);
	%% 1) Fits ...................
	fm = remainder(fits,q);
	fn = (fits-fm)/q;
	foo = fn*lr*q + fm;
	%% 2) No-fits ................
	nm = remainder(nofits,q);
	nn = (nofits-nm)/q;
	noo = nn*lr*q + nm;
end	

%% Step through the different parameters ...
for i = 1 : npanels

	index = find(mask);
	panelmask = zeros(1,maxpars);
	panelmask(index(i)) = 1;

	dx = (1-goodgap-(npanels*0.75*goodgap))/npanels;
	x0 = (1.25*goodgap) + (i-1)*dx + (i-1)*goodgap*0.75;
	y0 = (nfiles+4)*goodgap*0.5;
	dy = 1- y0 -(3*goodgap*0.5);
	panels(i) = axes(...
	  'Units','normal',...
	  'Position',[x0 y0 dx dy],...
	  'Box','On',...
	  'Visible','On');

	if (use_ws)
		%% Desired parameter in column index(i) ...
		%% Fitted values ...
		fpar   = param(fits,index(i));
		fupper = err_plus(fits,index(i));
		flower = err_minus(fits,index(i));
		%% Unfitted values ...
		npar   = param(nofits,index(i));
		nupper = err_plus(nofits,index(i));
		nlower = err_minus(nofits,index(i));
		%% The plot ...
		p = plot(fpar,hf,[white 'o'],[flower fupper]',[hf hf]',[white '-'], ...
			 npar,hn,[white '*'],[nlower nupper]',[hn hn]',[white '-']);
		set(p,'Markersize',marker_size);
	else
		%% Use "foo" and "noo" to take into account
		%% the particular parameter being plotted.
		fo = foo + (index(i)-1)*q;
		no = noo + (index(i)-1)*q;
		if (colour==1)
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			%% This does a slow job but nevertheless plots
			%% each file's values in a different colour.
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			col = unique(nn); for c=col'
				hnn    = hn(nn==c);
				npar   = param(no(nn==c));
				nupper = err_plus(no(nn==c));
				nlower = err_minus(no(nn==c));
				colname = colours(c+1);
				eval(['v =''' colname 'o'';'])
			        eval(['r =''' colname '*'';'])
				eval(['l =''' colname '-'';'])
				p = plot(npar,hnn,r,[nlower nupper]',[hnn hnn]',l);
				set(p,'Markersize',marker_size)
				hold on
			end
		else
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			%% This treats the whole parameter matrix as a
			%% single column and plots all fits/no-fits
			%% together, regardless of which file they came
			%% from. This is MUCH quicker than the alternative
			%% .... but everything comes out in the same colour.
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			fpar   = param(fo);
			fupper = err_plus(fo);
			flower = err_minus(fo);
			npar   = param(no);
			nupper = err_plus(no);
			nlower = err_minus(no);
			%% The plot ................
			col = rem(1,length(colours));
			if (col==0), col=length(colours); end
			colname = colours(col);
			eval(['v =''' colname 'o'';'])
			eval(['r =''' colname '*'';'])
			eval(['l =''' colname '-'';'])
			p = plot(fpar,hf,v,[flower fupper]',[hf hf]',l, ...
				 npar,hn,r,[nlower nupper]',[hn hn]',l);
			set(p,'Markersize',marker_size)
		end
	end

	set(gca,'ytick',hmajor,			...
		'ylim',[hght(1) hght(2)],	...
		'Yticklabel',[],		...
		'Fontname',font_name,		...
		'Fontsize',font_size,		...
		'Fontweight',font_wght)

	if any(panelmask & TR)	%% Line indicating Te/Ti = 1 ...
		line([1 1]',[hght(1) hght(2)], ...
		     'LineStyle',':',          ...
		     'Color',white)
	end
	if any(panelmask & VI)	%% Line indicating zero velocity ...
		line([0 0]',[hght(1) hght(2)], ...
		     'LineStyle',':',          ...
		     'Color',white)
	end

	title(labels(index(i),:))
end

axes(panels(1)), set(gca,'Yticklabel',hlabel)

%% Store the panels handles for subsequent use ....
if sum(get(gcf,'UserData'))~=4
	disp(['Note: Individual panel handles are obtained using: ' ...
	'get(gcf,''UserData'')'])
	set(gcf,'UserData',panels');	%% ROW OF PANEL HANDLES ...
end

%% Put time string(s) in, indicating when dump was made ...
axes('Units','Normal','Position',[0 0 1 1],'Visible','Off');
if (use_ws & write_text)
	fmt = ['%4d-%02d-%02d %02d%02d:%02.0f - '...
	       '%4d-%02d-%02d %02d%02d:%02.0f   '...
	       '(El=%.1f deg)  [%s:%s]'];
	tstrg = sprintf(fmt,time(1,:),time(2,:),...
			el,name_expr,name_site);
	t = text(goodgap,goodgap/2,tstrg);
	set(t,'VerticalAlignment','Baseline',	...
		      'Fontname',font_name,	...
		      'Fontsize',font_size,	...
		      'Fontweight',font_wght');
elseif (write_text)
	fmt = ['File %d:   '...
	       '%4d-%02d-%02d %02d%02d:%02.0f - '...
	       '%4d-%02d-%02d %02d%02d:%02.0f   '...
	       '(El=%.1f deg)  [%s:%s]'];
	for k = 1:nfiles
		tstrg = sprintf(fmt,k,time(2*k-1,:),time(2*k,:),...
				r_el,name(k,:),site(k,:));
		t=text(goodgap,k*(0.6*goodgap),tstrg);
		set(t,'VerticalAlignment','Baseline',...
		      'Fontname',font_name,	     ...
		      'Fontsize',font_size,	     ...
		      'Fontweight',font_wght');
	end
end
