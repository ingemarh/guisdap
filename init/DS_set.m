% DS_main: Main routine in the design package
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
function DS_main(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,...
                 arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20)

global  DS_frequency DS_channel DS_cycle DS_Ncycles DS_label DS_Nbits DS_Walsh ...
        DS_signs DS_Npulses DS_set DS_random DS_Barker DS_bitseq DS_bitlen ...
        DS_gaplen DS_adcint DS_impresp DS_taps DS_XMITtime DS_RECtime DS_RECrange ...
        DS_RECwindow DS_Lags DS_gating DS_Gates DS_LASTrange ...
        DS_Blags DS_Bgates DS_Bwindow DS_Bcycles DS_Cgates DS_Cwindow DS_STARTaddr
global  DS_debug  
global  vc_ch lp_code lp_ra lp_nt lp_ri lp_ind p_dtau ra_next type_next code_next p_rep p_dtau

str=zeros(1,nargin); % Element is set true, if the corresponding argument is string
for ind=1:nargin
  str(ind)=eval(['isstr(arg',int2str(ind),')']);
end
str(nargin+1)=1; % To ease programming later.


% All the keywords are listed here in the first column
% Second column tells what kind of numeric value is expected, if any:
% 'm' : matrix valued 
% 'v' : vector valued 
% 's' : scalar valued 
% ' ' : no numeric value expected
keys=[...
'clear    ',' ';
'debug    ',' ';
'frequency','s';
'channel  ','s';
'cycle    ','s';
'Ncycles  ','s';
'label    ','s';
'Nbits    ','s';
'Walsh    ','v';
'signs    ','m';
'Npulses  ','s';
'set      ','s';
'random   ',' ';
'Barker   ','s';
'bitseq   ','v';
'bitlen   ','s';
'gaplen   ','s';
'adcint   ','s';
'impresp  ','s';
'taps     ','v';
'XMITtime ','s';
'RECtime  ','s';
'RECrange ','s';
'RECwindow','v';
'Lags     ','v';
'gating   ','v';
'Gates    ','s';
'LASTrange','s';
'Blags    ','v';
'Bgates   ','s';
'Bwindow  ','v';
'Bcycles  ','s';
'Cgates   ','s';
'Cwindow  ','v';
'STARTaddr','s';
'go       ',' ';
];
keywords=keys(:,1:9);
values=keys(:,10);

% The parser:
% The programs finds out what keyword was intended in the call
argnumber=zeros(1,length(values));    % Store here the input argument number 
keyerror=0;                           % Set true if an error occurs
% Keywords are always strings. Therefore the loop goes through all strings
for argind=find(str(1:nargin))
  arg=eval(['arg',int2str(argind)]);  % arg contains the input argument
  lenarg=min([9,length(arg)]);        % maximum length of keywords is 9, so strip any extra

  % Here we take 'lenarg' columns from the keyword table and 
		% make a long vector for efficient search by matlab findstr-command.
  words=lower(keywords(:,1:lenarg))';words=words(:)';
  pos=findstr(words,lower(arg));
  if lenarg==1 % Easy case, the input argument was one character long
    argkey=pos;
  else 
    posind=find(rem(pos,lenarg)==1);  % find those hits, which start at a word boundary
    argkey=(pos(posind)-1)/lenarg+1;
  end

  if length(argkey)==1 % This is the correct case BUT ...
		  if (values(argkey)~=' ' &  str(argind+1)) % Numerical argument wanted but not found
      fprintf(['ERROR: Keyword ',keywords(argkey,:),' requires parameters\n'])
      keyerror=1;
    else % All is well!!!!!
      argnumber(argkey)=argind;   % Store here which input argument referenced this keywords
    end
  elseif isempty(argkey)  % If no hits found, the user supplied an unknown keyword
    fprintf(['ERROR: Unknown keyword  ',arg,'\n'])
    keyerror=1;
		else % length(argkey)>1 If more than one hit, the keyword is ambiguous
    fprintf(['ERROR:  Ambiguous keyword  ''',arg,''':']);
				for i=1:length(argkey), 
      fprintf([' ',keywords(argkey(i),:)]); 
    end, fprintf('?\n')
    keyerror=1;
  end
end
if keyerror, return, end

GOtoDESIGN=0;
for key=find(argnumber); % These keywords were referenced by one input argument
  argind=argnumber(key); % Corresponding input argument number
  arg=keywords(key,keywords(key,:)~=' '); % Strip blancks from the end
  if values(key)~=' '       % This keyword requires a numerical parameter 
    param=eval(['arg',int2str(argind+1)]);
  end
  if strcmp(lower(arg),'clear')
    DS_debug    =0;
    DS_cycle    =[];
    DS_Ncycles  =[];
    DS_Nbits    =[];
    DS_Walsh    =[];
    DS_signs    =[];
    DS_Npulses  =[];
    DS_set      =[];
    DS_random   =[];
    DS_Barker   =[];
    DS_bitseq   =[];
    DS_bitlen   =[];
    DS_gaplen   =[];
    DS_adcint   =[];
    DS_impresp  =[];
    DS_taps     =[];
    DS_XMITtime =[];
    DS_RECtime  =[];
    DS_RECwindow=[];
    DS_Lags     =[];
    DS_gating   =[];
    DS_RECrange =[];
    DS_Gates    =[];
    DS_LASTrange=[];
    DS_Nresmem  =[];
    DS_Blags    =[];
    DS_Bgates   =[];
    DS_Bwindow  =[];
    DS_Bcycles  =[];
    DS_Cgates   =[];
    DS_Cwindow  =[];
    DS_STARTaddr=[];
    DS_frequency=[];
    DS_channel  =[];
    DS_label    =[];
  elseif strcmp(lower(arg),'debug')
    DS_debug=1;
    if DS_debug, fprintf(' Debug started\n'), end
  elseif strcmp(lower(arg),'random')
    DS_random=1;
    if DS_debug, eval(['DS_',arg]), end
  elseif strcmp(lower(arg),'go')
    GOtoDESIGN=1;
  else
    com=['DS_',arg,'=param;'];
    eval(com)
    if DS_debug, eval(['DS_',arg]), end
  end
end

% If 'go' was issued as one of the call parameters,
% make a call to the design routine DS_go
% Otherwise return to calling program (or command prompt) 
% so that the definition of the variables can be continued
if GOtoDESIGN, DS_go, end
