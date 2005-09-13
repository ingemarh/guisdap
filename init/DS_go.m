% DS_go: Main routine in the design package
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
function DS_go

global  DS_frequency DS_channel DS_cycle DS_Ncycles DS_label DS_Nbits DS_Walsh ...
        DS_signs DS_Npulses DS_set DS_random DS_Barker DS_bitseq DS_bitlen ...
        DS_gaplen DS_adcint DS_impresp DS_taps DS_XMITtime DS_RECtime DS_RECrange ...
        DS_RECwindow DS_Lags DS_gating DS_Gates DS_LASTrange ...
        DS_Blags DS_Bgates DS_Bwindow DS_Bcycles DS_Cgates DS_Cwindow DS_STARTaddr
global  DS_debug  
global  vc_ch lp_code lp_ra lp_nt lp_ri lp_ind p_dtau ra_next type_next code_next p_rep p_dtau
global  vc_number

parerror=0;
ES_set=[];
ES_signs=[];
if isempty(DS_signs)
  if isempty(DS_Walsh)
    if isempty(DS_Nbits)
      if isempty(DS_set)
        % Npulses is the choice then
        Npulses=DS_choose(DS_Npulses,1);
        if Npulses==1
          ES_Npulses=1;
        elseif Npulses>1 & Npulses<=5,
          sets=[0,1,12,132,3152];
          ES_set=sets(DS_Npulses);
        elseif Npulses==0,
          ES_Npulses=0; % No new transmission defined, sampling the previous one
        else
          fprintf('ERROR: Npulses for %.0f pulses not implemented, use keyword ''set'' instead\n', Npulses), parerror=1;
        end
        ES_Ncycles=1;
      else % set
        ES_set=DS_set;
        ES_Ncycles=1;
      end
    else % Nbits
      [index,ES_Ncycles]=Walsh_index(DS_Nbits);
      ES_signs=altcod(0:ES_Ncycles-1,index);
    end
  else % Walsh
    ES_Ncycles=2^ceil(log(length(DS_Walsh))/log(2));
    ES_signs=altcod(0:ES_Ncycles-1,DS_Walsh);
  end
else
  ES_signs=DS_signs;
  ES_Ncycles=size(ES_signs,2);
end

ES_Ncycles=DS_choose(DS_Ncycles,ES_Ncycles);
ES_Bcycles=DS_choose(DS_Bcycles,1:ES_Ncycles);

if isempty(DS_bitseq)
  ES_Barker=DS_choose(DS_Barker,1);
  if ES_Barker==1  
    ES_bitseq=1; 
  else
    ES_bitseq=Barker(ES_Barker);
  end
else
  ES_bitseq=col(DS_bitseq);
end
  
if isempty(DS_bitlen)
  fprintf(' ''bitlen'' must be given before design is possible\n')
  parerror=1;
else
  ES_bitlen=DS_bitlen;
end

if ~isempty(ES_set)
  ES_gaplen=ES_bitlen;
else
  ES_gaplen=0;
end
ES_gaplen=DS_choose(DS_gaplen,ES_gaplen);

ES_adcint=DS_choose(DS_adcint,ES_bitlen);

%% taps and impresp
if isempty(DS_taps)
  ES_impresp=DS_choose(DS_impresp,ES_adcint);
  ES_taps=ones(ES_impresp/p_dtau,1);
  if ES_Barker>1
    B=flipud(col(Barker(ES_Barker)));temp=[];
    adcint=ES_adcint/p_dtau;
    for i=1:length(B);
      ind=(i-1)*adcint+(1:length(ES_taps));
      temp(max(ind),1)=0;
      temp(ind)=temp(ind)+B(i)*ES_taps;
    end 
    ES_taps=temp;
  end
else
  ES_taps=col(DS_taps);
end

%% Lags
if ~isempty(ES_signs)
%  ES_Lags=ES_adcint*ES_Barker:ES_adcint*ES_Barker:(ES_bitlen*length(ES_bitseq)*size(ES_signs,1)-1);
  ES_Lags=0:ES_adcint*ES_Barker:(ES_bitlen*length(ES_bitseq)*size(ES_signs,1)-1);
elseif ~isempty(ES_set)
  ES_Lags=multipulse(ES_set)*(ES_Barker*ES_bitlen+ES_gaplen);
elseif ~isempty(ES_Npulses)
  if ES_bitlen<40,
    ES_Lags=0;
  else
    ES_Lags=0:ES_adcint:(ES_bitlen-ES_adcint);
  end
%else
%  fprintf('SERIOUS ERROR: You in fact should never end up here ....\n'), return,
end
ES_Lags=DS_choose(DS_Lags,ES_Lags);

%% gating
ES_gating=DS_choose(DS_gating,1);

 if isempty(DS_XMITtime)
  fprintf(' ''XMITtime'' must be given before design is possible\n')
  parerror=1;
else
  ES_XMITtime=DS_XMITtime;
end

%% RECtime and Gates
if isempty(DS_RECwindow)
  if isempty(DS_RECrange)
    if isempty(DS_RECtime)
      fprintf(' ''RECtime''|''RECrange''|''RECwindow'' must be given before design is possible\n')
      parerror=1;
    else
      ES_RECtime=DS_RECtime;
    end
  else
    ES_RECtime=ES_XMITtime+DS_RECrange+ES_adcint;
    if ES_Npulses==1 % Correction for long pulse modulation case
      extra=(ES_bitlen-ES_gating*ES_adcint)/2-max(ES_Lags);
      ES_RECtime=ES_RECtime+extra;
    end
  end

  if isempty(DS_LASTrange)
    if isempty(DS_Gates)
      fprintf(' ''Gates''|''LASTrange'' must be given before design is possible\n')
      parerror=1;
    else
      ES_Gates=DS_Gates;
    end
  else
    ES_Gates=ceil((DS_LASTrange-(ES_RECtime-ES_XMITtime))/(ES_gating*ES_adcint));
  end

elseif length(DS_RECwindow)==2
  ES_RECtime=DS_RECwindow(1);
	 if ~isempty(ES_signs) % Correct formula for alternating codes
    ES_Gates=floor(abs(diff(DS_RECwindow)-ES_bitlen*size(ES_signs,1)+ES_adcint)/(ES_gating*ES_adcint));
		elseif ES_gating==1 % Uniprog case
    ES_Gates=floor(abs(diff(DS_RECwindow))/(ES_gating*ES_adcint))+1;
		else % Gated long pulse case
    ES_Gates=floor(abs(diff(DS_RECwindow)-max(ES_Lags))/(ES_gating*ES_adcint));
		end
else
  fprintf(' ''RECwindow'' must have two elements\n')
  parerror=1;
end

%% cycle and offset
if isempty(DS_cycle)
  fprintf(' ''cycle'' must be given before design is possible\n')
  parerror=1;
elseif length(DS_cycle)==1
  ES_cycle=DS_cycle;
  ES_offset=floor(ES_XMITtime/ES_cycle)*ES_cycle;
else
  ES_cycle=DS_cycle(1);
  ES_offset=DS_cycle(2);
end

%% Cgates, Ctime
if isempty(DS_Cwindow)
  ES_Cgates=DS_choose(DS_Cgates,0);
  ES_Ctime=ES_cycle-(ES_Cgates*ES_gating)*ES_adcint;
elseif length(DS_Cwindow)==1
  ES_Ctime=DS_Cwindow(1);
  if isempty(DS_Cgates) | DS_Cgates==0
    fprintf(' if ''Cwindow'' has only one element, ''Cgates'' must be nonzero')
    parerror=1;
  else
    ES_Cgates=DS_Cgates;
  end
elseif length(DS_Cwindow)==2
  ES_Ctime=DS_Cwindow(1);
  ES_Cgates=floor(abs(diff(DS_Cwindow))/(ES_gating*ES_adcint))+1;
else
  fprintf(' ''Cwindow'' can have at most two elements\n')
  parerror=1;
end

%% Blags
if isempty(DS_Blags)
  ES_Blags=diff_val([0,ES_Lags(find(ES_Lags<(length(ES_taps)-1)*p_dtau))]);
else
  ES_Blags=DS_Blags;
end

%% Bgates, Btime
if isempty(DS_Bwindow)
  ES_Bgates=DS_choose(DS_Bgates,0);
  ES_Btime=ES_Ctime-(ES_Bgates*ES_gating+max(ES_Blags/ES_adcint))*ES_adcint-length(ES_taps)*p_dtau;
elseif length(DS_Bwindow)==1
  ES_Btime=DS_Bwindow(1);
		if isempty(DS_Bgates) | DS_Bgates==0
    fprintf(' if ''Bwindow'' has only one element, ''Bgates'' must be nonzero')
    parerror=1;
  else
    ES_Bgates=DS_Bgates;
		end
elseif length(DS_Bwindow)==2
  ES_Btime=DS_Bwindow(1);
  ES_Bgates=floor(abs(diff(DS_Bwindow-max(ES_Blags)))/(ES_gating*ES_adcint))+1;
else
  fprintf(' ''Bwindow'' can have at most two elements\n')
  parerror=1;
end


ES_STARTaddr=DS_choose(DS_STARTaddr,ra_next);
ES_channel=DS_choose(DS_channel,max([vc_ch,0])+1);
if isempty(DS_label)
  ind=find(lp_ra(1:lp_ind)==ES_STARTaddr);
  if ~isempty(ind), 
    ES_label=lp_code(ind(1));
  else
    ES_label=max([lp_code,0])+1;
  end
else
  ES_label=DS_label;
end

if parerror, return, end
% The design code starts here!!!!!

lp_start=lp_ind;
vcnumbers=[];
for SCAN=1:ES_Ncycles
    
    SCANstart=(SCAN-1)*ES_cycle+ES_offset;
    ra_next=ES_STARTaddr;
    XMITtime=rem(ES_XMITtime,ES_cycle)+SCANstart; 
    RECtime=rem(ES_RECtime,ES_cycle)+SCANstart; 

    code_next=ES_label;
    if ~isempty(ES_signs)
      XMIT_pulse(ES_channel,XMITtime,ES_signs(:,1+rem(SCAN-1,size(ES_signs,2))),ES_bitseq,ES_bitlen),
    elseif ~isempty(ES_set)
      XMIT_mp(ES_channel,XMITtime,ES_set,ES_bitlen,ES_gaplen,ES_bitseq)
    elseif ~isempty(ES_Npulses)
      XMIT_pulse(ES_channel,XMITtime,1,ES_bitseq,ES_bitlen),
%      XMIT_sp(ES_channel,XMITtime,ES_bitlen)
    else
      fprintf('SERIOUS ERROR: You in fact should never end up here ....\n'), return,
    end
    vcnumbers=[vcnumbers, vc_number]; % Store the virtual channels numbers, (used later)

    REC_impresp(ES_taps,'taps') 
    type_next='s';  
    REC_sampling(RECtime,ES_adcint),

    if ~isempty(ES_signs)
      CORR_alter(ES_Gates,ES_gating,ES_Lags,size(ES_signs,1))
    elseif ~isempty(ES_set)
      CORR_mp(ES_gating,ES_Gates,ES_Lags)
    elseif ~isempty(ES_Npulses) & (all(ES_Lags==0) | ES_gating==1)
      CORR_uprog(ES_Gates,ES_gating,ES_Lags)        
    elseif ~isempty(ES_Npulses) 
      CORR_lp(ES_gating,0,ES_Gates,ES_Lags)
    else
      fprintf('SERIOUS ERROR: You in fact should never end up here ....\n'), return,
    end

    if any(ES_Bcycles(1,:)==SCAN)
      if ES_Bgates>0,
        type_next='b';
        REC_sampling(SCANstart+ES_Btime), CORR_uprog(ES_Bgates,ES_gating,ES_Blags), end

      if ES_Cgates>0
        type_next='c';
        ES_Clags=0;
        REC_sampling(SCANstart+ES_Ctime), CORR_uprog(ES_Cgates,ES_gating,ES_Clags), end
    end
end

p_rep=max([p_rep, (ES_offset+ES_Ncycles*ES_cycle)/p_dtau]);
lps=(lp_start+1):lp_ind;
ra_next=max(lp_ra(lps)+(lp_nt(lps)-1).*lp_ri(lps))+1;

% This addition in not used in standard experiments:
% Build for ESR testing!
for BC_loop=2:size(ES_Bcycles,1)
  BC_STARTaddr=ra_next;
  lp_start=lp_ind;
  for SCAN=1:ES_Ncycles
    ra_next=BC_STARTaddr;
    SCANstart=(SCAN-1)*ES_cycle+ES_offset;
    if any(ES_Bcycles(BC_loop,:)==SCAN)
      vc_number=vcnumbers(SCAN);
      fprintf(' More backgr/ cal: Virtual channel %.0f, real channel %.0f\n',vc_number,vc_ch(vc_number))
      if ES_Bgates>0,
        type_next='b';
        REC_sampling(SCANstart+ES_Btime), CORR_uprog(ES_Bgates,ES_gating,ES_Blags), end

      if ES_Cgates>0
        type_next='c';
        ES_Clags=0;
        REC_sampling(SCANstart+ES_Ctime), CORR_uprog(ES_Cgates,ES_gating,ES_Clags), end
    end
		end
  lps=(lp_start+1):lp_ind;
  ra_next=max(lp_ra(lps)+(lp_nt(lps)-1).*lp_ri(lps))+1;
end
