% load_PSrem.m: script for loading td_variable for EISCAT remotes
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
%
% script for loading td_variable for EISCAT remotes and for connecting the transmitter 
% frequencies with the receiver channels.
%
% See also: load_PS
load_PS(d_rcprog,N_rcprog)
% For remotes we must find transmission from Tromso files
R_ch=td_ch; R_f=ch_f; % Store the necessary remote variables
name_site='T';
load_PS(d_rcprog,N_rcprog)
ind=[]; % ind will contain the lines containing transmission to remotes
Rch=[]; % will tell in which channel they are received
for ch=diff_val(R_ch(R_ch>0)); %These channels in use
  chT=find(ch_f==R_f(ch)); % This is the Tromso channel for the transmission
  index=find(td_ch==chT & abs(td_am)==1);
  ind=[ind, index];
  Rch=[Rch, ch*ones(size(index))];
end
T_t1=td_t1(ind);
T_t2=td_t2(ind);
T_am=td_am(ind);
name_site='R';
load_PS(d_rcprog,N_rcprog) % load back the remote variables
global p_offsetppd
td_ch=[Rch, td_ch];
td_t1=[T_t1-p_offsetppd, td_t1];
td_t2=[T_t2-p_offsetppd, td_t2];
td_am=[T_am, td_am];
