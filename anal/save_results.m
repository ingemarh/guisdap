% save_results.m: function to store whole profile of results into a file
% GUISDAP v.1.81 03-02-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% function save_results
function save_results

global result_path name_expr name_site name_ant
global p_XMITloc p_RECloc sc_angle p_om0
global d_time GUP_ver ch_az ch_el ch_Pt p_m0 p_N0 p_dtau v_lightspeed
global r_h r_spec r_om r_freq r_phasepush r_lag r_acf r_ace
global r_range r_param r_error r_res r_status r_dp r_Offsetppd r_w
global r_apriori r_apriorierror
global pp_range pp_sigma pp_err pp_w
global di_results sysTemp a_NCAR a_realtime path_tmp NCAR_fid a_integr
global webfile a_save local a_gfd
global a_autodir di_figures START_TIME a_Magic_const a_code

if length(d_time)>0 & a_save
  filename=sprintf('%08d.mat',fix(tosecs(d_time(2,:))));
else
  filename='00000000.mat';
end
[i1,i2]=fileparts(result_path(1:end-1));
if strcmp(i2,'AUTO')
 if length(a_integr)>1
  a_autodir.int='scan';
 elseif a_integr==0
  a_autodir.int='ant';
 elseif a_integr<0
  a_autodir.int=['ant' num2str(-a_integr)];
 else
  a_autodir.int=num2str(a_integr);
 end
elseif isstruct(a_autodir) & any(d_time(1,1:3)-a_autodir.date)
 if a_NCAR
  NCAR_output
  if a_realtime & isunix & ~isempty(local.site)
   do_NCAR([],2)
  else
   do_NCAR([],a_NCAR)
  end
 end
 if di_figures(5)
  vizu('new','rtgup')
  if di_figures(5)>1
    vizu('save',[],'print')
  else
    vizu('save')
  end
  START_TIME=[];
 end
 r_h=[];
end
if isempty(r_h)
 if isstruct(a_autodir)
  a_autodir.date=d_time(2,1:3);
  result_dir=sprintf('%d-%02d-%02d_%s_%s@%s%s',a_autodir.date,name_expr,a_autodir.int,name_ant,filesep);
  result_path=fullfile(i1,result_dir,filesep);
 end
 if ~exist(result_path,'dir')
  [i1,i2,i3]=fileparts(result_path(1:end-1));
  if isempty(i1), mkdir([i2 i3]), else, mkdir(i1,[i2 i3]), end
 elseif ~isempty(dir([result_path '*.mat']))
  beep
  fprintf('\n********** %s is not empty! **********\n\n',result_path)
 end
 if ~isempty(a_gfd) & ~strcmp(path_tmp,result_path)
  save_setup([result_path '.gup']);
  save_setup([result_path 'gfd_setup.m']);
 end
end
s2km=p_dtau*1e-6*v_lightspeed/2/1000;

r_ver=GUP_ver;
r_time=d_time;
r_az=ch_az(1);
r_el=ch_el(1);
r_Pt=ch_Pt(1);
r_m0=p_m0;
r_XMITloc=p_XMITloc;
r_RECloc=p_RECloc;
r_SCangle=sc_angle/2;

r_pp=pp_sigma*p_N0;
r_pprange=col(pp_range)*s2km;
r_pperr=pp_err*p_N0;
r_ppw=col(pp_w)*s2km;

r_h=col(range_to_height(r_range,ch_el(1)));
r_range=col(r_range)*s2km;
r_Tsys=sysTemp(isfinite(sysTemp));
r_Magic_const=a_Magic_const;
r_om0=p_om0;
r_w=r_w*s2km;
r_code=a_code;
r_gfd=a_gfd;

if ~di_results
 fprintf('Status: '); fprintf('%d',r_status);
 fprintf(' %.1fMW %.0f/%.0f %.0fK\n',r_Pt/1e6,r_az,r_el,median(r_Tsys));
end

file=[result_path filename];
disp(file)
name_sig=[local.site local.host ' ' datestr(now)];
save_noglobal(file,r_ver,name_expr,name_site,name_ant,r_time,r_az,r_el,r_Pt,...
     r_m0,r_range,r_h,r_param,r_error,r_res,r_status,r_dp,r_w,r_apriori,...
     r_apriorierror,r_pp,r_pprange,r_pperr,r_ppw,r_XMITloc,r_RECloc,...
     r_SCangle,r_Tsys,r_Offsetppd,r_Magic_const,r_spec,r_om,r_om0,r_freq,...
     r_phasepush,name_sig,r_lag,r_acf,r_ace,r_code,r_gfd)
if a_NCAR
 file0=sprintf('%sNCAR_%d-%02d-%02d_%s',result_path,d_time(2,1:3),name_expr);
 if isstruct(a_autodir)
  file0=[file0 '_' a_autodir.int];
 end
 file0=[file0 '@' name_ant];
 i1=[]; i2=[];
 if rem(a_NCAR,2)
  i1=[file0 '.asc'];
  if a_realtime & isunix & ~isempty(local.site)
   i1=[path_tmp 'latest_' name_ant(1:3) '.asc'];
   if exist(i1,'file')
    delete(i1)
   end
  end
 end
 if a_NCAR>1, i2=[file0 '.bin']; end
 NCAR_output(file,i1,i2)
 if strfind(i1,'latest')
  fclose(NCAR_fid(1)); NCAR_fid(1)=0;
  webfile(2)=cellstr(i1);
 end
end

% save file name to "filelist.dat"
%listfid=fopen([result_path 'filelist.dat'],'a');
%fprintf(listfid,'%s\n',filename); fclose(listfid);
