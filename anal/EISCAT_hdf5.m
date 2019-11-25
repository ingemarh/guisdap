
function EISCAT_hdf5(dirpath, datapath, showfigs)

% Show figures?
% No:  showfigs = []
% Yes: showfigs = 1
if nargin<3
    showfigs = []; 
end
if nargin<2
    error('Experiment and output path needed.')
end

if ~exist(dirpath)
    error(['The directory ' dirpath ' does not exist. Why not choose a folder that actually does exist?'])
end

if ~exist(datapath)
    mkdir(datapath);
end

% ugly hack: start_GUP clears all variables, so store and re-read!!
save(fullfile(tempdir,'my_input.mat'),'dirpath','datapath','showfigs');
start_GUP;
load(fullfile(tempdir,'my_input.mat'));
delete(fullfile(tempdir,'my_input.mat'));

image_filelist = [dir(fullfile(dirpath,'*.png'));dir(fullfile(dirpath,'*.gif'));dir(fullfile(dirpath,'*.jpg'));dir(fullfile(dirpath,'*.jpeg'));dir(fullfile(dirpath,'*ps.gz'));dir(fullfile(dirpath,'*ps'))];

logs_file = [];
logs_filename = [];
targz_filecheck = dir(fullfile(dirpath,'*tar.gz'));
targz_files = [];
if ~isempty(targz_filecheck)
    n = 1; m = 1;
    for ii = 1:length(targz_filecheck)
        targz_filename = targz_filecheck(ii).name;
        q = strfind(targz_filename,'logs.tar.gz');
        if isempty(q)
            targz_files{n} = fullfile(dirpath,targz_filename);
            n = n+1;
        else
            logs_file{m} = fullfile(dirpath,targz_filename);
            [~,name,ext] = fileparts(logs_file{m}); 
            logs_filename{m} = [name ext];
            m = m+1;
        end
    end
end

hdf5oldfiles     = dir(fullfile(dirpath,'overview','*hdf5')); 
hdf5_allfiles  = [];
hdf5_files     = [];
hdf5ncar_files = [];
hdf5rest_files = [];

if ~isempty(hdf5oldfiles)
    l = 1; n = 1; m = 1;
    for ii = 1:length(hdf5oldfiles)
        hdf5_filename = hdf5oldfiles(ii).name;
        hdf5_allfiles{ii} = fullfile(dirpath,'overview',hdf5_filename);
        if contains(hdf5_filename,'.vr') || contains(hdf5_filename,'.tr') || contains(hdf5_filename,'.kr') || contains(hdf5_filename,'.sr') 
            hdf5_files{l} = fullfile(dirpath,'overview',hdf5_filename);
            l = l+1;
        elseif contains(hdf5_filename,'NCAR')
            hdf5ncar_files{m} = fullfile(dirpath,'overview',hdf5_filename);
            m = m+1;
        else
            hdf5rest_files{n} = fullfile(dirpath,'overview',hdf5_filename);
            n = n+1;
        end
    end
end


% display(targz_files')
% display(hdf5_allfiles')
% display(hdf5_files')
% display(hdf5ncar_files')
% display(hdf5rest_files')
% display(logs_file')

oldhdf5_files = [];
if isempty(targz_files) && isempty(hdf5oldfiles) 
    error('No "old" hdf5-files nor any tar.gz-files with mat-files exist.')
elseif isempty(targz_files) && ~isempty(hdf5oldfiles)
    oldhdf5_files = hdf5_allfiles;                     % consider all old hdf5 files when making new EISCAT hdf5 files
elseif ~isempty(targz_files) && ~isempty(hdf5_files)
    oldhdf5_files = hdf5_files;                        % consider only the .vr, .tr, .kr, and .sr of the old hdf5 files when making new EISCAT hdf5 files
end

data_files = [targz_files,oldhdf5_files];
display(data_files')

pulses = {'arc','beata','bella','cp','CP','folke','hilde','ipy','manda','steffe','taro','tau','tyco'};
ants   = {'32m','42m','uhf','vhf','esa','esr','eis','kir','sod','tro','lyr'};

figure_check = zeros(length(image_filelist),1);

for ii = 1:length(data_files)
    
    if contains(data_files{ii},'.tar.gz')
        untarpath = fullfile(tempdir,'UntaredContent');
        if exist(untarpath)
            rmdir(untarpath,'s')
        end
        mkdir(untarpath);
        
        untar(targz_files{ii},untarpath)
        untar_filelist = dir(untarpath);
    
        while length(untar_filelist(3:end)) == 1 && ~contains(untar_filelist(3).name,'.mat')
            untarfolder = untar_filelist(3).name;
            untarpath = fullfile(untarpath,untarfolder);
            untar_filelist = dir(untarpath);
        end
        
        [storepath,EISCAThdf5file] = mat2hdf5(untarpath,datapath); 
        
    else
        %OldHdf5File = oldhdf5_files{ii};
        [storepath,EISCAThdf5file] = cedar2hdf5(data_files{ii},datapath);
    end
    
    folders = regexp(storepath,filesep,'split');
    storefolder = char(folders(end));
    display(EISCAThdf5file)
    
    
    %%% copying figures to the new data folders
    
    bb = strfind(storefolder,'_');
    cc = strfind(storefolder,'@');
    pulse = storefolder(bb(2)+1:bb(3)-1);
    intper = storefolder(bb(3)+1:cc-1);
    ant = storefolder(cc+1:end);
       
    nfigs_expr = 0;
    
    for jj = 1:length(image_filelist)
        figurefile = fullfile(dirpath,image_filelist(jj).name);
        [~,figname,ext] = fileparts(figurefile);
        if strcmp(ext,'.gz')
            [~,figname,~] = fileparts(figname);
        end
        
        if length(data_files) > 1
            fig_pulse  = '';
            fig_intper = '';
            fig_ant    = '';
            
            dd_  = strfind(figname,'_');
            ddat = strfind(figname,'@');
            dd = sort([dd_ ddat]);
            
            fig_intper_set = 0;
            for oo = 1:length(dd)
                if oo == length(dd)
                    figpart = figname(dd(oo)+1:end);
                else
                    figpart = figname(dd(oo)+1:dd(oo+1)-1);
                end
                
                if ~isempty(str2num(figpart)) && isempty(fig_intper)
                    fig_intper = figpart;
                    fig_intper_set = 1;
                end
                for pp = 1:length(pulses)
                    if contains(figpart,pulses(pp))
                        fig_pulse = figpart;
                    end
                end
                for qq = 1:length(ants)
                    if contains(figpart,ants(qq))
                        fig_ant = figpart;
                    end
                end
            end
            
            if fig_intper_set == 0 && ~isempty(intper)
                fig_intper = intper;
            end
            
            if (contains(fig_ant,ant) && (str2num(fig_intper)==str2num(intper)) && contains(fig_pulse,pulse)) || ...
                (contains(figname,'plasmaline') && strcmp(intper,fig_intper) && contains(fig_pulse,pulse)) || ...
                (contains(figname,'scan') && contains(fig_ant,ant) && contains(fig_pulse,pulse)) || ...
                ((contains(figname,'BoreSight') || contains(figname,'WestBeam')) && contains(fig_pulse,pulse))
                if ~strcmp(ext,'.gz') && ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')  
                    store_image2Hdf5(figurefile,EISCAThdf5file)
                    figure_check(jj) = figure_check(jj) + 1;
                    nfigs_expr =  nfigs_expr +1;
                else
                    if strcmp(ext,'.gz')
                        epsfile = gunzip(figurefile);
                        figurefile = epsfile{1};
                    end
                    pdffile = eps2pdf(figurefile);
                    copyfile(pdffile,storepath)
                    delete(pdffile,figurefile)
                    figure_check(jj) = figure_check(jj) + 1;
                    nfigs_expr =  nfigs_expr +1;
                end
            end
        elseif length(data_files) == 1
            if ~strcmp(ext,'.gz') && ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')  
                store_image2Hdf5(figurefile,EISCAThdf5file)
                figure_check(jj) = figure_check(jj) + 1;
                nfigs_expr =  nfigs_expr +1;
            else
                if strcmp(ext,'.gz')
                    epsfile = gunzip(figurefile);
                    figurefile = epsfile{1};
                end
                pdffile = eps2pdf(figurefile);
                copyfile(pdffile,storepath)
                delete(pdffile,figurefile)
                figure_check(jj) = figure_check(jj) + 1;
                nfigs_expr =  nfigs_expr +1;
            end
        end
    end 
        
    notesfile = fullfile(dirpath,'notes.txt');
    if exist(notesfile)
        copyfile(notesfile,storepath)
    end
    
%     % Check if 
%     info = h5info(EISCAThdf5file,'/metadata');
%     metavar = {info.Datasets.Name}';
%     hdf5fileformeta = [];
%     if isempty(find(strcmp(metavar,'gfd')))
%         [~,tarfilename1,~] = fileparts(targz_file{ii});
%         [~,tarfilename,~]  = fileparts(tarfilename1);
%         if ~isempty(hdf5ncar_files)
%             ff = contains(hdf5ncar_files,tarfilename);
%             gg = find(ff == 1);
%             if ~isempty(gg)
%                 hdf5fileformeta = hdf5ncar_files{gg};
%             end
%         %elseif     
%         end
%         display(hdf5fileformeta)
%         metacompl(hdf5fileformeta,EISCAThdf5file)  
%     else
%     end
    
    if nfigs_expr == 0
        
        if contains(data_files{ii},'.tar.gz')
            input = untarpath;
        else
            input = EISCAThdf5file;
        end
        vizu('new',input,'HQ')
        vizu('save','24hrplt')
        if contains(data_files{ii},'.tar.gz')
            at = strfind(untarfolder,'@');
            %keyboard
            if ~isempty(strfind(untarfolder(at-3:at-1),'ant'))
                newpng = [untarpath '/' storefolder(8:bb(3)-1) '_24hrplt' storefolder(cc:end) '.png'];
                newpdf = [untarpath '/' storefolder(8:bb(3)-1) '_24hrplt' storefolder(cc:end) '.pdf'];
            elseif ~isempty(strfind(untarfolder(at-4:at-1),'scan'))
                newpng = [untarpath '/' storefolder(8:bb(3)-1) '_scan_24hrplt' storefolder(cc:end) '.png'];
                newpdf = [untarpath '/' storefolder(8:bb(3)-1) '_scan_24hrplt' storefolder(cc:end) '.pdf'];
            else
                newpng = [untarpath '/' storefolder(8:cc-1) '_24hrplt' storefolder(cc:end) '.png'];
                newpdf = [untarpath '/' storefolder(8:cc-1) '_24hrplt' storefolder(cc:end) '.pdf'];
            end
        else
            newpng = ['/tmp/' storefolder(8:bb(3)-1) '_24hrplt' storefolder(cc:end) '.png'];
            newpdf = ['/tmp/' storefolder(8:bb(3)-1) '_24hrplt' storefolder(cc:end) '.pdf'];
        end
       
        if exist(newpng)
            store_image2Hdf5(newpng,EISCAThdf5file);
        else
            display([newpng ' does not exist. Was it stored somewhere else?'])    
        end
        if exist(newpdf)
            %pdffile = eps2pdf(neweps);
            copyfile(newpdf,storepath)
        else
            display([newpdf ' does not exist. Was it stored somewhere else?'])    
        end   
    end
  
    copyfile(data_files{ii},storepath)
end

figure_check
% Check if a figure was not copied and saved at all, or copied and saved more than once
z1 = find(figure_check==0);
z2 = find(figure_check>1);
for z3 = 1:length(z1)
    display(['Warning: ' image_filelist(z1(z3)).name ' was not copied or stored'])
end
for z3 = 1:length(z2)
    display(['Warning: ' image_filelist(z2(z3)).name ' was copied or stored more than once'])
end


if ~isempty(showfigs)
    for ii = 1:length(image_filelist)
        cmd = ['display ' fullfile(dirpath,image_filelist(ii).name) ' &'];
        system(cmd);
    end
end

if exist(fullfile(tempdir,'UntaredContent'))
    rmdir(fullfile(tempdir,'UntaredContent'),'s')
end


% 
% for ii = 1:length(oldhdf5_files)
%       
%     OldHdf5File = oldhdf5_files{ii};
%     [storepath,EISCAThdf5file] = cedar2hdf5(OldHdf5File,datapath);
%     folders = regexp(storepath,filesep,'split');
%     storefolder = char(folders(end));
%     display(EISCAThdf5file)
%     
%     notesfile = fullfile(dirpath,'notes.txt');
%     if exist(notesfile)
%         copyfile(notesfile,storepath)
%     end
%     
%     [~,filename] = fileparts(OldHdf5File);
%     logsfilename = [filename(1:8) '-logs.tar.gz'];
%     c = find(strcmp(logsfilename,logs_filename)==1);
%     if c
%         copyfile(logs_file{c},storepath)
%     end
% 
%     bb = strfind(storefolder,'_');
%     cc = strfind(storefolder,'@');
%     pulse = storefolder(bb(2)+1:bb(3)-1);
%     intper = storefolder(bb(3)+1:cc-1);
%     ant = storefolder(cc+1:end);
%     
%     nfigs_expr = 0;                    % for counting the number of figures of the considered experiment
%     
%     for jj = 1:length(image_filelist)
%         figurefile = fullfile(dirpath,image_filelist(jj).name);
%         [~,figname,ext] = fileparts(figurefile);
%         if strcmp(ext,'.gz')
%             [~,figname,~] = fileparts(figname);
%         end
%             
%         fig_pulse  = '';
%         fig_intper = '';
%         fig_ant    = '';
%             
%         dd_  = strfind(figname,'_');
%         ddat = strfind(figname,'@');
%         dd = sort([dd_ ddat]);
%             
%         fig_intper_set = 0;
%         for oo = 1:length(dd)
%             if oo == length(dd)
%                 figpart = figname(dd(oo)+1:end);
%             else
%                 figpart = figname(dd(oo)+1:dd(oo+1)-1);
%             end
%                 
%             if ~isempty(str2num(figpart)) && isempty(fig_intper)
%                 fig_intper = figpart;
%                 fig_intper_set = 1;
%             end
%             for pp = 1:length(pulses)
%                 if contains(figpart,pulses(pp))
%                     fig_pulse = figpart;
%                 end
%             end
%             for qq = 1:length(ants)
%                 if contains(figpart,ants(qq))
%                     fig_ant = figpart;
%                 end
%             end
%         end
%             
%         if fig_intper_set == 0 && ~isempty(intper)
%             fig_intper = intper;
%         end
%             
%         if (contains(fig_ant,ant) && (str2num(fig_intper)==str2num(intper)) && contains(fig_pulse,pulse)) || ...
%            (contains(figname,'plasmaline') && strcmp(intper,fig_intper) && contains(fig_pulse,pulse)) || ...
%            (contains(figname,'scan') && contains(fig_ant,ant) && contains(fig_pulse,pulse)) 
%            if ~strcmp(ext,'.gz') && ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')  
%                 store_image2Hdf5(figurefile,EISCAThdf5file)
%                 figure_check(jj) = figure_check(jj) + 1;
%                 nfigs_expr =  nfigs_expr +1;
%            else
%                 if strcmp(ext,'.gz')
%                     epsfile = gunzip(figurefile);
%                     figurefile = epsfile{1};
%                 end
%                 pdffile = eps2pdf(figurefile);
%                 copyfile(pdffile,storepath)
%                 delete(pdffile,figurefile)
%                 figure_check(jj) = figure_check(jj) + 1;
%                 nfigs_expr =  nfigs_expr +1;
%             end
%         end
%     end
% 
%     
%     if nfigs_expr == 0 
%         display(['No figures coupled to' EISCAThdf5file 'exist. Now generating new figures via vizu...'])
%         vizu('new',EISCAThdf5file,'HQ')       % EISCAThdf5file is generated in cedar2hdf5.m
%         vizu('save','24hrplt')
%         newpng = ['/tmp/' storefolder(8:bb(3)-1) '_24hrplt' storefolder(cc:end) '.png'];
%         neweps = ['/tmp/' storefolder(8:bb(3)-1) '_24hrplt' storefolder(cc:end) '.eps'];
%         keyboard
%         if exist(newpng)
%             store_image2Hdf5(newpng,EISCAThdf5file);
%         else
%             display([newpng ' does not exist. Was it stored somewhere else?'])
%         end
%         if exist(neweps)
%             pdffile = eps2pdf(neweps);
%             copyfile(pdffile,storepath)
%         else
%             display([neweps ' does not exist. Was it stored somewhere else?'])
%        end   
%     end
%     
%     copyfile(OldHdf5File,storepath);
% end 
% 
% figure_check
% % Check if a figure was not copied and saved at all, or copied and saved more than once
% z1 = find(figure_check==0);
% z2 = find(figure_check>1);
% for z3 = 1:length(z1)
%     display(['Warning: ' image_filelist(z1(z3)).name ' was not copied or stored'])
% end
% for z3 = 1:length(z2)
%     display(['Warning: ' image_filelist(z2(z3)).name ' was copied or stored more than once'])
% end
% 
% 
% if ~isempty(showfigs)
%     for ii = 1:length(image_filelist)
%         cmd = ['display ' fullfile(dirpath,image_filelist(ii).name) ' &'];
%         system(cmd);
%     end
% end
% 
% if exist(fullfile(tempdir, 'UntaredContent'))
%     rmdir(fullfile(tempdir, 'UntaredContent'),'s')
% end
