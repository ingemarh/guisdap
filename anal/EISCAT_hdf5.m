
function EISCAT_hdf5(dirpath, datapath, showfigs)

global name_ant

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
notesfiles = dir(fullfile(dirpath,'notes*txt'));
logfiles = dir(fullfile(dirpath,'*log')); 

logs_files = [];
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
            logs_files{m} = fullfile(dirpath,targz_filename);
            [~,name,ext] = fileparts(logs_files{m}); 
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

pulses = {'arc','beata','bella','cp','CP','folke','hilde','ipy','manda','steffe','taro','tau','tyco','gup0','gup3'};
ants   = {'32m','42m','uhf','vhf','esa','esr','eis','kir','sod','tro','lyr'};

figure_check = zeros(length(image_filelist),1);

for ii = 1:length(data_files)
    
    if contains(data_files{ii},'.tar.gz')
        untarpath = fullfile(tempdir,'UntaredContent');
        if exist(untarpath)
            rmdir(untarpath,'s')
        end
        mkdir(untarpath);
        
        try
            untar(data_files{ii},untarpath)
        catch
            warning(['Ooops ... ' data_files{ii} ' could not be untared, and is therefore ignored.'])
            continue
        end
        
        untar_filelist = dir(untarpath);
    
        while length(untar_filelist(3:end)) == 1 && ~contains(untar_filelist(3).name,'.mat')
            untarfolder = untar_filelist(3).name;
            untarpath = fullfile(untarpath,untarfolder);
            untar_filelist = dir(untarpath);
        end
        
        [storepath,EISCAThdf5file] = mat2hdf5(untarpath,datapath); 
        
    else
        [storepath,EISCAThdf5file] = cedar2hdf5(data_files{ii},datapath);
    end
    
    
    
    folders = regexp(storepath,filesep,'split');
    storefolder = char(folders(end));
    display(EISCAThdf5file)
    
    
    %%% copying figures to the new data folders
    
    bb = strfind(storefolder,'_');
    cc = strfind(storefolder,'@');
    bbcc = sort([bb cc]);
    if length(bb) == 4
        pulse = storefolder(bb(2)+1:bb(4)-1);
        intper = storefolder(bb(4)+1:cc-1);
    else
        pulse = storefolder(bb(2)+1:bb(3)-1);
        intper = storefolder(bb(3)+1:cc-1);
    end
    ant = storefolder(cc+1:end);
    
    nfigs_expr = 0;
    pdf_forHDF5 = [];
   % keyboard
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
            c = 0;
            
            for oo = 1:length(dd)
                
                if oo == length(dd)
                    figpart = figname(dd(oo)+1:end);
                elseif length(bbcc) == 5 && oo == 1
                    figpart = figname(dd(oo)+1:dd(oo+2)-1);
                    if strcmp(pulse,figpart)
                        c = 1;
                    end
                elseif length(bbcc) == 5 && oo == 2 && c == 1
                    continue
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

            if (contains(fig_intper,intper) && contains(fig_ant,ant) && strcmp(fig_pulse,pulse)) && c == 1 || ...
               (contains(fig_intper,intper) && contains(fig_ant,ant) && contains(fig_pulse,pulse)) || ...     
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
                    [~,pdfname,ext]=fileparts(pdffile);
                    if isempty(pdf_forHDF5)
                        pdf_forHDF5 = [pdfname ext];
                    else
                        pdf_forHDF5 = [pdf_forHDF5 ', ' pdfname ext];
                    end
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
                [~,pdfname,ext]=fileparts(pdffile);
                if isempty(pdf_forHDF5)
                    pdf_forHDF5 = [pdfname ext];
                else
                    pdf_forHDF5 = [pdf_forHDF5 ', ' pdfname ext];
                end
                delete(pdffile,figurefile)
                figure_check(jj) = figure_check(jj) + 1;
                nfigs_expr =  nfigs_expr +1;
            end
        end
    end 
    hdf5write(EISCAThdf5file,'/metadata/figure_links',pdf_forHDF5,'WriteMode','append');
                    
    notes_forHDF5 = [];
    for nn = 1:length(notesfiles)
        notesfile = fullfile(dirpath,notesfiles(nn).name);
        copyfile(notesfile,storepath)
        if nn == 1
            notes_forHDF5 = notesfiles(nn).name;
        else
            notes_forHDF5 = [notes_forHDF5 ', ' notesfiles(nn).name];
        end
    end
    hdf5write(EISCAThdf5file,'/metadata/comment_links',notes_forHDF5,'WriteMode','append');
  
    for ll = 1:length(logs_filename)
        [~,data_filename,~] = fileparts(data_files{ii});
        if strcmp(data_filename(1:8),logs_filename{ll}(1:8))
            copyfile(logs_files{ll},storepath)
            hdf5write(EISCAThdf5file,'/metadata/logs_links',logs_filename{ll},'WriteMode','append');
        end
    end
    
    for nn = 1:length(logfiles)
        logfile = fullfile(dirpath,logfiles(nn).name);
        copyfile(logfile,storepath)
    end
    %keyboard
    % Check if metadata exist
    info = h5info(EISCAThdf5file,'/metadata');
    metavar = {info.Datasets.Name}';
    hdf5fileformeta = [];
    if contains(data_files{ii},'.tar.gz') && isempty(find(strcmp(metavar,'gfd')))
        [~,tarfilename1,~] = fileparts(data_files{ii});
        [~,tarfilename,~]  = fileparts(tarfilename1);
        if ~isempty(hdf5ncar_files)
            gg = find(contains(hdf5ncar_files,tarfilename));
            if ~isempty(gg)
                hdf5fileformeta = hdf5ncar_files{gg};
            end
        %elseif     
        end
        if isempty(hdf5fileformeta) && length(data_files) == 1
            hdf5fileformeta = hdf5rest_files{1};
        end
        disp(hdf5fileformeta)
        if hdf5fileformeta
            metacompl(hdf5fileformeta,EISCAThdf5file)  
        end
    else
    end
    if nfigs_expr == 0
        
        if contains(data_files{ii},'.tar.gz')
            input = untarpath;
            vizugo = 1;
        else
            input = EISCAThdf5file;
            metadata0d = h5read(EISCAThdf5file,'/metadata/par0d');
            aa = find(strcmp(deblank(metadata0d(1,:)),'nrec'));
            if aa
                data0d = h5read(EISCAThdf5file,'/data/par0d');
                nrec = data0d(aa);
            else
                vizugo = 1;
            end
            if nrec == 1 || nrec > 5
                vizugo = 1;
            else
                vizugo = [];
                warning(['No figures, and nrec = ' num2str(nrec) ' so no new figures generated.'])
            end
        end
        %keyboard
        if vizugo
            vizu('new',input,'HQ')
            file = vizu('save','24hrplt');
            newpng = [file '.png'];
            newpdf = [file '.pdf'];
            store_image2Hdf5(newpng,EISCAThdf5file);
            copyfile(newpdf,storepath)
            [~,pdfname,ext]=fileparts(newpdf);
            hdf5write(EISCAThdf5file,'/metadata/figure_links',[pdfname ext],'WriteMode','append');
        end
    end  
    copyfile(data_files{ii},storepath)
end

figure_check;
% Check if a figure was not copied and saved at all, or copied and saved more than once
z1 = find(figure_check==0);
z2 = find(figure_check>1);
for z3 = 1:length(z1)
    warning([image_filelist(z1(z3)).name ' was not copied or stored'])
end
for z3 = 1:length(z2)
    warning([image_filelist(z2(z3)).name ' was copied or stored more than once'])
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
