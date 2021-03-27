
function EISCAT_hdf5(dirpath, datapath, showfigs)
% EISCAT_hdf5(dirpath, datapath, showfigs)
% The master script for creating EISCAT HDF5-files from old madrigal
% formated files and handling corresponding figures and additional files
% dirpath: path to the directory with old files 
% datapath: path to the folder where the EISCAT HDF5 file and additional
% files are stored
% Show figures?
% No:  showfigs = []
% Yes: showfigs = 1 (non-empty)

global path_GUP % name_ant

if nargin<3
    showfigs = []; 
end
if nargin<2
    error('Experiment and output path needed.')
end

if ~exist('path_GUP','var') || isempty(path_GUP)
   error('Guisdap needs to be initiated. Run start_GUP.')
end

if ~exist(dirpath)
    error(['The directory ' dirpath ' does not exist. Why not choose a directory that actually does exist?'])
end

if ~exist(datapath)
    mkdir(datapath);
end

% ugly hack: start_GUP clears all variables, so store and re-read!!
% save(fullfile(tempdir,'my_input.mat'),'dirpath','datapath','showfigs');
% start_GUP;
% load(fullfile(tempdir,'my_input.mat'));
% delete(fullfile(tempdir,'my_input.mat'));

image_filelist = [dir(fullfile(dirpath,'*.png'));dir(fullfile(dirpath,'*.gif'));dir(fullfile(dirpath,'*.jpg'));dir(fullfile(dirpath,'*.jpeg'));dir(fullfile(dirpath,'*.pdf'));dir(fullfile(dirpath,'*ps.gz'));dir(fullfile(dirpath,'*ps'))];
%image_filelist_unk = [dir(fullfile('/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/tro/11mar08_unkfigures','*.png'));dir(fullfile(dirpath,'*.gif'));dir(fullfile(dirpath,'*.jpg'));dir(fullfile(dirpath,'*.jpeg'));dir(fullfile(dirpath,'*.pdf'));dir(fullfile('/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/11mar08_unkfigures','*ps.gz'));dir(fullfile(dirpath,'*ps'))];
notesfiles = dir(fullfile(dirpath,'notes*txt'));
logfiles = dir(fullfile(dirpath,'*log')); 

logs_files = [];
logs_filename = [];
targz_filecheck = [dir(fullfile(dirpath,'*tar.gz'));dir(fullfile(dirpath,'*tar'))];
targz_files = [];
junk_files = [dir(fullfile(dirpath,'*.txt'));dir(fullfile(dirpath,'*.log'))];
junk_files = junk_files(~startsWith({junk_files.name},{'note'}));
for utf = 1:length(junk_files)
    junkfortar(utf,:) = {strjoin([{junk_files(utf).folder} {junk_files(utf).name}],'/')};
end

if ~isempty(targz_filecheck)
    n = 1; m = 1;
    for ii = 1:length(targz_filecheck)
        targz_filename = targz_filecheck(ii).name;
        if contains(targz_filename,'logs.tar')
            logs_files{m} = fullfile(dirpath,targz_filename);
            [~,name,ext] = fileparts(logs_files{m}); 
            logs_filename{m} = [name ext];
            m = m + 1;
        else
            targz_files{n} = fullfile(dirpath,targz_filename); 
            n = n + 1;
        end
    end
end

hdf5oldfiles     = dir(fullfile(dirpath,'overview','*hdf5')); 
hdf5_allfiles  = [];
hdf5_files     = [];
hdf5ncar_files = [];
hdf5rest_files = [];

if ~isempty(hdf5oldfiles)
    l = 1; n = 1; m = 1; o = 1;
    for ii = 1:length(hdf5oldfiles)
        hdf5_filename = hdf5oldfiles(ii).name;
        if contains(hdf5_filename,'.ar') || contains(hdf5_filename,'.vr') || contains(hdf5_filename,'.tr') || contains(hdf5_filename,'.kr') || contains(hdf5_filename,'.sr')
            hdf5_files{l} = fullfile(dirpath,'overview',hdf5_filename); l = l + 1;
            hdf5_allfiles{o} = fullfile(dirpath,'overview',hdf5_filename); o = o + 1;
        elseif contains(hdf5_filename,'NCAR_')
            hdf5ncar_files{m} = fullfile(dirpath,'overview',hdf5_filename); m = m+1;
            hdf5_allfiles{o} = fullfile(dirpath,'overview',hdf5_filename); o = o + 1;
        else
            hdf5rest_files{n} = fullfile(dirpath,'overview',hdf5_filename); n = n+1;
            hdf5_allfiles{o} = fullfile(dirpath,'overview',hdf5_filename); o = o + 1;
        end
    end
end

oldhdf5_files = [];
if isempty(targz_files) && isempty(hdf5oldfiles) 
    error(['No EISCAT_hdf5 file generated: no old hdf5-files nor any tar.gz-files exist to handle in ' dirpath '.'])
elseif isempty(targz_files) && ~isempty(hdf5oldfiles)
    oldhdf5_files = hdf5_allfiles;                     % consider all old hdf5 files when making new EISCAT hdf5 files
elseif ~isempty(targz_files)  && ~isempty(hdf5_files)
    oldhdf5_files = hdf5_files;                        % consider only the .ar. .vr, .tr, .kr, and .sr of the old hdf5 files when making new EISCAT hdf5 files
end

data_files = [targz_files,oldhdf5_files];
pulses = {'arc','beata','bella','cp','CP','folke','hilde','ipy','manda','steffe','taro','tau','otia','tyco','gup0','gup3'};
ants   = {'32m','42m','uhf','vhf','esa','esr','eis','kir','sod','tro','lyr'};

antcheck = 0;
for df = 1:length(data_files)
    if contains(data_files{df},'ant')
        antcheck = 1;
    end
end

figure_check = zeros(length(image_filelist),1);
npdf = 0;
for ii = 1:length(data_files)
    disp([newline 'Handling:' newline data_files{ii}])
    list_fortar      = {};
    list_linksfortar = {};
    list_junkfortar  = junkfortar;
    if contains(data_files{ii},'.tar')
        untarpath = fullfile(tempdir,'UntaredContent');
        if exist(untarpath)
            rmdir(untarpath,'s')
        end
        mkdir(untarpath);
        try
            untar(data_files{ii},untarpath)
            tared = 1;
        catch
            if length(data_files) == 1 && ~isempty(hdf5_allfiles) == 1    
                warning(['Ooops ... ' data_files{ii} ' could not be untared, and is replaced by ' hdf5_allfiles{1} newline])
                data_files = hdf5_allfiles(1);
                disp(['Handling:' newline data_files{ii} newline])
                [storepath,EISCAThdf5file] = cedar2hdf5(data_files{ii},datapath);
                vecvel = zeros(length(EISCAThdf5file),1);
                tared = 0;
            else
                warning(['Ooops ... ' data_files{ii} ' could not be untared, and is therefore ignored.' newline])
                continue
            end
        end    
        
        if tared
            untar_filelist = dir(untarpath);
            untar_filelist = untar_filelist(~ismember({untar_filelist.name},{'.','..'}));   % ignore '.' and '..'

            while length(untar_filelist) == 1 && ~contains(untar_filelist(1).name,'.mat')
                untarfolder = untar_filelist(1).name;
                untarpath = fullfile(untarpath,untarfolder);
                untar_filelist = dir(untarpath);
                untar_filelist = untar_filelist(~startsWith({untar_filelist.name},{'.'}) & ~endsWith({untar_filelist.name},{'.m','.dat','.log','bin','.tar','.gz'}));   % ignore '.' and '..' etc
            end
        
            for jj = 1:length(untar_filelist)
                [~,~,ext] = fileparts(untar_filelist(jj).name);
                if strcmp(ext,'.bz2')
                    unix(['bunzip2 ' fullfile(untar_filelist(jj).folder,untar_filelist(jj).name)]);
                end
            end
           
            if ~isempty(untar_filelist)
                [storepath,EISCAThdf5file,nvecvel,fortar,junk_fortar,links_fortar] = mat2hdf5(untarpath,datapath);
                list_linksfortar = [list_linksfortar;links_fortar];
                list_junkfortar = [list_junkfortar; junk_fortar];
                list_fortar = [links_fortar;fortar];
                vecvel = zeros(length(EISCAThdf5file),1);
                if nvecvel>0
                    vecvel(1:nvecvel) = ones(nvecvel,1); 
                end
            elseif isempty(untar_filelist) && length(data_files) == 1 && ~isempty(hdf5_allfiles)  
                warning(['Ooops ... ' data_files{ii} ' was untared but empty, and is replaced by ' hdf5_allfiles{1} newline])
                data_files = hdf5_allfiles(1);
                disp(['Handling:' newline data_files{ii} newline])
                [storepath,EISCAThdf5file] = cedar2hdf5(data_files{ii},datapath);
                list_fortar = [links_fortar;data_files{ii}];
                vecvel = zeros(length(EISCAThdf5file),1);
            elseif isempty(untar_filelist)
                warning(['Ooops ... ' data_files{ii} ' was untared but empty, and is therefore ignored.' newline])
                continue
            end
        end
    else
        list_fortar = [list_fortar;data_files{ii}];
        if contains(data_files{ii},'.ar')
            [storepath,EISCAThdf5file] = cedar2hdf5_vel(data_files{ii},datapath);
            vecvel = ones(length(EISCAThdf5file),1);
        else
            [storepath,EISCAThdf5file] = cedar2hdf5(data_files{ii},datapath);
            vecvel = zeros(length(EISCAThdf5file),1);
        end
    end

    nfiles = length(EISCAThdf5file);
    isempt = [];
    for nf = 1:nfiles
        if isempty(EISCAThdf5file{nf})
            isempt = [isempt nf];
        end
    end
    EISCAThdf5file(isempt) = [];
    storepath(isempt) = [];
    vecvel(isempt) = [];
    
    nfiles = length(EISCAThdf5file);

    for nnn = 1:nfiles    
        folders = regexp(storepath{nnn},filesep,'split');
        storefolder = char(folders(end));
        display([newline 'Generated:' newline EISCAThdf5file{nnn}])

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
        if ~isnan(str2double(ant(end)))  % If last 'letter' in ant is a number, remove it before comparison with figure antenna name.
            ant = ant(1:end-1);
        end
        
        nfigs_expr = 0;

        for jj = 1:length(image_filelist)
            
            figurefile = fullfile(dirpath,image_filelist(jj).name);
            [~,figname,ext] = fileparts(figurefile);
            if strcmp(ext,'.gz')
                [~,figname,~] = fileparts(figname);
            end
         
            if nfiles > 1 || length(data_files) > 1
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
                    elseif length(bbcc) == 5 && oo == 1 && length(dd) > 2
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
                
                intpertol = 3;
                intpercheck = str2double(intper)-intpertol < str2double(fig_intper) && str2double(fig_intper) < str2double(intper)+intpertol;  % demand fig_interp to be in the interval intper +/- som tolerence

                if (intpercheck && contains(fig_ant,ant) && strcmp(fig_pulse,pulse) && c == 1) || ...
                   (intpercheck && contains(fig_ant,ant) && contains(fig_pulse,pulse)) || ...   
                   (isempty(fig_intper) && contains(fig_ant,ant) && contains(fig_pulse,pulse)) || ... 
                   (contains(figname,'plasmaline') && intpercheck && contains(fig_pulse,pulse)) || ...
                   (contains(figname,'scan') && contains(fig_ant,ant) && contains(fig_pulse,pulse)) || ...
                   ((contains(figname,'Bore') || contains(figname,'West')) && contains(fig_pulse,pulse))
                    if ~strcmp(ext,'.gz') && ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')  
                        if strcmp(ext,'.pdf')
                            npdf = npdf + 1;
                            copyfile(figurefile,storepath{nnn})
                            [~,pdfname,ext] = fileparts(figurefile);
                            pdf_forHDF5(npdf) = {[pdfname ext]};
                            list_linksfortar(length(list_linksfortar)+1,:) = {fullfile(storepath{nnn},[pdfname ext])};
                        else    
                            image2hdf5(figurefile,EISCAThdf5file{nnn})
                            list_fortar(length(list_fortar)+1,:) = {figurefile};
                        end
                        figure_check(jj) = figure_check(jj) + 1;
                        nfigs_expr =  nfigs_expr +1;
                    else
                        saveplot = 1;
                        if strcmp(ext,'.gz')
                            try
                                epsfile = gunzip(figurefile);
                                figurefile = epsfile{1};
                            catch
                                warning([figurefile ' could not be unzipped.'])
                                saveplot = 0;
                            end    
                        end
                        if saveplot
                            pdffile = eps2pdf(figurefile);
                            npdf = npdf + 1;
                            copyfile(pdffile,storepath{nnn})
                            [~,pdfname,ext] = fileparts(pdffile);
                            pdf_forHDF5(npdf) = {[pdfname ext]};
                            list_linksfortar(length(list_linksfortar)+1,:) = {fullfile(storepath{nnn},[pdfname ext])};
                            delete(pdffile,figurefile)
                            figure_check(jj) = figure_check(jj) + 1;
                            nfigs_expr =  nfigs_expr +1;
                        end
                    end
                end
            else
                if ~strcmp(ext,'.gz') && ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')  
                    if strcmp(ext,'.pdf')
                        npdf = npdf + 1;
                        copyfile(figurefile,storepath{nnn})
                        [~,pdfname,ext] = fileparts(figurefile);
                        pdf_forHDF5(npdf) = {[pdfname ext]};
                        list_linksfortar(length(list_linksfortar)+1,:) = {fullfile(storepath{nnn},[pdfname ext])};
                    else
                        image2hdf5(figurefile,EISCAThdf5file{nnn})
                        list_fortar(length(list_fortar)+1,:) = {figurefile};
                    end
                    figure_check(jj) = figure_check(jj) + 1;
                    nfigs_expr =  nfigs_expr +1;
                else
                    if strcmp(ext,'.gz')
                        try
                            epsfile = gunzip(figurefile);
                            figurefile = epsfile{1};
                            saveplot = 1;
                        catch
                            warning([figurefile ' could not be unzipped.'])
                            saveplot = 0;
                        end
                    end
                    if saveplot
                        pdffile = eps2pdf(figurefile);
                        npdf = npdf + 1;
                        copyfile(pdffile,storepath{nnn})
                        [~,pdfname,ext] = fileparts(pdffile);
                        pdf_forHDF5(npdf) = {[pdfname ext]};
                        list_linksfortar(length(list_linksfortar)+1,:) = {fullfile(storepath{nnn},[pdfname ext])};
                        delete(pdffile,figurefile)
                        figure_check(jj) = figure_check(jj) + 1;
                        nfigs_expr =  nfigs_expr +1;
                    end
                end
            end
        end 

        if npdf
            strds2hdf5(EISCAThdf5file{nnn},'/figures','figure_links',pdf_forHDF5')
            npdf = 0; clear pdf_forHDF5
        end

        for nn = 1:length(notesfiles)
            notesfile = fullfile(dirpath,notesfiles(nn).name);
            note2hdf5(notesfile,EISCAThdf5file{nnn},nn)  
            list_fortar(length(list_fortar)+1,:) = {notesfile};       
        end

        for ll = 1:length(logs_filename)
            [~,data_filename,~] = fileparts(data_files{ii});
            if strcmp(data_filename(1:8),logs_filename{ll}(1:8))  
                strds2hdf5(EISCAThdf5file{nnn},'/metadata','logs_links',{logs_filename{ll}})
                copyfile(logs_files{ll},storepath{nnn})
                list_linksfortar(length(list_linksfortar)+1,:) = {fullfile(storepath{nnn},logs_filename{ll})};
            end
        end

%         for nn = 1:length(logfiles)
%             logfile = fullfile(dirpath,logfiles(nn).name);
%             %copyfile(logfile,storepath{nnn})
%             list_junkfortar{length(list_junkfortar)+1,:} = logfile;
%         end

        % Check if metadata exist
        info = h5info(EISCAThdf5file{nnn},'/metadata/software');
        if ~isempty(info.Groups)
            metagroups = {info.Groups.Name}';
        else
            metagroups = {''}';
        end
        hdf5fileformeta = [];

        if contains(data_files{ii},'.tar') && isempty(find(strcmp(metagroups,'/metadata/software/gfd'),1))
            [~,tarfilename1,~] = fileparts(data_files{ii});
            [~,tarfilename,~]  = fileparts(tarfilename1);
            if ~isempty(hdf5ncar_files)
                gg = find(contains(hdf5ncar_files,tarfilename));
                if ~isempty(gg)
                    hdf5fileformeta = hdf5ncar_files{gg};
                end
            end
            if isempty(hdf5fileformeta) && length(hdf5rest_files) == 1
                hdf5fileformeta = hdf5rest_files{1};
            end

            if hdf5fileformeta
                metacompl(hdf5fileformeta,EISCAThdf5file{nnn})  
                disp(['gfd was missing ... Taking metadata from ' hdf5fileformeta '.'])
            elseif isempty(hdf5fileformeta) && length(hdf5rest_files) > 1
                warning(['gfd was missing ... several (>1) older HDF5-files (not NCAR) were found. The correct complementing metadata need to be manually extracted.'])
            else
                warning(['gfd was missing ... and no complementing metadata was found.'])
            end
        else
        end
        
        vizugo = [];
        if nfigs_expr == 0
            if vecvel(nnn) == 1
                if isempty(image_filelist) %%% Make one (or two) new plot(s) from efield!
                    
                    hdf5file_info = h5info(EISCAThdf5file{nnn},'/metadata/');
                    meta_datasets = {hdf5file_info.Datasets.Name}';
                    if ~isempty(find(contains(meta_datasets,'par0d')==1))
                        metadata0d = deblank(h5read(EISCAThdf5file{nnn},'/metadata/par0d'));
                        data0d     = double(h5read(EISCAThdf5file{nnn},'/data/par0d'));     
                        nr0        = find(strcmp(metadata0d(1,:),'nrec')==1);                 end
                    if ~isempty(find(contains(meta_datasets,'par1d')==1))
                        metadata1d = deblank(h5read(EISCAThdf5file{nnn},'/metadata/par1d')); 
                        data1d     = double(h5read(EISCAThdf5file{nnn},'/data/par1d'));
                        nr1         = find(strcmp(metadata1d(1,:),'nrec')==1);                end    
                    if ~isempty(find(contains(meta_datasets,'par2d')==1))
                        metadata2d = deblank(h5read(EISCAThdf5file{nnn},'/metadata/par2d'));
                        data2d     = double(h5read(EISCAThdf5file{nnn},'/data/par2d'));       end    
                    if ~isempty(find(contains(meta_datasets,'names')==1))
                        names       = deblank(h5read(EISCAThdf5file{nnn},'/metadata/names')); end    
                    utime = h5read(EISCAThdf5file{nnn},'/data/utime');
                    
                    if exist('nr1','var') && ~isempty(nr1)
                        lrecs = data1d(:,nr1);
                    elseif exist('nr0','var') && ~isempty(nr0)
                        lrec  = data0d(nr0);
                        lrecs = lrec*ones(length(utime(:,1)),1);
                    end
                    
                    utime_tmp = [];
                    for nnr = 1:length(lrecs)
                        utime_tmp = [utime_tmp; repmat(utime(nnr,:),lrecs(nnr),1)]; 
                    end
                    utime = utime_tmp;
                    
                    v = {'vi_east' 'vi_north' 'vi_up'};
                    var_v = {'var_vi_east' 'var_vi_north' 'var_vi_up'};
                    crossvar = {'vi_crossvar_12' 'vi_crossvar_23' 'vi_crossvar_13'};
                    pos = {'lat' 'lon' 'h'};
                    Vg = []; Vvar = []; Crossvar = []; Vpos = []; 
                    for vv = 1:3
                        if (exist('nr1','var') && ~isempty(nr1)) || (exist('nr0','var') &&  ~isempty(nr0) && data0d(nr0)>1)   % data in 2d
                            aa = find(strcmp(metadata2d(1,:),v{vv})==1);
                            bb = find(strcmp(metadata2d(1,:),var_v{vv})==1);
                            cc = find(strcmp(metadata2d(1,:),crossvar{vv})==1);
                            dd = find(strcmp(metadata2d(1,:),pos{vv})==1);
                            if isempty(dd)
                                dd = find(strcmp(metadata1d(1,:),pos{vv})==1);
                                if isempty(dd)
                                    dd = find(strcmp(metadata0d(1,:),pos{vv})==1);
                                    Vpos = [Vpos data0d(dd)*ones(length(utime(:,1)),1)];
                                else
                                    Vpos  = [Vpos data1d(:,dd)]; 
                                end
                            else
                                Vpos  = [Vpos data2d(:,dd)]; 
                            end
                            Vg   = [Vg data2d(:,aa)];
                            Vvar = [Vvar data2d(:,bb)];
                            Crossvar = [Crossvar data2d(:,cc)];
                        else                                                        % data in 1d
                            aa = find(strcmp(metadata1d(1,:),v{vv})==1);
                            bb = find(strcmp(metadata1d(1,:),var_v{vv})==1);
                            cc = find(strcmp(metadata1d(1,:),crossvar{vv})==1);
                            dd = find(strcmp(metadata1d(1,:),pos{vv})==1);
                            if isempty(dd)
                                dd = find(strcmp(metadata0d(1,:),pos{vv})==1);
                                Vpos = [Vpos data0d(dd)*ones(length(utime(:,1)),1)];
                            else
                                Vpos  = [Vpos data1d(:,dd)]; 
                            end
                            Vg  = [Vg data1d(:,aa)];
                            Vvar = [Vvar data1d(:,bb)];
                            Crossvar = [Crossvar data1d(:,cc)];
                        end
                    end
                    
                    Vpos(:,3) = Vpos(:,3)/1e3;    % m --> km 
                    Vgv   = [Vvar Crossvar];  
                    Vdate = [timeconv(utime(:,1),'unx2mat')';timeconv(utime(:,2),'unx2mat')'];
                    ee= find(strcmp(names(1,:),'name_expr')==1);
                    ff = find(strcmp(names(1,:),'name_ant')==1);
                    name_expr = names{2,ee};
                    name_ant  = names{2,ff};
                    [~,file,~] = fileparts(EISCAThdf5file{nnn});
                    plotfilename = file(8:end);
                    vfile = [storepath{nnn} '/' plotfilename];
                    save([vfile '.mat'],'Vdate','Vpos','Vg','Vgv','name_expr','name_ant');

                    if contains(name_expr,'cp1')
                        plotfilename_orig = plotfilename;
                        f = strfind(plotfilename_orig,'_V');
                        plottype = 'tVm3';
                        altlim = [90 160 500];
                        region = ['E';'F'];
                        for pp = 1:2
                             hh = find(Vpos(:,3)>=altlim(pp) & Vpos(:,3)<altlim(pp+1));
                             if hh
                                plotfilename = [plotfilename_orig(1:f-1) '_' region(pp) plotfilename_orig(f:end)];
                                plotfile = [storepath{nnn} '/' plotfilename];
                                close
                                efield([vfile '.mat'],plottype,[altlim(pp) altlim(pp+1)])
                                fig = gcf; axObjs = fig.Children; dataObjs = axObjs(3).Children;
                                if isempty(dataObjs)
                                    warning('Empty plot, figure was not saved.')
                                else
                                    print('-dpdf',[plotfile '.pdf'])
                                    npdf = npdf + 1;
                                    pdf_forHDF5(npdf) = {[plotfilename '.pdf']};
                                    print('-dpng256',[plotfile '.png'])
                                    image2hdf5([plotfile '.png'],EISCAThdf5file{nnn});
                                    list_fortar(length(list_fortar)+1,:) = {[plotfile '.png']};
                                    list_linksfortar(length(list_linksfortar)+1,:) = {[plotfile '.pdf']};
                                    insert_exif(gcf,plotfile,{'pdf' 'png'})
                                end
                            end
                        end
                    else
                        if contains(name_expr,'cp2')
                            plottype = 'tVm3';
                        else
                            plottype = 'pVm';
                        end
                        close
                        efield([vfile '.mat'],plottype);
                        fig = gcf; axObjs = fig.Children; dataObjs = axObjs(3).Children;
                        if isempty(dataObjs)
                             warning('Empty plot, figure was not saved.')
                        else
                            print('-dpdf',[vfile '.pdf'])
                            npdf = npdf + 1;
                            pdf_forHDF5(npdf) = {[plotfilename '.pdf']};
                            print('-dpng256',[vfile '.png'])
                            image2hdf5([vfile '.png'],EISCAThdf5file{nnn});
                            list_fortar(length(list_fortar)+1,:) = {[vfile '.png']};
                            list_linksfortar(length(list_linksfortar)+1,:) = {[vfile '.pdf']};
                            insert_exif(gcf,vfile,{'pdf' 'png'})
                        end
                    end
                    if npdf
                        strds2hdf5(EISCAThdf5file{nnn},'/figures','figure_links',pdf_forHDF5')
                        npdf = 0; clear pdf_forHDF5
                    end
                    delete([vfile '.mat'])
                end
            else
                input = EISCAThdf5file{nnn};
                metadata0d = h5read(EISCAThdf5file{nnn},'/metadata/par0d');
                aa = find(strcmp(deblank(metadata0d(1,:)),'nrec'));
                if aa
                    data0d = h5read(EISCAThdf5file{nnn},'/data/par0d');
                    nrec = data0d(aa);
                    if nrec == 1 || nrec > 5
                        vizugo = 1;
                    else
                        warning(['No figures, and nrec = ' num2str(nrec) ' so no new figures generated.'])
                    end
                else
                    vizugo = 1;
                end
            end

            if vizugo
                vizu('new',input,'HQ')
                file = vizu('save','24hrplt');
                newpng = [file '.png'];
                newpdf = [file '.pdf'];
                image2hdf5(newpng,EISCAThdf5file{nnn});
                movefile(newpng,storepath{nnn})
                [~,figname] = fileparts(newpng);
                list_fortar(length(list_fortar)+1,:) = {fullfile(storepath{nnn},[figname '.png'])};
                strds2hdf5(EISCAThdf5file{nnn},'/figures','figure_links',{[figname '.pdf']})
                list_linksfortar(length(list_linksfortar)+1,:) = {newpdf};
            end
        end  
        %copyfile(data_files{ii},storepath{nnn})
    end
    pathparts = strsplit(storepath{nnn},filesep);
    tar(fullfile(storepath{nnn},[pathparts{end} '.tar.gz']),list_fortar);
    for dpng = 1:length(list_fortar)        % .pngs stored at storepath are deleted after being tared.
        filepath = fileparts(list_fortar{dpng}); 
        if strcmp(filepath,storepath{nnn})
            delete(list_fortar{dpng})
        end
    end
    if ~isempty(list_junkfortar)
        tar(fullfile(storepath{nnn},[pathparts{end} '_suppl.tar.gz']),list_junkfortar);
    end
    if ~isempty(list_linksfortar)
        tar(fullfile(storepath{nnn},[pathparts{end} '_linked.tar.gz']),list_linksfortar);
        for dl= 1:length(list_linksfortar)
             delete(list_linksfortar{dl})
        end
    end
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



function metacompl(OldHdf5file,EISCAThdf5file)
% metacompl(OldHdf5file,EISCAThdf5file)
% If experiment metadata is missing in EISCAThdf5file, extract experiment 
% metadata from older HDF5-file (OldHdf5file) and insert in the new 
% HDF5-file (EISCAThdf5file)

inform1 = h5info(OldHdf5file,'/Metadata');
metavar = {inform1.Datasets.Name}';
if ~isempty(find(strcmp(metavar,'Experiment Parameters')))
    exprpar = h5read(OldHdf5file,'/Metadata/Experiment Parameters');
else
    warning('No field named "Experiment Parameters"')
    return
end
exprparnames = cellstr(exprpar.name');
exprparvalues = cellstr(exprpar.value');

for ii = 1:length(exprparnames)
   experiment.(char(regexprep(exprparnames(ii),' ','_'))) = {exprparvalues{ii}};
end

inform2 = h5info(EISCAThdf5file,'/metadata');
metagroups = {inform2.Datasets.Name}';
if isempty(find(strcmp(metagroups,'/metadata/software/gfd'))) && isempty(find(strcmp(metagroups,'/metadata/software/experiment')))
    fields = fieldnames(experiment);
    for ii = 1:length(fields)
        strds2hdf5(EISCAThdf5file,'/metadata/software/experiment',char(fields(ii)),experiment.(char(fields(ii))))
    end
else
    display('/metadata/software/gfd or /metadata/software/experiment already exists in the new HDF5 file.')
    return
end


function pdffile = eps2pdf(epsfile) 
% Generating a pdf-file from an eps or ps file.

if nargin<1
    error('No eps or ps-file as input')
end

[dirpath,epsfilename,ext] = fileparts(epsfile);
if ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')
    error('The input file is not .eps or .ps')
end

epsfile = [dirpath filesep epsfilename];

pngor = '820x580';
gd=fullfile(matlabroot,'sys','ghostscript',filesep);
gsbin=fullfile(gd,'bin',lower(computer),'gs');
gsinc=sprintf('-I%sps_files -I%sfonts',gd,gd);
if ~exist(gsbin,'file'), gsbin='gs'; gsinc=[]; end
unix(sprintf('%s -I%sps_files -I%sfonts -dNOPAUSE -q -sDEVICE=pdfwrite -sPAPERSIZE=a4 -sOutputFile=%s.pdf %s%s </dev/null >/dev/null',gsbin,gsinc,pngor,epsfile,epsfile,ext));
pdffile = [epsfile '.pdf'];