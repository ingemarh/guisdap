function [impresp,t0] = get_impresp(firpar_file, p_dtau, do_plot)

%GET_IMPRESP - Get impulse response from ESR filter definition file.
%[IMPRESP, T0] = GET_IMPRESP(FIRDEF_FILE,P_DTAU,PLOT_FLAG)
%   The impulse response is returned with tap spacing of p_dtau usec,
%   interpolated from the actual impulse response. The first tap point
%   is in the interval [0,p_dtau], at time T0.
%   The IMPRESP is normalized to unit area.
%   Ex.
%   >> [impresp,t0] = get_impresp('w25d200.fir',2);

%   24-Jul-1998 Jm
%   23-Feb-2001 Jm: Handling of odd number opf taps changed
%   (c) EISCAT Scientific Association
%=========================================================================    

    [a,b]=fileparts(firpar_file);
    adc_rate=15;
    if b(1)=='w'
        adc_rate=10;
    elseif b(1)~='b'
        fprintf('Cannot tell ADC rate from fir file, setting %g MHz\n',adc_rate)
    end
    if nargin == 2
        do_plot = 0;
    elseif nargin < 2 | nargin > 3
        error('Wrong number of input arguments')
    end

    if ~isnumeric(p_dtau)
        error('Invalid argument -- p_dtau must be numeric')
    end

    [fir, f_dec, h_dec, h_order, msg] = get_fir(firpar_file); 
    if ~isempty(msg)
        error(msg)
    elseif firpar_file(1)=='b'
        adc_rate=15;
    end
    fir = fir/max(fir);

    hdf = hcic(h_order,h_dec); hdf = hdf/max(abs(hdf));

    fir2 = insertz(fir,h_dec - 1); fir2 = fir2/max(abs(fir2));

    ddf = conv(hdf,fir2); ddf = ddf/sum(ddf/adc_rate);

    t_ddf = (0:length(ddf)-1)/adc_rate;

% Find times (t_ip) for the interpolated points.

    tcenter = t_ddf(end)/2;
    a = p_dtau/2;
    t2 = tcenter - a;
    t3 = tcenter + a;
    n = floor(t2/p_dtau);
    t1 = t2 - n*p_dtau;
    t4 = t3 + n*p_dtau;
    %t_ip = t1:p_dtau:t4;
    t_ip = (0:round((t4-t1)/p_dtau))*p_dtau+t1;
 
    %h2 = interp1(t_ddf,ddf,t_ip,'*cubic');
    h2 = interp1(t_ddf,ddf,t_ip,'pchip');

    if do_plot
        %getfigure('get_impresp','white');
        t_hdf = (0:length(hdf)-1)/adc_rate;
        t_fir = h_dec*(0:length(fir)-1)/adc_rate;
        tlim = [0,t_ddf(end)];

        subplot(2,1,1)
        plot(t_hdf,hdf,t_fir,fir)
        set(gca,'XLimMode','manual','XLim',tlim,'Box','off')
        [path,name,ext,ver] = fileparts(firpar_file);
        title(name)
        ylabel('HDF & FIR')

        subplot(2,1,2)
        plot(t_ddf,ddf,'r',t_ip,h2,'ro')
        set(gca,'XLimMode','manual','XLim',tlim,'Box','off')
        ylabel('DDF')
        xlabel('time [\mus]')
    end
 
    if nargout > 0
        impresp = h2;
        t0 = t1;
    end


function [key,val] = token(s)
% expected syntax: key value1 [value2]

    key = [];
    val = [];

    [key,s] = strtok(s);
    if isempty(key), return, end

    if isempty(s), return, end
 
    [val1, s] = strtok(s);
    if isempty(val1), return, end
 
    val1 = sscanf(val1,'%i');
    if isempty(val1), return, end
    val(1) = val1;
 
    if ~strcmp(key,'TAP'), return, end
 
    if isempty(s), val = []; return, end

    [val2, s] = strtok(s);
    if isempty(val2), val = []; return, end
    val2 = sscanf(val2,'%i');
    if isempty(val2), val = []; return, end
    val(2) = val2;
 
function s = strip(ss)
%---------------------
    %STRIP remove trailing and leading blanks
    %   S= STRIP(SS)

    % Nay 29, 97 Jm

    s= deblank(ss);
    if isempty(s), return, end

    t= strtok(s);
    k= findstr(s,t);
    if k > 1
        s(1:k-1)= [];
    end


function [fir, f_dec, h_dec, h_order, msg] = get_fir(firpar_file)

    fir = [];
    hec = [];
    h_order = [];
    msg = '';

    [fid,msg] = fopen(firpar_file);
    if fid < 1
        msg = [firpar_file ': ', msg];
        return
    end
 
    k = 0;
    while 1
        line = fgetl(fid);
        if ~isstr(line), break,end
        line = strip(line);
        if ~isempty(line) & line(1) ~= '%'
            k= k+1; L{k} = line;
        end
    end

    fclose(fid);
 
    if k == 0
        msg = [firpar_file ' appears empty'];
        return
    end

    if isempty(strmatch('FIRPAR_VS 0.1',L,'exact'))
        msg = 'Illegal format - ''FIRPAR_VS 0.1'' not found';
        return
    end

    key_list = {'TAP'; 'H_STAGES'; 'H_DRATE'; 'F_DRATE'; 'F_TAPS'; 'F_ESYM'};
    val_list = -1 * ones(length(key_list),1);
    tap = [];

    for k = 1:length(L)
        [key,val] = token(L{k});
        if isempty(val)
            msg = [ L{k} ' - invalid format' ];
            return
        end
        key_index = strmatch(key,key_list,'exact');
        if ~isempty(key_index)
            if key_index == 1
                tap(val(1)+1) = val(2);
            elseif key_index > 1
                val_list(key_index) = val;
            end
        end
    end
 
    if ~tap
        msg = 'TAPs not defined';
        return
    else
        negs = find(tap >= 2^19 );
        tap(negs) = tap(negs) - 2^20;
    end

    f_esym = val_list(6);
    h_order = val_list(2);
    h_dec  = val_list(3)+1;
    f_dec = val_list(4)+1;
    f_taps = val_list(5)+1;
 
    if f_esym == 1
        if rem(f_taps,2) == 0                % 23 Feb 2001 Jm
            taps = [tap tap(end:-1:1)];      %
        else                                 %
            taps = [tap tap(end-1:-1:1)];    %   
        end                                  %
    elseif f_esym == 0
        taps = [tap tap(end-1:-1:1)];
    else
        msg = '''F_ESYM'' not defined';
        return
    end

    if f_taps ~= length(taps)
        msg = sprintf('Number of TAPs (%d) does not match number of defined TAPs (%d)',...
                       f_taps,length(taps));
        return
    end

    fir = taps;


function  h = hcic(K,M)
%--------------------------------------------------
%   h= hcic(K,M)
%   impulse response of the HSP43220 CIC filter,
%   order= K, decimation = M.
%   ("infinite" precision, scaling as in Deci.Mate)
%--------------------------------------------------

    h1= ones(1,M);
    h= h1;
    for i= 1:(K-1)
       h= conv(h,h1);
    end

    h= h/fftceil(sum(h));


function y= insertz(x,M)
% y= INSERTZ(x,M)
%    insert M zeros between elements of x

   if M < 1, y = x; return, end

   L= (length(x)-1)*(M+1) + 1;
   y= zeros(1,L);
   y( rem(0:L-1,M+1) == 0 ) = x;


function n = fftceil(m)
    if nargin ~= 1
        error('Invalid number of input argumements')
    end

    n = 2^(nextpow2(m));
