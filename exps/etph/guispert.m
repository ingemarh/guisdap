% guispert.m: special experiment specific hacks
%
% See also: GUISPERT GUIZARD
%

if d_date>datenum(2024,01,13)
    callen=30;sglen=10070;bklen=1610;
    sggat=513;bkgat=90;maxlag=19;

    sgstart=callen;
    bgstart=callen+sglen+bklen+callen;
    bkstart=callen+sglen+bklen+callen+sglen;

    d_data((1:sglen)+sgstart)=d_data((1:sglen)+sgstart)-d_data((1:sglen)+bgstart);
    indsg=0;indbk=0;
    for j=0:maxlag
        d_data((1:(sggat-j))+sgstart+indsg)=d_data((1:(sggat-j))+sgstart+indsg)+median(d_data((1:(bkgat-j))+bkstart+indbk));
        indsg=indsg+sggat-j;
        indbk=indbk+bkgat-j;
    end
end