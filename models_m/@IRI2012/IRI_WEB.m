function [ a,b,context ] = IRI_WEB( context,jmag,jf,alati,along,iyyyy,mmdd,iut,...
  dhour,height,h_tec_max,ivar,vbeg,vend,vstp,b )
%IRI_WEB iri_web
%        subroutine iri_web(jmag,jf,alati,along,iyyyy,mmdd,iut,dhour,
%                height,h_tec_max,ivar,vbeg,vend,vstp,a,b)
%-----------------------------------------------------------------------        
% changes:
%       11/16/99 jf(30) instead of jf(17)
%       10/31/08 outf, a, b (100 -> 500)
%
%-----------------------------------------------------------------------        
% input:   jmag,alati,along,iyyyy,mmdd,dhour  see IRI_SUB
%          height  height in km
%          h_tec_max  =0 no TEC otherwise upper boundary for integral
%          iut     =1 for UT       =0 for LT
%          ivar    =1      altitude
%                  =2,3    latitude,longitude
%                  =4,5,6  year,month,day
%                  =7      day of year
%                  =8      hour (UT or LT)
%          vbeg,vend,vstp  variable range (begin,end,step)
% output:  a       similar to outf in IRI_SUB
%          b       similar to oarr in IRI_SUB
%
%          numstp  number of steps; maximal 1000
%-----------------------------------------------------------------------        

%        dimension   outf(20,1000),oar(100),oarr(100),a(20,1000)
%        dimension   xvar(8),b(100,1000)
%        logical     jf(50)
  hstart = 50.; % km
  doTEC=(jf(IRI2012.Ne_COMPUTED_SW) || jf(IRI2012.NeNi_COMPUTED_SW)) && ...
    (h_tec_max > hstart);

  if ivar == IRI2012.ALTITUDE_VARIATION
    xhour=mod(dhour,24.0)+iut*IRI2012.UT_INDICATOR;
    [a,oarr,context] = context.IRI_SUB(jf,jmag,alati,along,iyyyy,mmdd,xhour, ...
                vbeg,vend,vstp,b);
    if doTEC
      [tec,tect,~] = context.IRI_TEC (hstart,h_tec_max,IRI2012.BEST_TEC);
      oarr(IRI2012.TEC_OUT)=tec;
      oarr(IRI2012.TEC_TOP_OUT)=tect;
    end
    for i=1:IRI2012.numAdditionalResults
      b(i,1)=oarr(i);
    end
  else
    oarr = zeros(IRI2012.numAdditionalResults,1);
    nummax=1000;
    numstp=floor((vend-vbeg)/vstp)+1;
    if (numstp > nummax)
      numstp=nummax;
    end
    a = zeros(IRI2012.numResults,numstp);
    if height <= 0.0
      height=100;
    end
    xvar(IRI2012.LATITUDE_VARIATION)=alati;
    xvar(IRI2012.LONGITUDE_VARIATION)=along;
    xvar(IRI2012.YEAR_VARIATION)=iyyyy;
    if mmdd > 0 % month, day input
      xvar(IRI2012.MONTH_VARIATION)=floor(mmdd/100);
      xvar(IRI2012.DAY_VARIATION)=mmdd-xvar(IRI2012.MONTH_VARIATION)*100;
      [ ~,~,xvar(IRI2012.DOY_VARIATION) ] = IRI2012.MODA( -1,...
        xvar(IRI2012.YEAR_VARIATION),xvar(IRI2012.MONTH_VARIATION),xvar(IRI2012.DAY_VARIATION) );
    else % DOY input
      xvar(IRI2012.DOY_VARIATION)=abs(mmdd);
      [ xvar(IRI2012.MONTH_VARIATION),xvar(IRI2012.DAY_VARIATION) ] = IRI2012.MODA( 1,...
        xvar(IRI2012.YEAR_VARIATION),xvar(IRI2012.DOY_VARIATION) );
    end
    xvar(IRI2012.HOUR_VARIATION)=dhour;

    xvar(ivar)=vbeg;

    for i=1:numstp
      for iii=1:IRI2012.numAdditionalResults
        oarr(iii)=b(iii,i);
      end
      if ivar == IRI2012.DOY_VARIATION
      [ xvar(IRI2012.MONTH_VARIATION), ...
        xvar(IRI2012.DAY_VARIATION), ...
        xvar(IRI2012.DOY_VARIATION),~, ...
        xvar(IRI2012.YEAR_VARIATION) ] = IRI2012.MODA( 1,xvar(IRI2012.YEAR_VARIATION), ...
                                                         xvar(IRI2012.DOY_VARIATION) );
      else
      [ xvar(IRI2012.MONTH_VARIATION), ...
        xvar(IRI2012.DAY_VARIATION), ...
        xvar(IRI2012.DOY_VARIATION),~, ...
        xvar(IRI2012.YEAR_VARIATION) ] = IRI2012.MODA( -1,xvar(IRI2012.YEAR_VARIATION), ...
                                                          xvar(IRI2012.MONTH_VARIATION), ...
                                                          xvar(IRI2012.DAY_VARIATION) );
      end
      alati=xvar(IRI2012.LATITUDE_VARIATION);
      along=xvar(IRI2012.LONGITUDE_VARIATION);
      iyyyy=floor(xvar(IRI2012.YEAR_VARIATION));
      mmdd=-abs(xvar(IRI2012.DOY_VARIATION));
      dhour=mod(xvar(IRI2012.HOUR_VARIATION),24.0)+iut*IRI2012.UT_INDICATOR;
      [outf,oarr,context] = context.IRI_SUB(jf,jmag,alati,along,iyyyy,mmdd,dhour, ...
              height,height,1.,oarr);
      if doTEC
        [tec,tect,~] = context.IRI_TEC (hstart,h_tec_max,IRI2012.BEST_TEC);
        oarr(IRI2012.TEC_OUT)=tec;
        oarr(IRI2012.TEC_TOP_OUT)=tect;
      end
      for ii=1:IRI2012.numResults
        a(ii,i)=outf(ii,1);
      end
      for ii=1:IRI2012.numAdditionalResults
        b(ii,i)=oarr(ii);
      end
      xvar(ivar)=xvar(ivar)+vstp;
      if ivar == IRI2012.HOUR_VARIATION || ivar == IRI2012.DAY_VARIATION % hour or day increments
        n = floor(xvar(IRI2012.HOUR_VARIATION)/24);
        xvar(IRI2012.DAY_VARIATION) = xvar(IRI2012.DAY_VARIATION) + n; % day increase
        xvar(IRI2012.HOUR_VARIATION) = xvar(IRI2012.HOUR_VARIATION) - n*24;
      end
    end
  end

end

