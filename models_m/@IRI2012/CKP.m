function [ ckp ] = CKP( ap )
%CKP Converts ap index into kp index
%
%        function ckp(ap)
%-----------------------------------------------------------------------
% Converts ap index (ap is integer variable varying from 0 to 400) into 
% kp index (xkp is real variable varying from 0 to 9). Using standard
% tables for deriving the 3-hourly ap index from the 3-hourly Kp index
% (e.g., http://www.ngdc.noaa.gov/stp/GEOMAG/kp_ap.shtml) 
%-----------------------------------------------------------------------

%        integer		ap,ap_array
%        real		kp_array,ap_log_array
%        dimension 	ap_array(28),kp_array(28),alap(28)
  persistent alap kp_array;
  if isempty(alap)
    ap_array = [0,2,3,4,5,6,7,9,12,15,18,22,27,32,39,48,56,67, ...
                       80,94,111,132,154,179,207,236,300,400];
    alap = log(ap_array);
    kp_array = zeros(length(ap_array),1);
    for i=2:length(ap_array)
      kp_array(i)=(i-1)/3.;
    end
  end
  ap = floor(ap);
  if(ap == 0)
    ckp=0.0;
    return;
  end
  if(ap == 1)
    ckp=kp_array(2)/2.;
    return;
  end
  if(ap < 8 && ap > 1)
    ckp=kp_array(ap);
    return;
  end

  xl_ap=log(ap);

  i=8;
  while i <= length(alap)
    if(xl_ap > alap(i))
      i=i+1;
    else
      break;
    end
  end

  slope=(kp_array(i)-kp_array(i-1))/(alap(i)-alap(i-1));

  ckp = kp_array(i) + slope * (xl_ap - alap(i));
		

end

