global iHandle
fprintf('*************************************************************\n')
fprintf(' Data will be integrated by Nigel Wade''s integration front end\n')
fprintf('*************************************************************\n')
if ~exist('source')
  global source strategy
end
if isempty(source)
  global source strategy
  t1=sprintf('start %d/%d/%d %d:%d:%d;',analysis_start([3 2 1 4:6]));
  t2=sprintf('end %d/%d/%d %d:%d:%d;',analysis_end([3 2 1 4:6]));
  if a_realtime
    rr=' realtime;';
  else
    rr=[];
  end
  if ~isempty(recurse)
    rr=['recurse ' recurse ';' rr];
  end
  source=sprintf('gupmat {%s %s directory %s; %s}',t1,t2,data_path,rr);
  strategy=sprintf('filter time {%s %s} filter power {min %g kW;}',t1,t2,a_txlim/1000);
  if a_integr
    strategy=[strategy sprintf(' integrate time {period %d;}',a_integr)];
  else
    strategy=[strategy ' integrate move {}'];
  end
  clear rr t1 t2
end
[status,iHandle] = PI_init(source,strategy,'');
