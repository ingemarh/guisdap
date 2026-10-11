       subroutine irinit(ipath,iplen)
       integer ipath(*)
       character path*128
       common /iripath/path,lenpath
       lenpath=iplen
       do 3 i=1,lenpath
3      path(i:i)=char(ipath(i))
       call read_ig_rz
       call readapf107
       return
       end
