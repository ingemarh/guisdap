; setup lib path and name
; write default to variables if not provided.
case !version.os of
    'linux':ext='so'
    'sunos':ext='so'
    'Win32':ext='dll'
endcase
lib_name='../onera_desp_lib_'+!version.OS+'_'+$
   !version.ARCH+'.'+ext

print,''
print,'Testing make_lstar ...'
ntime=1l
kext=5l
options=lonarr(5)
options(0)=1l
options(1)=0l
sysaxes=2l

iyear=lonarr(100000)
iyear(0)=1998l
idoy=lonarr(100000)
idoy(0)=100l
UT=dblarr(100000)
UT(0)=3600.d0

x1=dblarr(100000)
x1(0)=7.d0
x2=dblarr(100000)
x2(0)=0.d0
x3=dblarr(100000)
x3(0)=0.d0

maginput=dblarr(25,100000)

Lm=dblarr(100000)
Lstar=dblarr(100000)
Blocal=dblarr(100000)
Bmin=dblarr(100000)
XJ=dblarr(100000)
MLT=dblarr(100000)


result = call_external(lib_name, 'make_lstar_',$
ntime,kext,options,sysaxes,iyear,idoy,ut, x1,x2,x3,$
maginput,lm,lstar,blocal,bmin,xj,mlt, /f_value)

print,'         Your distribution | Expected value'
print,'Lm =     ',strcompress(string(Lm(0)),/REMOVE_ALL),'         | 6.6159597'
print,'L* =     ',strcompress(string(Lstar(0)),/REMOVE_ALL),'         | 5.9074070'
print,'Blocal = ',strcompress(string(Blocal(0)),/REMOVE_ALL),'         | 104.54581'
print,'Bmin =   ',strcompress(string(Bmin(0)),/REMOVE_ALL),'         | 104.15972'


print,''
print,'Testing make_lstar_shell_splitting ...'
ntime=1l
npa=6l
kext=5l
options=lonarr(5)
options(0)=1l
options(1)=0l
sysaxes=2l

iyear=lonarr(100000)
iyear(0)=1998l
idoy=lonarr(100000)
idoy(0)=100l
UT=dblarr(100000)
UT(0)=3600.d0

x1=dblarr(100000)
x1(0)=7.d0
x2=dblarr(100000)
x2(0)=0.d0
x3=dblarr(100000)
x3(0)=0.d0

alpha=dblarr(25)
alpha(0)=5.d0
alpha(1)=20.d0
alpha(2)=40.d0
alpha(3)=60.d0
alpha(4)=80.d0
alpha(5)=90.d0

maginput=dblarr(25,100000)

Lm=dblarr(100000,25)
Lstar=dblarr(100000,25)
Blocal=dblarr(100000,25)
Bmin=dblarr(100000)
XJ=dblarr(100000,25)
MLT=dblarr(100000)

result = call_external(lib_name, 'make_lstar_shell_splitting_',$
ntime,Npa,kext,options,sysaxes,iyear,idoy,ut, x1,x2,x3,alpha,$
maginput,lm,lstar,blocal,bmin,xj,mlt, /f_value)

expectLstar=[6.3440260d0,6.3377330d0,6.2429260d0,6.0852281d0,5.9219087d0,5.9074070d0]
expectBloc=[13739.408d0,892.84257d0,252.97473d0,139.37175d0,107.78491d0,104.54581d0]
print,'         Your distribution | Expected value'
for i=0,5 do print,'L* = ',strcompress(string(lstar(0,i)),/REMOVE_ALL),'             | ',strcompress(string(expectLstar(i)),/REMOVE_ALL)
for i=0,5 do print,'blocal = ',strcompress(string(blocal(0,i)),/REMOVE_ALL),'         | ',strcompress(string(expectBloc(i)),/REMOVE_ALL)

print,''
print,'Testing get_field ...'
iyear=1998l
idoy=100l
UT=3600.d0

x1=7.d0
x2=0.d0
x3=0.d0
maginput=dblarr(25)
Bgeo=dblarr(3)
Bl=0.d0
result = call_external(lib_name, 'get_field_', kext,options,sysaxes,iyear,idoy,ut, $
x1,x2,x3, maginput,Bgeo, Bl,  /f_value)
print,'         Your distribution | Expected value'
print,'blocal = ',strcompress(string(bl),/REMOVE_ALL),'         | 104.54581'
end
