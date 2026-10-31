#!/bin/env python3
import read_guisdap_ph as e3 
from ctypes import *
from pathlib import Path
import numpy as np
import time
import os
import sys
import json

class hdfhead(Structure):
    _fields_ = [
        ('dt' , c_uint64 ),
        #('bc' , c_uint16 ),
        #('lc' , c_uint16 ),
        #('az' , c_float ),
        #('el' , c_float ),
        #('pw' , c_uint16 ),
        #('bw' , c_uint16 ),
        ('ipp', c_uint16 ),
        ('ws' , c_uint16 ),
        ('at' , c_float ),
        ('et' , c_float ),
        ('rf' , c_float ),
        ('ex' , c_wchar_p ),
        ('st' , c_wchar_p ),
]

class hdfdata(Structure):
    _fields_ = [
        ('i' , c_int16 ),
        ('r' , c_int16 ),
]

def gethead(f):
    if type(f)==str: f=open(f,'rb')
    head=e3.PhaHead()
    if f.readinto(head)!=sizeof(head):
        return None
    #print('%x %x'%(head.begflag,head.endflag))
    return head

def getheadm(f):
    s=gethead(f)
    return dict((field, getattr(s,field)) for field,_ in s._fields_)

def getIQ(f,TotalIQ):
    if type(f)==str: f=open(f,'rb')
    IQ=np.fromfile(f,dtype='int16',count=TotalIQ)
    return IQ

def flist(f,extra_samples=0):
    if type(f)==str: f=open(f,'rb')
    fend=f.name.split('-')[-1]
    tid=[]
    bc=[]
    lc=[]
    az=[]
    el=[]
    hdx=[]
    nd=[]
    ftell=f.tell()
    fsize=f.seek(0,os.SEEK_END)
    f.seek(ftell)
    mess=''
    TotalIQ=0
    while ftell<fsize:
        fpos=ftell
        h=gethead(f)
        if h==None:
            mess=' EOF'
            break
        #print(h.month,h.year,h.endflag)
        TotalIQ=h.nd
        #print(TotalIQ)
        if TotalIQ>=0:
            tid.append(h.dt/1e6)
            bc.append(h.bc)
            lc.append(h.lc)
            az.append(h.az)
            el.append(h.el)
            hdx.append(ftell)
            nd.append(TotalIQ)
            f.seek(TotalIQ*2*sizeof(c_int16),1)
        else:
            print(h.nd,h.ws,h.ipp,h.Pad[4])
        ftell=f.tell()
    return tid,bc,lc,az,el,hdx,nd,mess

def struct2dict(s):
    return dict((field, getattr(s,field)) for field,_ in s._fields_)

def exp(head):
    CodeWidth=head.pw
    if head.pw==CodeWidth:
        exp='ed%d'%head.pw
    else:
        exp='ed%dx%d'%(head.pw/CodeWidth,CodeWidth)
    return exp

def getcodes(f):
    jsonfile=Path(f).with_suffix('.json')
    metadata=json.loads(jsonfile.read_text(encoding="utf-8"))
    return metadata

def gethdf(f,hdx,nd):
    if type(f)==str: f=open(f,'rb')
    f.seek(hdx,0)
    h=gethead(f)
    TotalIQ=nd*2
    #jsonfile=Path(f.name).with_suffix('.json')
    #metadata=json.loads(jsonfile.read_text(encoding="utf-8"))
    #scale=float(metadata.get("scale_counts_per_input_unit"))
    #print(jsonfile,scale)
    #s2=scale/4096
    #IQ32=(getIQ(f,TotalIQ)/np.array(s2)).round()
    #IQ=IQ32.astype(c_int16)
    IQ=getIQ(f,TotalIQ).astype(c_float)
    #IQ//=np.array(64,dtype='int16')
    return h,IQ

def printstr(s):
    for field, field_type in s._fields_:
        if field[:3]!='Pad':
            print(f"{field}: {getattr(s,field)}")

def getheadmat(f):
    s=gethead(f)
    h=dict((field, getattr(s,field)) for field,_ in s._fields_)
    c=getcodes(f)
    #print(s)
    #c=dict((field, getattr(s,field)) for field,_ in s._fields_)
    return h,c

def gethdfmat(f,i,n):
    s,iq=gethdf(f,i,n)
    h=dict((field, getattr(s,field)) for field,_ in s._fields_)
    return h,iq

def guess(f):
    if type(f)==str: f=open(f,'rb')
    h=gethead(f)
    site=os.path.basename(f.name)[0:3]
    return dict(unx=h.dt/1e6,exp=exp(h),site=site,mode='Other')

def filelist(dir,nsite='I',tsky=False,savedir=None):
    import re,scipy
    filen=[]
    scale=[]
    mess=''
    nsites='NIO'
    sites=['ski','kai','kar']
    site=sites[nsites.find(nsite)]
    l=np.empty((0,8))
    pattern=r'^%s.*\.ph+$'%(site)
    for f in sorted(os.listdir(dir)):
        if re.match(pattern,f):
            fullfile=os.path.join(dir,f)
            tid,bc,lc,az,el,hdx,nd,mess=flist(fullfile)
            filen+=[f]
            fno=[len(filen)]*len(tid)
            fl=np.c_[fno,tid,bc,lc,az,el,hdx,nd]
            l=np.concatenate((l,fl),axis=0)
            if not mess: print('.',end='',flush=True)
            jsonfile=Path(fullfile).with_suffix('.json')
            if jsonfile.is_file():
                metadata=json.loads(jsonfile.read_text(encoding="utf-8"))
                scale.append(float(metadata.get("scale_counts_per_input_unit")))
            #print(jsonfile,scale)
        #else: print(f)
    print()
    if not filen:
        print('No data files found')
        return
    if tsky:
        sp=sys.path[0]
        sys.path.append(sp)
        import add_tsky_main
        tid=l[:,1]
        az=l[:,3]
        el=l[:,4]
        t408=add_tsky_main.matface(os.path.join(sp,'..','matfiles'),site,tid,az,el)
        l=np.c_[l,t408]
    #print(l.shape)
    
    fname=dir.replace(os.sep,'__')
    if savedir==None:
        if os.access(dir,os.W_OK):
            savedir=dir
            fname='filelist'+nsite+'.mat'
        else:
            savedir=os.path.join(os.path.expanduser('~'),'gup','mydata','filelist')
            fname=dir.replace(os.sep,'__')+site+'.mat'
            if not os.path.isdir(savedir): os.makedirs(savedir)
    outf=os.path.join(savedir,fname)
    listf=l[l[:,1].argsort()]
    #print(list[0,:],lsort[0,:])
    if len(scale):
        scipy.io.savemat(outf,{'folder':dir,'fname':filen,'scale':scale,'flist':listf},do_compression=True)
    else:
        scipy.io.savemat(outf,{'folder':dir,'fname':filen,'flist':listf},do_compression=True)
    return outf

def main():
    if len(sys.argv)<2:
        print("Usage: python script.py <binary file>")
        filename='/data/Guisdap_data/kaise_el63_20260917T122713Z_300s_polC_10us_satclean.ph'
        #sys.exit(1)
    else:
        filename=sys.argv[1]
    if os.path.isdir(filename):
        site='I'
        t408=False
        if len(sys.argv)>2: site=sys.argv[2]
        if len(sys.argv)>3: t408=sys.argv[3].lower() in ("true","t","yes","y","1")
        if len(sys.argv)>4: seq_no=sys.argv[4]
        filelist(filename,site,t408)
        exit(0)
    f=open(filename,'rb')

    tid,bc,lc,az,el,hdx,nd,mess=flist(f)
    print(len(tid),type(tid))
    f.seek(hdx[0],0)
    head=gethead(f)
    printstr(head)
    f.seek(0,0)
    print(guess(f))
    h,IQ=gethdf(f,hdx[0],nd[0])
    printstr(h)
    print(struct2dict(h))
    print(len(IQ)/2)
    #print(IQ)
    #DataIQ = complex(signalIQ(2:2:end), signalIQ(1:2:end));
    #for hi in hdx:
    #    gethdf(f,hi)

if __name__ == "__main__":
    main()
