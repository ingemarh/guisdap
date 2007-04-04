/**************************************************************
TLAN2PS Version 3.0
        Plusieurs cannaux par frequence (experience des lignes plasma).
        Decodage UHF et VHF.
        Bug on ALTCP1 (V 2.1) fixed
        Tested on SNEJA2, HIFREJA, ATLTCP1
        Bug on asko worksation fixed 
        Round off error on channel-frequency calculation eliminated
2.31 News:
        Channel-Frequency correspondance is now stored under PS_SETFREQ for
         remote analysis purpose.
        All Radar Controler Programs are decoded. If ther is more than one
         rcp, the pat_PS files are labeled "name_expr_#rcpnpat_PS.m",
         otherwise the single "name_exprpat_PS.m" file is created.
3.0  News:
        Bugsss fixed when looping on rcpn
        SINTRAN syntax handled by the TLAN interpretor
        Remote calibration fully decoded (VCAL & HCAL)
        File name extension can be either lower or upper case.
**************************************************************/

/******** INCLUDES ********/
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "etlan.h"

/***********************************************/
/******** DEFINES ********/

#define BUFFLEN 90
char buff[BUFFLEN];

#define NCHMX           8       /* Maximum number of channels */
#define NFREQMX         16      /* Maximum number of frequencies */
#define LMXCHSTATUS	20      /* Maximum length of the channel status */

#define PSHEADER        "initialize\n"
#define PSFOOT          "filetrailer\n"
#define MESSAGE "Channel-frequency correspondence read from"

#define abs(A)          ((A<0)?-A:A)
#define min(A,B)	(A<B?A:B)

#ifdef NAMDOS	/* DOS Naming Convention flag */
 #define NAMMX		13
 #define PSEXT		"pat_ps.m"
#else
 #define NAMMX		20
 #define EXP_DEF	"HIALTT"
 #define PSEXT		"pat_PS.m"
#endif

/***********************************************/
/******** GLOBALS ********/
//FILE	*fplog=*stdout;

char    elan_nam[NAMMX],
        tlan_nam[NAMMX],
        ps_nam[NAMMX];

int	rcpn=1;		/* Default radar controler program treated */

int fverch[NCHMX+1];		/* frequency versus channel */
int chverf[NFREQMX][NCHMX+1];   /* channel versus frequency */
                                /* Now an array NFREQMX x NCHMX+1. The first
element chverf[f][0] is the number of channels connected to frequency f.
Next elements are those channels.*/

static  char ch_status[NCHMX+2][LMXCHSTATUS];
static  float  ch_time[NCHMX+2];

/******** RECEIVER *******/
typedef struct {/* receiver */
char	type[8];/* Receiver type */
float   f0,     /* Transmitted frequency 0 */
        df;     /* Transmitted frequency increment */
int     nmx;    /* Number of Transmitted frequencies */
float   lo1,    /* First local oscillator */
        if1,    /* Center Frequency of the first IF filter */
        if2;    /* Second IF frequency */
} Receiver;

Receiver reclist[]={
 "UHF85A",929.5,.5,16, 813.5,120,30,
 "UHF85B",929.5,.5,16,1053.5,120,30,
 "UHF90" ,929.5,.5,16, 811.5,120,30,
 "UHF91" ,927.5,.5,16, 812  ,120,30,
 "VHF91" ,222.4,.2,16, 294  , 70,30,
 NULL    ,  0  , 0, 0,   0  ,  0, 0,
};

/* RFC : Typical value the RF frequency */
/* LO  : Local Oscillator frequency */
/* IF  : Inetermediate frequency */
/* unmix(RFC,LO,IF) : Actual RF frequency */
#define unmix(RFC,LO,IF)          ((RFC>LO)?LO+IF:LO-IF)

/******
Decode la frequence Fn.
- Return -1 if the IFn is out of the first IF filter if not
- Return the Transmitted frequency index = Mixer index.
******/
int Fn(float lo1, float lo2)
{
int	n;
float   if1n,bw,fn;
Receiver *rec;
/* First find the Receiver that match the first lo */
for(rec=reclist;rec->type!=NULL;rec++)if(rec->lo1==lo1)break;
if(rec->type==NULL)
{
 fprintf(stderr,"Can't find receiver spec\n");
 exit(1);
}
/* First IF */
if1n=unmix(rec->if1,lo2,rec->if2);
/* Test if if1n goes through the first IF filter */
bw=rec->nmx*rec->df*1.5; /* Band Width of the filter */
if(if1n<rec->if1-bw/2||if1n>rec->if1+bw/2)return(-1);
/* Transmitted frequency */
fn=unmix(rec->f0,rec->lo1,if1n);
/* Transmitted frequency index */
n=(int)(abs((rec->f0-fn)/rec->df)+.1);
#ifdef DEBUG
 printf("  lo1=%g --> f0=%g df=%g\n",lo1,rec->f0,rec->df);
 printf("  lo2=%g if2=%g --> if1n=%g\n",lo2,rec->if2,if1n);
 printf("  lo1=%g if1n=%g --> fn=%g\n",lo1,if1n,fn);
 printf("  fn=%g f0=%g df=%g --> n=%d\n",fn,rec->f0,rec->df,n);
#endif
/* Test if n is in the Transmitted frequency index ensemble */
return((n>=0&&n<rec->nmx)?n:-1);
}

/******
Ecrit dans le fichier de sortie l'etat du cannal c.
******/
void setch(FILE *ps, int c, float t,char *s)
{
int i=c+1;
#ifdef DEBUG_TLAN
 fprintf(stdout,"setch : AT %g channel %d %s\n",t,c,s);
#endif
if(strcmp(s,ch_status[i]))
{
 fprintf(ps,"[%d %g %g] ;PS_%s\n",c,ch_time[i],t,ch_status[i]);
 if (strlen(s)>=LMXCHSTATUS)
 {
  fprintf(stderr,"setch: Status String too long\n");
  fprintf(stderr,"       Increase define LMXCHSTATUS\n");
  exit(1);
 }
 ch_time[i]=t;
 strcpy(ch_status[i],s);
}
}

/*********************************/
/****** ELAN INTERPRETOR *********/
/*********************************/
void elan2ps(FILE *elan, FILE *ps)
{
char    filter[20];
ElanCmd *cmd;
float   lo1,lo2,dt,df;
int     i,ch,f;
int     exitELAN=0;

fseek(elan,0L,0);
fprintf(stdout,"*** Elan interpretor\n");

while((cmd=NextElanCmd(elan))!=NULL)
{
 switch(cmd->code)
 {
  case 105:
   sscanf(cmd->arg,"%d%f",&ch,&dt);
   fprintf(ps,"[%d %4.1f] ;PS_SETADCINT\n",ch,dt);
#ifdef DEBUG_ELAN
  fprintf(stdout,"elan2ps : channel %d, ADCINT = %g us\n",ch,dt);
#endif
   break;
  case 202: exitELAN=1; break;
  case 302:
   sscanf(cmd->arg,"%f",&lo1);
#ifdef DEBUG_ELAN
  fprintf(stdout,"elan2ps : LO1 = %g MHz\n",lo1);
#endif
   break;
  case 305:
   sscanf(cmd->arg,"%d%f",&ch,&lo2);
   f=Fn(lo1,lo2);
   fprintf(ps,"[%d %d] ;PS_SETFREQ\n",ch,f);
   if(f>=0&&f<NFREQMX) fverch[ch]=f;
   else
   {fprintf(stderr,"Frequency index out of limits f=%d\n",f);exit(1);}
#ifdef DEBUG_ELAN
 fprintf(stdout,"elan2ps : channel %d, LO2 = %5.1f KHz ==> F# = %d\n",ch,lo2,f);
#endif
   break;
  case 307:
   sscanf(cmd->arg,"%d%s%f",&ch,filter,&df);
   fprintf(ps,"[%d %4.1f] ;PS_%c%cFILT\n",ch,df,filter[0],filter[1]);
   break;
  case 315:
   sscanf(cmd->arg,"%d",&ch);
   sscanf(strchr(cmd->arg,'F')+1,"%d",&f);
   fprintf(ps,"[%d %d] ;PS_SETFREQ\n",ch,f);
   if(f>=0&&f<NFREQMX) fverch[ch]=f;
   else
   {fprintf(stderr,"Frequency index out of limits f=%d\n",f);exit(1);}
#ifdef DEBUG_ELAN
 fprintf(stdout,"elan2ps : channel %d, F# = %d\n",ch,f);
#endif
   break;
  case 505: exitELAN=1; break;
 }
 if(exitELAN) break;
}

for(ch=1;ch<=NCHMX;ch++)
{
 if((f=fverch[ch])>=0)
 {
  chverf[f][0]++;
  chverf[f][chverf[f][0]]=ch;
 }
}

#ifdef DEBUG_ELAN
 fprintf(stdout,"elan2ps : frequency-channel(s) correspondance\n");
 for(f=0;f<NFREQMX;f++)
 if(chverf[f][0]>0)
 {
  fprintf(stdout,"          F%-2d received on channel(s)",f);
  for(i=1;i<=chverf[f][0];i++)fprintf(stdout," %d",chverf[f][i]);
  fprintf(stdout,"\n");
 }
#endif

}

/*********************************/
/****** TLAN INTERPRETOR *********/
/*********************************/
void tlan2ps(FILE *tlan, FILE *ps)
{
char    *ptr,
        status[8],
        phase[8]="PHA";
int     Tcal,rf=0;
float   timerep,time;
int     ch,f,phi;
float   lo1,lo2,dt,df;
TlanCmd	*cmd;
int     i,iscan;

fprintf(stdout,"*** Tlan interpretor on Radar Controler Program # %d\n",rcpn);

/************ REP ******************************************************/
/* First find the REP command for the radar controler program # rcpn   */
fprintf(stdout,"   1- Looking for REP\n");
timerep=getrep(rcpn,tlan);

/************ CAL == Channel 0 ***************/
/* Then the CALibration commands*/
fprintf(stdout,"   2- Calibration\n");
gotorcp(rcpn,tlan);
while((cmd=NextTlanCmd(tlan))!=NULL)
{
 ptr=cmd->name;
 time=cmd->time;
 if(strstr(ptr,"REP"))break;
 else if(strstr(ptr,"CAL"))
 {
  if(strstr(ptr,"CALOFF"))strcpy(status,"EMPTY");
  else if(strstr(ptr,"CALON")||strstr(ptr,"VCAL")||strstr(ptr,"HCAL"))
   strcpy(status,"CALON");
  else
  {
   sscanf(ptr+3,"%d",&Tcal);
   if(Tcal>0)strcpy(status,"CALON");
   else      strcpy(status,"EMPTY");
  }
  setch(ps,0,time,status);
 }
}
setch(ps,0,timerep,"REP");

/************ Channels != 0 ***************/
fprintf(stdout,"   3- Transmit & receive\n");
gotorcp(rcpn,tlan);
while((cmd=NextTlanCmd(tlan))!=NULL)
{
 ptr=cmd->name;
 time=cmd->time;
 if(strstr(ptr,"REP"))break;

/************ TRANS ***************/

    if(ptr[0]=='F'&&sscanf(ptr+1,"%d",&i)==1)
    {
     f=i;
#ifdef DEBUG_TLAN
 fprintf(stdout,"tlan2ps : Current freq %d\n",f);
#endif
    }
    else if(strstr(ptr,"RFON"))
    {
     rf=1;
     for(i=1;i<=chverf[f][0];i++)setch(ps,chverf[f][i],time,phase);
    }
    else if(strstr(ptr,"RFOFF"))
    {
     rf=0;
     for(i=1;i<=chverf[f][0];i++)setch(ps,chverf[f][i],time,"EMPTY");
    }
    else if(strstr(ptr,"PHA"))
    {
     sscanf(ptr,"%*3c%d",&phi);
     if(phi)strcpy(phase,"PHAN");
     else   strcpy(phase,"PHA");
     if(rf)for(i=1;i<=chverf[f][0];i++)setch(ps,chverf[f][i],time,phase);
    }

/************ RECEV ***************/

    else if(strstr(ptr,"ALLOFF"))
     for(ch=1;ch<=8;ch++)setch(ps,ch,time,"EMPTY");
    else if(strstr(ptr,"CH")&&
(iscan=sscanf(ptr,"%*2c%d%s",&ch,status))>0)
    {
      if(iscan==2&&!strcmp(status,"OFF"))
       setch(ps,ch,time,"EMPTY");
      else
       setch(ps,ch,time,"ON");
    }

/************ CH -1 ***************/

    else if(strstr(ptr,"MATCHFIL"))setch(ps,-1,time,"MATCHFIL");
    else if(strstr(ptr,"BYPASMAT"))setch(ps,-1,time,"EMPTY");

}

/************ REP ***************/

#ifdef DEBUG
 printf("AT %g REP\n",timerep);
#endif
for(ch=-1;ch<=8;ch++)if(ch!=0)setch(ps,ch,timerep,"REP");
}

/*********************************/
/****** INITIALISATION ***********/
/*********************************/
char *beg_main(FILE **elan,FILE **tlan, int argc, char *argv[])
{
char    *exp_nam;

if(argc<2)exp_nam=EXP_DEF;
else      exp_nam=argv[1];

if(argc>2) sscanf(argv[2],"%d",&rcpn);

#ifdef NAMDOS
 if(!*(elan=fopen("elan.txt","r")
 {printf("No file elan.txt !\n");exit(1);}
 if(!*(tlan=fopen("tlan.txt","r")
 {printf("No file tlan.txt !\n");exit(1);}
#else
 sprintf(elan_nam,"%s.elan",exp_nam);
 *elan=fopen(elan_nam,"r");
 if(!*elan)
 {
  sprintf(elan_nam,"%s.ELAN",exp_nam);
  *elan=fopen(elan_nam,"r");
 }
 if(!*elan)
 {fprintf(stderr,"No file %s.elan or %s.ELAN !\n",exp_nam,exp_nam);exit(1);}
 sprintf(tlan_nam,"%s.tlan",exp_nam);
 *tlan=fopen(tlan_nam,"r");
 if(!*tlan)
 {
  sprintf(tlan_nam,"%s.TLAN",exp_nam);
  *tlan=fopen(tlan_nam,"r");
 }
 if(!*tlan)
 {fprintf(stderr,"No file %s.tlan or %s.TLAN !\n",exp_nam,exp_nam);exit(1);}
#endif

#ifdef QUIET
 stdout=fopen("/dev/null","w");
#endif

return(exp_nam);
}

FILE *psopen(char *exp_nam,int n)
{
 FILE *fp;
 int ch,f;
 if(n>0)
  sprintf(ps_nam,"%s_%d%s",exp_nam,n,PSEXT);
 else
  sprintf(ps_nam,"%s%s",exp_nam,PSEXT);
 fp=fopen(ps_nam,"w");
 fprintf(fp,"%s",PSHEADER);

 for(ch=0;ch<NCHMX;ch++)fverch[ch]=-1;
 for(f=0;f<NFREQMX;f++)chverf[f][0]=0;

 for(ch=0;ch<NCHMX+2;ch++)
 {
  ch_time[ch]=0;
  strcpy(ch_status[ch],"EMPTY");
 }

 return(fp);
}

/*********************************/
/****** FERMETURE FICHIERS *******/
/*********************************/
psclose(FILE *fp)
{
 fprintf(fp,"%s",PSFOOT);
 fclose(fp);
}

end_main(FILE *elan,FILE *tlan)
{
fclose(elan);
fclose(tlan);
}

/******************/
/****** MAIN ******/
/******************/
int main(int argc, char *argv[])
{
FILE    *elan;          /* Fichier ELAN */
FILE    *tlan;          /* Fichier TLAN */
FILE    *ps;            /* Fichier PS */
int     rcpN;
char *exp_nam;

exp_nam=beg_main(&elan,&tlan,argc,argv);
fprintf(stdout,"*** tlan2ps 3.0\n");
fprintf(stdout,"*** Count number of radar controler program\n");
rcpN=countrcp(tlan);
for(rcpn=1;rcpn<=rcpN;rcpn++)
{
 ps=psopen(exp_nam,rcpN>1?rcpn:0);
 elan2ps(elan,ps);
 tlan2ps(tlan,ps);
 psclose(ps);
}
end_main(elan,tlan);
} 
