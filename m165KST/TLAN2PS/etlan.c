#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "etlan.h"

extern FILE *fplog;

#define BUFFLEN 90
static char buff[BUFFLEN];

/***********************************************/
/****** NextLine
Idem fgets sauf que:
        1) Remplace les ',' par des SPACE de telle sorte qu'on puisse
s'y retrouver.
        2) Arrete la ligne au premier commentaire trouve. Un commentaire
commence par '?' ou '%'. Si la ligne ne contient que du commentaire, passe a
la ligne suivante.
        3) Retourne un pointeur sur le premier mot de la ligne trouvee. Si
aucune ligne n'est trouvee retourne le pointeur NULL.
******/
static char *NextLine(char *buff, int bufflen, FILE *fp)
{
char *ptr,*debut,*fin;
if((debut=fgets(buff,bufflen,fp))==NULL)return(NULL);
debut=debut+strspn(debut," ");
fin=strpbrk(debut,"%?\n");
if(debut==fin)return(NextLine(buff,bufflen,fp));
if(fin==NULL)fin=debut+strlen(debut);
else fin--;
for(ptr=debut;ptr<=fin;ptr++) if(*ptr==',')*ptr=' ';
for(ptr=fin;ptr>=debut;ptr--)if(*ptr!=' ')break;
*(ptr+1)='\0';
return(debut);
}

/***** ELAN LIBRARY *****/
static ElanCmd elanlist[]={
 105,"SET-ADC-INTERVAL",NULL,
 202,"START-RADAR-CONTROLLER",NULL,
 302,"SET-LO1",NULL,
 305,"SET-LO2",NULL,
 307,"SET-FILTER",NULL,
 315,"SET-FREQUENCY-ADJUSTED-LO2",NULL,
 505,"STOP-RECORDING-DATA",NULL,
 0  ,NULL                 ,NULL,
};

ElanCmd *NextElanCmd(FILE *fp)
{
 char *line,*tok,*ptr,*arg,
      abrv[80];
 int nmatch=0;
 ElanCmd *cmd=elanlist,*match=NULL;

 if((line=NextLine(buff,BUFFLEN,fp))==NULL)return(NULL);
#ifdef DEBUG_ELAN
  fprintf(stdout,"NextElanCmd: %s\n    --> ",line);
#endif

 line=strtok(line," ");
 arg=strtok(NULL,"");
 while(cmd->code!=0)
 {
  ptr=cmd->name;
  tok=strtok(strcpy(abrv,line),"-");
  while(tok!=NULL)
  {
   if(strstr(ptr,tok)!=ptr)break;
   tok=strtok(NULL,"-");
   if((ptr=strchr(ptr,'-'))==NULL)break;
   ptr++;
  }
  if(tok==NULL){match=cmd;match->arg=arg;nmatch++;}
  if(nmatch>1)
  {
   fprintf(stdout,"NextTlanCmd: Ambiguous command\n");
   exit(1);
  }
  cmd++;
 }
#ifdef DEBUG_ELAN
 if(match!=NULL)
  fprintf(stdout,"%d %s %s\n",match->code,match->name,match->arg);
 else
  fprintf(stdout,"Not implemented!\n");
#endif
 return(match!=NULL?match:cmd);
}

/***** TLAN LIBRARY *****/
static float basetime=0;

TlanCmd *NextTlanCmd(FILE *fp)
{
static TlanCmd cmd={0,NULL};
char *ptr;
float t;
if((ptr=strtok(NULL," "))==NULL)
{
 ptr=NextLine(buff,BUFFLEN,fp);
 ptr=strtok(ptr," ");
}
if(ptr==NULL)return(NULL);

if(strstr(ptr,"AT")==ptr)
{
 ptr=strtok(NULL," ");
 sscanf(ptr,"%f",&t);t=t+basetime;
 cmd.time=t;
 return(NextTlanCmd(fp));
}
else if(strstr(ptr,"SETTCR")==ptr)
{
 ptr=strtok(NULL," ");
 sscanf(ptr,"%f",&basetime);
 return(NextTlanCmd(fp));
}
else
{
 cmd.name=ptr;
#ifdef DEBUG_TLAN
 fprintf(stderr,"NextTlanCmd: AT %g %s\n",cmd.time,cmd.name);
#endif
 return(&cmd);
}
}

float getrep(int rcpn,FILE *fp)
{
int n=0;
float rep;
TlanCmd *cmd;

fprintf(stdout,"getrep: Radar Controler program #%d REP=",rcpn);
fseek(fp,0L,0);
while(n!=rcpn)
{
 while((cmd=NextTlanCmd(fp))!=NULL)if(strstr(cmd->name,"REP"))break;
 if(cmd==NULL)
 {
  fprintf(stdout,"Not found!\n");
  exit(1);
 }
 else{rep=cmd->time;n++;}
}
fprintf(stdout,"%g\n",rep);
return(rep);
}

void gotorcp(int rcpn,FILE *fp)
{
TlanCmd *cmd;
int     n;

fprintf(stdout,"gotorcp: Radar Controler program #%d",rcpn);
fseek(fp,0L,0);
basetime=0;
for(n=1;n<rcpn;n++)
{
 while((cmd=NextTlanCmd(fp))!=NULL)if(strstr(cmd->name,"REP"))break;
 if(cmd==NULL)
 {
  fprintf(stderr," not found!\n",rcpn);
  exit(1);
 }
}
fprintf(stdout," found\n");
}

int countrcp(FILE *fp)
{
TlanCmd *cmd;
int     n=0;

fprintf(stdout,"countrcp :");
fseek(fp,0L,0);
while((cmd=NextTlanCmd(fp))!=NULL)
 if(strstr(cmd->name,"REP"))
 {
  n++;
  fprintf(stdout," %d",n);
 }
fprintf(stdout,"\n");
return(n);
}
