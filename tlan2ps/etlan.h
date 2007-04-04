typedef struct {
int code;
char *name,
     *arg;
} ElanCmd;

ElanCmd *NextElanCmd(FILE *fp);

typedef struct {
float time;
char *name;
} TlanCmd;

TlanCmd *NextTlanCmd(FILE *fp);
float getrep(int rcpn,FILE *fp);
void gotorcp(int rcpn,FILE *fp);
int countrcp(FILE *fp);
