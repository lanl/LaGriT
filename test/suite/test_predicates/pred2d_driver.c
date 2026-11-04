/* test_predicates driver, 2D: reads 8 doubles per test (a, b, c, p) from stdin, prints
   lg_incircle2d and lg_orient2d signs */
#include <stdio.h>
#include <stdint.h>
void lg_incircle2d_(const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,int64_t*);
void lg_orient2d_(const double*,const double*,const double*,const double*,const double*,const double*,int64_t*);
int main(){ double p[8]; int64_t s,o;
 while (fread(p,8,8,stdin)==8){ lg_incircle2d_(p,p+1,p+2,p+3,p+4,p+5,p+6,p+7,&s); lg_orient2d_(p,p+1,p+2,p+3,p+4,p+5,&o); printf("%lld %lld\n",(long long)s,(long long)o);} return 0;}
