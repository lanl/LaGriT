/* test_predicates driver, 3D: reads 15 doubles per test (a, b, c, d, e) from stdin, prints
   lg_insphere3d and lg_orient3d signs */
#include <stdio.h>
#include <stdint.h>
void lg_insphere3d_(const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,int64_t*);
void lg_orient3d_(const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,const double*,int64_t*);
int main(){ double p[15]; int64_t s,o;
 while (fread(p,8,15,stdin)==15){
  lg_insphere3d_(p,p+1,p+2,p+3,p+4,p+5,p+6,p+7,p+8,p+9,p+10,p+11,p+12,p+13,p+14,&s);
  lg_orient3d_(p,p+1,p+2,p+3,p+4,p+5,p+6,p+7,p+8,p+9,p+10,p+11,&o);
  printf("%lld %lld\n",(long long)s,(long long)o);}
 return 0;}
