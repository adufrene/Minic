#include<stdio.h>
#include<stdlib.h>
long _function(long EV_n)
{
long EV_i;
long EV_j;
if ((EV_n<=0))
{
return 0;
}
EV_i = 0;
while ((EV_i<(EV_n*EV_n)))
{
EV_j = (EV_i+EV_n);
printf("%ld ", (long)EV_j);
EV_i = (EV_i+1);
}
return _function((EV_n-1));
}
long _main()
{
long EV_num;
scanf("%ld", &EV_num);
_function(EV_num);
printf("%ld\n", (long)0);
return 0;
}
int main(void)
{
   return _main();
}

