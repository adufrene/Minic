#include<stdio.h>
#include<stdlib.h>
long EV_GLOBAL;
long EV_count;
long _fun2(long EV_x,long EV_y)
{
if ((EV_x==0))
{
return EV_y;
}
else
{
return _fun2((EV_x-1), EV_y);
}
}
long _fun1(long EV_x,long EV_y,long EV_z)
{
long EV_retVal;
EV_retVal = ((((5+6)-(EV_x*2))+(4/EV_y))+EV_z);
if ((EV_retVal>EV_y))
{
return _fun2(EV_retVal, EV_x);
}
else
{
if (((5<6)&&(EV_retVal<=EV_y)))
{
return _fun2(EV_retVal, EV_y);
}
}
return EV_retVal;
}
long _main()
{
long EV_i;
EV_i = 0;
scanf("%ld", &EV_i);
while ((EV_i<10000))
{
printf("%ld\n", (long)_fun1(3, EV_i, 5));
EV_i = (EV_i+1);
}
return 0;
}
int main(void)
{
   return _main();
}

