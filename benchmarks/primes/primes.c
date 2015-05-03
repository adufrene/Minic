#include<stdio.h>
#include<stdlib.h>
long _isqrt(long EV_a)
{
long EV_square;
long EV_delta;
EV_square = 1;
EV_delta = 3;
while ((EV_square<=EV_a))
{
EV_square = (EV_square+EV_delta);
EV_delta = (EV_delta+2);
}
return ((EV_delta/2)-1);
}
long _prime(long EV_a)
{
long EV_max;
long EV_divisor;
long EV_remainder;
if ((EV_a<2))
{
return 0;
}
else
{
EV_max = _isqrt(EV_a);
EV_divisor = 2;
while ((EV_divisor<=EV_max))
{
EV_remainder = (EV_a-((EV_a/EV_divisor)*EV_divisor));
if ((EV_remainder==0))
{
return 0;
}
EV_divisor = (EV_divisor+1);
}
return 1;
}
}
long _main()
{
long EV_limit;
long EV_a;
scanf("%ld", &EV_limit);
EV_a = 0;
while ((EV_a<=EV_limit))
{
if (_prime(EV_a))
{
printf("%ld\n", (long)EV_a);
}
EV_a = (EV_a+1);
}
return 0;
}
int main(void)
{
   return _main();
}

