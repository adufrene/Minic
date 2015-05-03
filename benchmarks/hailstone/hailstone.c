#include<stdio.h>
#include<stdlib.h>
long _mod(long EV_a,long EV_b)
{
return (EV_a-((EV_a/EV_b)*EV_b));
}
void _hailstone(long EV_n)
{
while (1)
{
printf("%ld ", (long)EV_n);
if ((_mod(EV_n, 2)==1))
{
EV_n = ((3*EV_n)+1);
}
else
{
EV_n = (EV_n/2);
}
if ((EV_n<=1))
{
printf("%ld\n", (long)EV_n);
return ;
}
}
}
long _main()
{
long EV_num;
scanf("%ld", &EV_num);
_hailstone(EV_num);
return 0;
}
int main(void)
{
   return _main();
}

