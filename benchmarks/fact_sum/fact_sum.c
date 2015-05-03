#include<stdio.h>
#include<stdlib.h>
long _sum(long EV_a,long EV_b)
{
return (EV_a+EV_b);
}
long _fact(long EV_n)
{
long EV_t;
if (((EV_n==1)||(EV_n==0)))
{
return 1;
}
if ((EV_n<=1))
{
return _fact(((-1)*EV_n));
}
EV_t = (EV_n*_fact((EV_n-1)));
return EV_t;
}
long _main()
{
long EV_num1;
long EV_num2;
long EV_flag;
EV_flag = 0;
while ((EV_flag!=(-1)))
{
scanf("%ld", &EV_num1);
scanf("%ld", &EV_num2);
EV_num1 = _fact(EV_num1);
EV_num2 = _fact(EV_num2);
printf("%ld\n", (long)_sum(EV_num1, EV_num2));
scanf("%ld", &EV_flag);
}
return 0;
}
int main(void)
{
   return _main();
}

