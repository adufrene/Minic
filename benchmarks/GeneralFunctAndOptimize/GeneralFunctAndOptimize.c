#include<stdio.h>
#include<stdlib.h>
struct EV_IntHolder
{
long EV_num;
};
long EV_interval;
long EV_end;
long _multBy4xTimes(struct EV_IntHolder * EV_num,long EV_timesLeft)
{
if ((EV_timesLeft<=0))
{
return EV_num->EV_num;
}
EV_num->EV_num = (4*EV_num->EV_num);
_multBy4xTimes(EV_num, (EV_timesLeft-1));
return EV_num->EV_num;
}
void _divideBy8(struct EV_IntHolder * EV_num)
{
EV_num->EV_num = (EV_num->EV_num/2);
EV_num->EV_num = (EV_num->EV_num/2);
EV_num->EV_num = (EV_num->EV_num/2);
}
long _main()
{
long EV_start;
long EV_countOuter;
long EV_countInner;
long EV_calc;
long EV_tempAnswer;
long EV_tempInterval;
struct EV_IntHolder * EV_x;
long EV_uselessVar;
long EV_uselessVar2;
EV_x = (struct EV_IntHolder*)malloc(sizeof(struct EV_IntHolder));
EV_end = 1000000;
scanf("%ld", &EV_start);
scanf("%ld", &EV_interval);
printf("%ld\n", (long)EV_start);
printf("%ld\n", (long)EV_interval);
EV_countOuter = 0;
while ((EV_countOuter<50))
{
EV_countInner = 0;
while ((EV_countInner<=EV_end))
{
EV_calc = ((((((((((1*2)*3)*4)*5)*6)*7)*8)*9)*10)*11);
EV_countInner = (EV_countInner+1);
EV_x->EV_num = EV_countInner;
EV_tempAnswer = EV_x->EV_num;
_multBy4xTimes(EV_x, 2);
_divideBy8(EV_x);
EV_tempInterval = (EV_interval-1);
EV_uselessVar = (EV_tempInterval<=0);
if ((EV_tempInterval<=0))
{
EV_tempInterval = 1;
}
EV_countInner = (EV_countInner+EV_tempInterval);
}
EV_countOuter = (EV_countOuter+1);
}
printf("%ld\n", (long)EV_countInner);
printf("%ld\n", (long)EV_calc);
return 0;
}
int main(void)
{
   return _main();
}

