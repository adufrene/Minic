#include<stdio.h>
#include<stdlib.h>
struct EV_linkedNums
{
long EV_num;
struct EV_linkedNums * EV_next;
};
struct EV_linkedNums * _getRands(long EV_seed,long EV_num)
{
long EV_cur;
long EV_prev;
struct EV_linkedNums * EV_curNode;
struct EV_linkedNums * EV_prevNode;
EV_cur = (EV_seed*EV_seed);
EV_prevNode = (struct EV_linkedNums*)malloc(sizeof(struct EV_linkedNums));
EV_prevNode->EV_num = EV_cur;
EV_prevNode->EV_next = NULL;
EV_num = (EV_num-1);
EV_prev = EV_cur;
while ((EV_num>0))
{
EV_cur = ((((EV_prev*EV_prev)/EV_seed)*(EV_seed/2))+1);
EV_curNode = (struct EV_linkedNums*)malloc(sizeof(struct EV_linkedNums));
EV_curNode->EV_num = EV_cur;
EV_curNode->EV_next = EV_prevNode;
EV_prevNode = EV_curNode;
EV_num = (EV_num-1);
EV_prev = EV_cur;
}
return EV_curNode;
}
long _calcMean(struct EV_linkedNums * EV_nums)
{
long EV_sum;
long EV_num;
long EV_mean;
EV_sum = 0;
EV_num = 0;
EV_mean = 0;
while ((EV_nums!=NULL))
{
EV_num = (EV_num+1);
EV_sum = (EV_sum+EV_nums->EV_num);
EV_nums = EV_nums->EV_next;
}
if ((EV_num!=0))
{
EV_mean = (EV_sum/EV_num);
}
return EV_mean;
}
long _approxSqrt(long EV_num)
{
long EV_guess;
long EV_result;
long EV_prev;
EV_guess = 1;
EV_prev = EV_guess;
EV_result = 0;
while ((EV_result<EV_num))
{
EV_result = (EV_guess*EV_guess);
EV_prev = EV_guess;
EV_guess = (EV_guess+1);
}
return EV_prev;
}
void _approxSqrtAll(struct EV_linkedNums * EV_nums)
{
while ((EV_nums!=NULL))
{
printf("%ld\n", (long)_approxSqrt(EV_nums->EV_num));
EV_nums = EV_nums->EV_next;
}
}
void _range(struct EV_linkedNums * EV_nums)
{
long EV_min;
long EV_max;
long EV_first;
EV_min = 0;
EV_max = 0;
EV_first = 1;
while ((EV_nums!=NULL))
{
if (EV_first)
{
EV_min = EV_nums->EV_num;
EV_max = EV_nums->EV_num;
EV_first = 0;
}
else
{
if ((EV_nums->EV_num<EV_min))
{
EV_min = EV_nums->EV_num;
}
else
{
if ((EV_nums->EV_num>EV_max))
{
EV_max = EV_nums->EV_num;
}
}
}
EV_nums = EV_nums->EV_next;
}
printf("%ld\n", (long)EV_min);
printf("%ld\n", (long)EV_max);
}
long _main()
{
long EV_seed;
long EV_num;
long EV_mean;
struct EV_linkedNums * EV_nums;
scanf("%ld", &EV_seed);
scanf("%ld", &EV_num);
EV_nums = _getRands(EV_seed, EV_num);
EV_mean = _calcMean(EV_nums);
printf("%ld\n", (long)EV_mean);
_range(EV_nums);
_approxSqrtAll(EV_nums);
return 0;
}
int main(void)
{
   return _main();
}

