#include<stdio.h>
#include<stdlib.h>
long _wait(long EV_waitTime)
{
while ((EV_waitTime>0))
{
EV_waitTime = (EV_waitTime-1);
}
return 0;
}
long _power(long EV_base,long EV_exponent)
{
long EV_product;
EV_product = 1;
while ((EV_exponent>0))
{
EV_product = (EV_product*EV_base);
EV_exponent = (EV_exponent-1);
}
return EV_product;
}
long _recursiveDecimalSum(long EV_binaryNum,long EV_decimalSum,long EV_recursiveDepth)
{
long EV_tempNum;
long EV_base;
long EV_remainder;
if ((EV_binaryNum>0))
{
EV_base = 2;
EV_tempNum = (EV_binaryNum/10);
EV_tempNum = (EV_tempNum*10);
EV_tempNum = (EV_binaryNum-EV_tempNum);
if ((EV_tempNum==1))
{
EV_decimalSum = (EV_decimalSum+_power(EV_base, EV_recursiveDepth));
}
return _recursiveDecimalSum((EV_binaryNum/10), EV_decimalSum, (EV_recursiveDepth+1));
}
return EV_decimalSum;
}
long _convertToDecimal(long EV_binaryNum)
{
long EV_recursiveDepth;
long EV_decimalSum;
EV_recursiveDepth = 0;
EV_decimalSum = 0;
return _recursiveDecimalSum(EV_binaryNum, EV_decimalSum, EV_recursiveDepth);
}
long _main()
{
long EV_number;
long EV_waitTime;
scanf("%ld", &EV_number);
EV_number = _convertToDecimal(EV_number);
EV_waitTime = (EV_number*EV_number);
while ((EV_waitTime>0))
{
_wait(EV_waitTime);
EV_waitTime = (EV_waitTime-1);
}
printf("%ld\n", (long)EV_number);
return 0;
}
int main(void)
{
   return _main();
}

