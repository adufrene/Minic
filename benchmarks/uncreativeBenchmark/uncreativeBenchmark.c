#include<stdio.h>
#include<stdlib.h>
struct EV_LameStruct
{
long EV_one;
long EV_two;
long EV_three;
long EV_four;
long EV_five;
long EV_six;
long EV_seven;
long EV_eight;
long EV_nine;
};
long _countTotal(struct EV_LameStruct * EV_what)
{
long EV_total;
EV_total = 0;
EV_total = ((((((((EV_what->EV_one+EV_what->EV_two)+EV_what->EV_three)+EV_what->EV_four)+EV_what->EV_five)+EV_what->EV_six)+EV_what->EV_seven)+EV_what->EV_eight)+EV_what->EV_nine);
return EV_total;
}
long _printRecursive(long EV_num,long EV_rec)
{
if ((EV_rec==0))
{
return EV_num;
}
else
{
printf("%ld\n", (long)EV_num);
EV_rec = (EV_rec-1);
return _printRecursive(EV_num, EV_rec);
}
}
long _convertBinary(long EV_number)
{
if ((EV_number==0))
{
return 0;
}
else
{
if ((EV_number==1))
{
return 1;
}
else
{
if ((EV_number==2))
{
return 10;
}
else
{
if ((EV_number==3))
{
return 11;
}
else
{
if ((EV_number==4))
{
return 100;
}
else
{
if ((EV_number==5))
{
return 101;
}
else
{
if ((EV_number==6))
{
return 110;
}
else
{
if ((EV_number==7))
{
return 111;
}
else
{
if ((EV_number==8))
{
return 1000;
}
else
{
return 1001;
}
}
}
}
}
}
}
}
}
}
long _main()
{
long EV_current;
long EV_count;
long EV_total;
struct EV_LameStruct * EV_digits;
EV_digits = (struct EV_LameStruct*)malloc(sizeof(struct EV_LameStruct));
EV_count = 0;
EV_digits->EV_one = 0;
EV_digits->EV_two = 0;
EV_digits->EV_three = 0;
EV_digits->EV_four = 0;
EV_digits->EV_five = 0;
EV_digits->EV_six = 0;
EV_digits->EV_seven = 0;
EV_digits->EV_eight = 0;
EV_digits->EV_nine = 0;
scanf("%ld", &EV_current);
while ((EV_current!=0))
{
if ((EV_current==1))
{
EV_digits->EV_one = (EV_digits->EV_one+1);
}
else
{
if ((EV_current==2))
{
EV_digits->EV_two = (EV_digits->EV_two+1);
}
else
{
if ((EV_current==3))
{
EV_digits->EV_three = (EV_digits->EV_three+1);
}
else
{
if ((EV_current==4))
{
EV_digits->EV_four = (EV_digits->EV_four+1);
}
else
{
if ((EV_current==5))
{
EV_digits->EV_five = (EV_digits->EV_five+1);
}
else
{
if ((EV_current==6))
{
EV_digits->EV_six = (EV_digits->EV_six+1);
}
else
{
if ((EV_current==7))
{
EV_digits->EV_seven = (EV_digits->EV_seven+1);
}
else
{
if ((EV_current==8))
{
EV_digits->EV_eight = (EV_digits->EV_eight+1);
}
else
{
EV_digits->EV_nine = (EV_digits->EV_nine+1);
}
}
}
}
}
}
}
}
scanf("%ld", &EV_current);
}
printf("%ld\n", (long)_countTotal(EV_digits));
_printRecursive(_convertBinary(1), EV_digits->EV_one);
_printRecursive(_convertBinary(2), EV_digits->EV_two);
_printRecursive(_convertBinary(3), EV_digits->EV_three);
_printRecursive(_convertBinary(4), EV_digits->EV_four);
_printRecursive(_convertBinary(5), EV_digits->EV_five);
_printRecursive(_convertBinary(6), EV_digits->EV_six);
_printRecursive(_convertBinary(7), EV_digits->EV_seven);
_printRecursive(_convertBinary(8), EV_digits->EV_eight);
_printRecursive(_convertBinary(9), EV_digits->EV_nine);
return 0;
}
int main(void)
{
   return _main();
}

