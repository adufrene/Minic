#include<stdio.h>
#include<stdlib.h>
long _computeFib(long EV_input)
{
if ((EV_input==0))
{
return 0;
}
else
{
if ((EV_input<=2))
{
return 1;
}
else
{
return (_computeFib((EV_input-1))+_computeFib((EV_input-2)));
}
}
}
long _main()
{
long EV_input;
scanf("%ld", &EV_input);
printf("%ld\n", (long)_computeFib(EV_input));
return 0;
}
int main(void)
{
   return _main();
}

