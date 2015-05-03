#include<stdio.h>
#include<stdlib.h>
struct EV_simple
{
long EV_one;
};
struct EV_foo
{
long EV_bar;
long EV_cool;
struct EV_simple * EV_simp;
};
struct EV_foo * EV_globalfoo;
void _tailrecursive(long EV_num)
{
struct EV_foo * EV_unused;
if ((EV_num<=0))
{
return ;
}
EV_unused = (struct EV_foo*)malloc(sizeof(struct EV_foo));
_tailrecursive((EV_num-1));
}
long _add(long EV_x,long EV_y)
{
return (EV_x+EV_y);
}
void _domath(long EV_num)
{
struct EV_foo * EV_math1;
struct EV_foo * EV_math2;
long EV_tmp;
EV_math1 = (struct EV_foo*)malloc(sizeof(struct EV_foo));
EV_math1->EV_simp = (struct EV_simple*)malloc(sizeof(struct EV_simple));
EV_math2 = (struct EV_foo*)malloc(sizeof(struct EV_foo));
EV_math2->EV_simp = (struct EV_simple*)malloc(sizeof(struct EV_simple));
EV_math1->EV_bar = EV_num;
EV_math2->EV_bar = 3;
EV_math1->EV_simp->EV_one = EV_math1->EV_bar;
EV_math2->EV_simp->EV_one = EV_math2->EV_bar;
while ((EV_num>0))
{
EV_tmp = (EV_math1->EV_bar*EV_math2->EV_bar);
EV_tmp = ((EV_tmp*EV_math1->EV_simp->EV_one)/EV_math2->EV_bar);
EV_tmp = _add(EV_math2->EV_simp->EV_one, EV_math1->EV_bar);
EV_tmp = (EV_math2->EV_bar-EV_math1->EV_bar);
EV_num = (EV_num-1);
}
free(EV_math1);
free(EV_math2);
}
void _objinstantiation(long EV_num)
{
struct EV_foo * EV_tmp;
while ((EV_num>0))
{
EV_tmp = (struct EV_foo*)malloc(sizeof(struct EV_foo));
free(EV_tmp);
EV_num = (EV_num-1);
}
}
long _ackermann(long EV_m,long EV_n)
{
if ((EV_m==0))
{
return (EV_n+1);
}
if ((EV_n==0))
{
return _ackermann((EV_m-1), 1);
}
else
{
return _ackermann((EV_m-1), _ackermann(EV_m, (EV_n-1)));
}
}
long _main()
{
long EV_a;
long EV_b;
long EV_c;
long EV_d;
long EV_e;
scanf("%ld", &EV_a);
scanf("%ld", &EV_b);
scanf("%ld", &EV_c);
scanf("%ld", &EV_d);
scanf("%ld", &EV_e);
_tailrecursive(EV_a);
printf("%ld\n", (long)EV_a);
_domath(EV_b);
printf("%ld\n", (long)EV_b);
_objinstantiation(EV_c);
printf("%ld\n", (long)EV_c);
printf("%ld\n", (long)_ackermann(EV_d, EV_e));
return 0;
}
int main(void)
{
   return _main();
}

