#include<stdio.h>
#include<stdlib.h>
long EV_global1;
long EV_global2;
long EV_global3;
long _constantFolding()
{
long EV_a;
EV_a = ((((((((((8*9)/4)+2)-(5*8))+9)-12)+6)-9)-18)+(((23*3)/23)*90));
return EV_a;
}
long _constantPropagation()
{
long EV_a;
long EV_b;
long EV_c;
long EV_d;
long EV_e;
long EV_f;
long EV_g;
long EV_h;
long EV_i;
long EV_j;
long EV_k;
long EV_l;
long EV_m;
long EV_n;
long EV_o;
long EV_p;
long EV_q;
long EV_r;
long EV_s;
long EV_t;
long EV_u;
long EV_v;
long EV_w;
long EV_x;
long EV_y;
long EV_z;
EV_a = 4;
EV_b = 7;
EV_c = 8;
EV_d = 5;
EV_e = 11;
EV_f = 21;
EV_g = (EV_a+EV_b);
EV_h = (EV_c+EV_d);
EV_i = (EV_e+EV_f);
EV_j = (EV_g+EV_h);
EV_k = (EV_i*EV_j);
EV_l = ((EV_e+(EV_h*EV_i))-EV_k);
EV_m = ((EV_h-(EV_i*EV_j))+(EV_k/EV_l));
EV_n = (((((EV_e+EV_f)+EV_g)+EV_h)+EV_i)-EV_j);
EV_o = ((((EV_n-EV_m)+EV_h)-EV_a)-EV_b);
EV_p = (((EV_k+EV_l)-EV_g)-EV_h);
EV_q = (((EV_b-EV_a)*EV_d)-EV_i);
EV_r = (((EV_l*EV_c)*EV_d)+EV_o);
EV_s = ((((EV_b*EV_a)*EV_c)/EV_e)-EV_o);
EV_t = (((EV_i+EV_k)+EV_c)-EV_p);
EV_u = ((EV_n+EV_o)-(EV_f*EV_a));
EV_v = (((EV_a*EV_b)-EV_k)-EV_l);
EV_w = ((EV_v-EV_s)-(EV_r*EV_d));
EV_x = (((EV_o-EV_w)-EV_v)-EV_n);
EV_y = (((EV_p*EV_x)*EV_t)-EV_w);
EV_z = ((EV_w-(EV_x*EV_y))+EV_k);
return EV_z;
}
long _deadCodeElimination()
{
long EV_a;
long EV_b;
long EV_c;
long EV_d;
long EV_e;
EV_a = 4;
EV_a = 5;
EV_a = 7;
EV_a = 8;
EV_b = 6;
EV_b = 9;
EV_b = 12;
EV_b = 8;
EV_c = 10;
EV_c = 13;
EV_c = 9;
EV_d = 45;
EV_d = 12;
EV_d = 3;
EV_e = 23;
EV_e = 10;
EV_global1 = 11;
EV_global1 = 5;
EV_global1 = 9;
return ((((EV_a+EV_b)+EV_c)+EV_d)+EV_e);
}
long _sum(long EV_number)
{
long EV_total;
EV_total = 0;
while ((EV_number>0))
{
EV_total = (EV_total+EV_number);
EV_number = (EV_number-1);
}
return EV_total;
}
long _doesntModifyGlobals()
{
long EV_a;
long EV_b;
EV_a = 1;
EV_b = 2;
return (EV_a+EV_b);
}
long _interProceduralOptimization()
{
long EV_a;
EV_global1 = 1;
EV_global2 = 0;
EV_global3 = 0;
EV_a = _sum(100);
if ((EV_global1==1))
{
EV_a = _sum(10000);
}
else
{
if ((EV_global2==2))
{
EV_a = _sum(20000);
}
if ((EV_global3==3))
{
EV_a = _sum(30000);
}
}
return EV_a;
}
long _commonSubexpressionElimination()
{
long EV_a;
long EV_b;
long EV_c;
long EV_d;
long EV_e;
long EV_f;
long EV_g;
long EV_h;
long EV_i;
long EV_j;
long EV_k;
long EV_l;
long EV_m;
long EV_n;
long EV_o;
long EV_p;
long EV_q;
long EV_r;
long EV_s;
long EV_t;
long EV_u;
long EV_v;
long EV_w;
long EV_x;
long EV_y;
long EV_z;
EV_a = 11;
EV_b = 22;
EV_c = 33;
EV_d = 44;
EV_e = 55;
EV_f = 66;
EV_g = 77;
EV_h = (EV_a*EV_b);
EV_i = (EV_c/EV_d);
EV_j = (EV_e*EV_f);
EV_k = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_l = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_m = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_n = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_o = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_p = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_q = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_r = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_s = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_t = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_u = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_v = ((((EV_b*EV_a)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_w = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_f*EV_e))+EV_g);
EV_x = (((EV_g+(EV_a*EV_b))+(EV_c/EV_d))-(EV_e*EV_f));
EV_y = ((((EV_a*EV_b)+(EV_c/EV_d))-(EV_e*EV_f))+EV_g);
EV_z = ((((EV_c/EV_d)+(EV_a*EV_b))-(EV_e*EV_f))+EV_g);
return (((((((((((((((((((((((((EV_a+EV_b)+EV_c)+EV_d)+EV_e)+EV_f)+EV_g)+EV_h)+EV_i)+EV_j)+EV_k)+EV_l)+EV_m)+EV_n)+EV_o)+EV_p)+EV_q)+EV_r)+EV_s)+EV_t)+EV_u)+EV_v)+EV_w)+EV_x)+EV_y)+EV_z);
}
long _hoisting()
{
long EV_a;
long EV_b;
long EV_c;
long EV_d;
long EV_e;
long EV_f;
long EV_g;
long EV_h;
long EV_i;
EV_a = 1;
EV_b = 2;
EV_c = 3;
EV_d = 4;
EV_i = 0;
while ((EV_i<1000000))
{
EV_e = 5;
EV_g = ((EV_a+EV_b)+EV_c);
EV_h = ((EV_c+EV_d)+EV_g);
EV_i = (EV_i+1);
}
return EV_b;
}
long _doubleIf()
{
long EV_a;
long EV_b;
long EV_c;
long EV_d;
EV_a = 1;
EV_b = 2;
EV_c = 3;
if ((EV_a==1))
{
EV_b = 20;
if ((EV_a==1))
{
EV_b = 200;
EV_c = 300;
}
else
{
EV_a = 1;
EV_b = 2;
EV_c = 3;
}
EV_d = 50;
}
return EV_d;
}
long _integerDivide()
{
long EV_a;
EV_a = 3000;
EV_a = (EV_a/2);
EV_a = (EV_a*4);
EV_a = (EV_a/8);
EV_a = (EV_a/16);
EV_a = (EV_a*32);
EV_a = (EV_a/64);
EV_a = (EV_a*128);
EV_a = (EV_a/4);
return EV_a;
}
long _association()
{
long EV_a;
EV_a = 10;
EV_a = (EV_a*2);
EV_a = (EV_a/2);
EV_a = (3*EV_a);
EV_a = (EV_a/3);
EV_a = (EV_a*4);
EV_a = (EV_a/4);
EV_a = (EV_a+4);
EV_a = (EV_a-4);
EV_a = (EV_a*50);
EV_a = (EV_a/50);
return EV_a;
}
long _tailRecursionHelper(long EV_value,long EV_sum)
{
if ((EV_value==0))
{
return EV_sum;
}
else
{
return _tailRecursionHelper((EV_value-1), (EV_sum+EV_value));
}
}
long _tailRecursion(long EV_value)
{
return _tailRecursionHelper(EV_value, 0);
}
long _unswitching()
{
long EV_a;
long EV_b;
EV_a = 1;
EV_b = 2;
while ((EV_a<1000000))
{
if ((EV_b==2))
{
EV_a = (EV_a+1);
}
else
{
EV_a = (EV_a+2);
}
}
return EV_a;
}
long _randomCalculation(long EV_number)
{
long EV_a;
long EV_b;
long EV_c;
long EV_d;
long EV_e;
long EV_i;
long EV_sum;
EV_i = 0;
EV_sum = 0;
while ((EV_i<EV_number))
{
EV_a = 4;
EV_b = 7;
EV_c = 8;
EV_d = (EV_a+EV_b);
EV_e = (EV_d+EV_c);
EV_sum = (EV_sum+EV_e);
EV_i = (EV_i*2);
EV_i = (EV_i/2);
EV_i = (3*EV_i);
EV_i = (EV_i/3);
EV_i = (EV_i*4);
EV_i = (EV_i/4);
EV_i = (EV_i+1);
}
return EV_sum;
}
long _iterativeFibonacci(long EV_number)
{
long EV_previous;
long EV_result;
long EV_count;
long EV_i;
long EV_sum;
EV_previous = (-1);
EV_result = 1;
EV_i = 0;
while ((EV_i<EV_number))
{
EV_sum = (EV_result+EV_previous);
EV_previous = EV_result;
EV_result = EV_sum;
EV_i = (EV_i+1);
}
return EV_result;
}
long _recursiveFibonacci(long EV_number)
{
if (((EV_number<=0)||(EV_number==1)))
{
return EV_number;
}
else
{
return (_recursiveFibonacci((EV_number-1))+_recursiveFibonacci((EV_number-2)));
}
}
long _main()
{
long EV_input;
long EV_result;
long EV_i;
scanf("%ld", &EV_input);
EV_i = 1;
while ((EV_i<EV_input))
{
EV_result = _constantFolding();
printf("%ld\n", (long)EV_result);
EV_result = _constantPropagation();
printf("%ld\n", (long)EV_result);
EV_result = _deadCodeElimination();
printf("%ld\n", (long)EV_result);
EV_result = _interProceduralOptimization();
printf("%ld\n", (long)EV_result);
EV_result = _commonSubexpressionElimination();
printf("%ld\n", (long)EV_result);
EV_result = _hoisting();
printf("%ld\n", (long)EV_result);
EV_result = _doubleIf();
printf("%ld\n", (long)EV_result);
EV_result = _integerDivide();
printf("%ld\n", (long)EV_result);
EV_result = _association();
printf("%ld\n", (long)EV_result);
EV_result = _tailRecursion((EV_input/1000));
printf("%ld\n", (long)EV_result);
EV_result = _unswitching();
printf("%ld\n", (long)EV_result);
EV_result = _randomCalculation(EV_input);
printf("%ld\n", (long)EV_result);
EV_result = _iterativeFibonacci((EV_input/5));
printf("%ld\n", (long)EV_result);
EV_result = _recursiveFibonacci((EV_input/1000));
printf("%ld\n", (long)EV_result);
EV_i = (EV_i+1);
}
printf("%ld\n", (long)9999);
return 0;
}
int main(void)
{
   return _main();
}

