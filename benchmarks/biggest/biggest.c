#include<stdio.h>
#include<stdlib.h>
struct EV_IntList
{
long EV_head;
struct EV_IntList * EV_tail;
};
struct EV_IntList * _getIntList()
{
struct EV_IntList * EV_list;
long EV_next;
EV_list = (struct EV_IntList*)malloc(sizeof(struct EV_IntList));
scanf("%ld", &EV_next);
if ((EV_next==(-1)))
{
EV_list->EV_head = EV_next;
EV_list->EV_tail = NULL;
return EV_list;
}
else
{
EV_list->EV_head = EV_next;
EV_list->EV_tail = _getIntList();
return EV_list;
}
}
long _biggest(long EV_num1,long EV_num2)
{
if ((EV_num1>EV_num2))
{
return EV_num1;
}
else
{
return EV_num2;
}
}
long _biggestInList(struct EV_IntList * EV_list)
{
long EV_big;
EV_big = EV_list->EV_head;
while ((EV_list->EV_tail!=NULL))
{
EV_big = _biggest(EV_big, EV_list->EV_head);
EV_list = EV_list->EV_tail;
}
return EV_big;
}
long _main()
{
struct EV_IntList * EV_list;
EV_list = _getIntList();
printf("%ld\n", (long)_biggestInList(EV_list));
return 0;
}
int main(void)
{
   return _main();
}

