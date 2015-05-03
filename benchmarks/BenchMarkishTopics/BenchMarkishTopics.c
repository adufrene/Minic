#include<stdio.h>
#include<stdlib.h>
struct EV_intList
{
long EV_data;
struct EV_intList * EV_rest;
};
long EV_intList;
long _length(struct EV_intList * EV_list)
{
if ((EV_list==NULL))
{
return 0;
}
return (1+_length(EV_list->EV_rest));
}
struct EV_intList * _addToFront(struct EV_intList * EV_list,long EV_element)
{
struct EV_intList * EV_front;
if ((EV_list==NULL))
{
EV_list = (struct EV_intList*)malloc(sizeof(struct EV_intList));
EV_list->EV_data = EV_element;
EV_list->EV_rest = NULL;
return EV_list;
}
EV_front = (struct EV_intList*)malloc(sizeof(struct EV_intList));
EV_front->EV_data = EV_element;
EV_front->EV_rest = EV_list;
return EV_front;
}
struct EV_intList * _deleteFirst(struct EV_intList * EV_list)
{
struct EV_intList * EV_first;
if ((EV_list==NULL))
{
return NULL;
}
EV_first = EV_list;
EV_list = EV_list->EV_rest;
free(EV_first);
return EV_list;
}
long _main()
{
struct EV_intList * EV_list;
long EV_sum;
scanf("%ld", &EV_intList);
EV_sum = 0;
EV_list = NULL;
while ((EV_intList>0))
{
EV_list = _addToFront(EV_list, EV_intList);
printf("%ld ", (long)EV_list->EV_data);
EV_intList = (EV_intList-1);
}
printf("%ld ", (long)_length(EV_list));
while ((_length(EV_list)>0))
{
EV_sum = (EV_sum+EV_list->EV_data);
printf("%ld ", (long)_length(EV_list));
EV_list = _deleteFirst(EV_list);
}
printf("%ld\n", (long)EV_sum);
return 0;
}
int main(void)
{
   return _main();
}

