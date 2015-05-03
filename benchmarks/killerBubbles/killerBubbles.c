#include<stdio.h>
#include<stdlib.h>
struct EV_Node
{
long EV_val;
struct EV_Node * EV_prev;
struct EV_Node * EV_next;
};
long EV_swapped;
long _compare(struct EV_Node * EV_a,struct EV_Node * EV_b)
{
return (EV_a->EV_val-EV_b->EV_val);
}
void _deathSort(struct EV_Node * EV_head)
{
long EV_swapped;
long EV_swap;
struct EV_Node * EV_currNode;
EV_swapped = 1;
while ((EV_swapped==1))
{
EV_swapped = 0;
EV_currNode = EV_head;
while ((EV_currNode->EV_next!=EV_head))
{
if ((_compare(EV_currNode, EV_currNode->EV_next)>0))
{
EV_swap = EV_currNode->EV_val;
EV_currNode->EV_val = EV_currNode->EV_next->EV_val;
EV_currNode->EV_next->EV_val = EV_swap;
EV_swapped = 1;
}
EV_currNode = EV_currNode->EV_next;
}
}
}
void _printEVILList(struct EV_Node * EV_head)
{
struct EV_Node * EV_currNode;
struct EV_Node * EV_toFree;
EV_currNode = EV_head->EV_next;
printf("%ld\n", (long)EV_head->EV_val);
free(EV_head);
while ((EV_currNode!=EV_head))
{
EV_toFree = EV_currNode;
printf("%ld\n", (long)EV_currNode->EV_val);
EV_currNode = EV_currNode->EV_next;
free(EV_toFree);
}
}
long _main()
{
long EV_numNodes;
long EV_counter;
struct EV_Node * EV_currNode;
struct EV_Node * EV_head;
struct EV_Node * EV_previous;
EV_swapped = 666;
scanf("%ld", &EV_numNodes);
if ((EV_numNodes<=0))
{
printf("%ld\n", (long)(-1));
return (-1);
}
EV_numNodes = (EV_numNodes*1000);
EV_counter = EV_numNodes;
EV_head = (struct EV_Node*)malloc(sizeof(struct EV_Node));
EV_head->EV_val = EV_counter;
EV_head->EV_prev = EV_head;
EV_head->EV_next = EV_head;
EV_counter = (EV_counter-1);
EV_previous = EV_head;
while ((EV_counter>0))
{
EV_currNode = (struct EV_Node*)malloc(sizeof(struct EV_Node));
EV_currNode->EV_val = EV_counter;
EV_currNode->EV_prev = EV_previous;
EV_currNode->EV_next = EV_head;
EV_previous->EV_next = EV_currNode;
EV_previous = EV_currNode;
EV_counter = (EV_counter-1);
}
_deathSort(EV_head);
_printEVILList(EV_head);
return 0;
}
int main(void)
{
   return _main();
}

