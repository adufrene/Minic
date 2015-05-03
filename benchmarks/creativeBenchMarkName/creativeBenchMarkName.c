#include<stdio.h>
#include<stdlib.h>
struct EV_node
{
long EV_value;
struct EV_node * EV_next;
};
struct EV_node * _buildList()
{
long EV_input;
long EV_i;
struct EV_node * EV_n0;
struct EV_node * EV_n1;
struct EV_node * EV_n2;
struct EV_node * EV_n3;
struct EV_node * EV_n4;
struct EV_node * EV_n5;
EV_n0 = (struct EV_node*)malloc(sizeof(struct EV_node));
EV_n1 = (struct EV_node*)malloc(sizeof(struct EV_node));
EV_n2 = (struct EV_node*)malloc(sizeof(struct EV_node));
EV_n3 = (struct EV_node*)malloc(sizeof(struct EV_node));
EV_n4 = (struct EV_node*)malloc(sizeof(struct EV_node));
EV_n5 = (struct EV_node*)malloc(sizeof(struct EV_node));
scanf("%ld", &EV_n0->EV_value);
scanf("%ld", &EV_n1->EV_value);
scanf("%ld", &EV_n2->EV_value);
scanf("%ld", &EV_n3->EV_value);
scanf("%ld", &EV_n4->EV_value);
scanf("%ld", &EV_n5->EV_value);
EV_n0->EV_next = EV_n1;
EV_n1->EV_next = EV_n2;
EV_n2->EV_next = EV_n3;
EV_n3->EV_next = EV_n4;
EV_n4->EV_next = EV_n5;
EV_n5->EV_next = NULL;
return EV_n0;
}
long _multiple(struct EV_node * EV_list)
{
long EV_i;
long EV_product;
struct EV_node * EV_cur;
EV_i = 0;
EV_cur = EV_list;
EV_product = EV_cur->EV_value;
EV_cur = EV_cur->EV_next;
while ((EV_i<5))
{
EV_product = (EV_product*EV_cur->EV_value);
EV_cur = EV_cur->EV_next;
printf("%ld\n", (long)EV_product);
EV_i = (EV_i+1);
}
return EV_product;
}
long _add(struct EV_node * EV_list)
{
long EV_i;
long EV_sum;
struct EV_node * EV_cur;
EV_i = 0;
EV_cur = EV_list;
EV_sum = EV_cur->EV_value;
EV_cur = EV_cur->EV_next;
while ((EV_i<5))
{
EV_sum = (EV_sum+EV_cur->EV_value);
EV_cur = EV_cur->EV_next;
printf("%ld\n", (long)EV_sum);
EV_i = (EV_i+1);
}
return EV_sum;
}
long _recurseList(struct EV_node * EV_list)
{
if ((EV_list->EV_next==NULL))
{
return EV_list->EV_value;
}
else
{
return (EV_list->EV_value*_recurseList(EV_list->EV_next));
}
}
long _main()
{
struct EV_node * EV_list;
long EV_product;
long EV_sum;
long EV_result;
long EV_bigProduct;
long EV_i;
EV_i = 0;
EV_bigProduct = 0;
EV_list = _buildList();
EV_product = _multiple(EV_list);
EV_sum = _add(EV_list);
EV_result = (EV_product-(EV_sum/2));
while ((EV_i<10))
{
EV_bigProduct = (EV_bigProduct+_recurseList(EV_list));
EV_i = (EV_i+1);
}
printf("%ld\n", (long)EV_bigProduct);
while ((EV_bigProduct!=0))
{
EV_bigProduct = (EV_bigProduct-1);
}
printf("%ld\n", (long)EV_result);
printf("%ld\n", (long)EV_bigProduct);
return 0;
}
int main(void)
{
   return _main();
}

