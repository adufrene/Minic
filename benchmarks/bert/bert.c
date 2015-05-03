#include<stdio.h>
#include<stdlib.h>
struct EV_node
{
long EV_data;
struct EV_node * EV_next;
};
struct EV_tnode
{
long EV_data;
struct EV_tnode * EV_left;
struct EV_tnode * EV_right;
};
struct EV_i
{
long EV_i;
};
struct EV_myCopy
{
long EV_b;
};
long EV_a;
long EV_b;
struct EV_i * EV_i;
struct EV_node * _concatLists(struct EV_node * EV_first,struct EV_node * EV_second)
{
struct EV_node * EV_temp;
EV_temp = EV_first;
if ((EV_first==NULL))
{
return EV_second;
}
while ((EV_temp->EV_next!=NULL))
{
EV_temp = EV_temp->EV_next;
}
EV_temp->EV_next = EV_second;
return EV_first;
}
struct EV_node * _add(struct EV_node * EV_list,long EV_toAdd)
{
struct EV_node * EV_newNode;
EV_newNode = (struct EV_node*)malloc(sizeof(struct EV_node));
EV_newNode->EV_data = EV_toAdd;
EV_newNode->EV_next = EV_list;
return EV_newNode;
}
long _size(struct EV_node * EV_list)
{
if ((EV_list==NULL))
{
return 0;
}
return (1+_size(EV_list->EV_next));
}
long _get(struct EV_node * EV_list,long EV_index)
{
if ((EV_index==0))
{
return EV_list->EV_data;
}
return _get(EV_list->EV_next, (EV_index-1));
}
struct EV_node * _pop(struct EV_node * EV_list)
{
EV_list = EV_list->EV_next;
return EV_list;
}
void _printList(struct EV_node * EV_list)
{
if ((EV_list!=NULL))
{
printf("%ld\n", (long)EV_list->EV_data);
_printList(EV_list->EV_next);
}
}
void _treeprint(struct EV_tnode * EV_root)
{
if ((EV_root!=NULL))
{
_treeprint(EV_root->EV_left);
printf("%ld\n", (long)EV_root->EV_data);
_treeprint(EV_root->EV_right);
}
}
void _freeList(struct EV_node * EV_list)
{
if ((EV_list!=NULL))
{
_freeList(EV_list->EV_next);
free(EV_list);
}
}
void _freeTree(struct EV_tnode * EV_root)
{
if ((!(EV_root==NULL)))
{
_freeTree(EV_root->EV_left);
_freeTree(EV_root->EV_right);
free(EV_root);
}
}
struct EV_node * _postOrder(struct EV_tnode * EV_root)
{
struct EV_node * EV_temp;
if ((EV_root!=NULL))
{
EV_temp = (struct EV_node*)malloc(sizeof(struct EV_node));
EV_temp->EV_data = EV_root->EV_data;
EV_temp->EV_next = NULL;
return _concatLists(_concatLists(_postOrder(EV_root->EV_left), _postOrder(EV_root->EV_right)), EV_temp);
}
return NULL;
}
struct EV_tnode * _treeadd(struct EV_tnode * EV_root,long EV_toAdd)
{
struct EV_tnode * EV_temp;
if ((EV_root==NULL))
{
EV_temp = (struct EV_tnode*)malloc(sizeof(struct EV_tnode));
EV_temp->EV_data = EV_toAdd;
EV_temp->EV_left = NULL;
EV_temp->EV_right = NULL;
return EV_temp;
}
if ((EV_toAdd<EV_root->EV_data))
{
EV_root->EV_left = _treeadd(EV_root->EV_left, EV_toAdd);
}
else
{
EV_root->EV_right = _treeadd(EV_root->EV_right, EV_toAdd);
}
return EV_root;
}
struct EV_node * _quickSort(struct EV_node * EV_list)
{
long EV_pivot;
long EV_i;
struct EV_node * EV_less;
struct EV_node * EV_greater;
struct EV_node * EV_temp;
EV_less = NULL;
EV_greater = NULL;
if ((_size(EV_list)<=1))
{
return EV_list;
}
EV_pivot = ((_get(EV_list, 0)+_get(EV_list, (_size(EV_list)-1)))/2);
EV_temp = EV_list;
EV_i = 0;
while ((EV_temp!=NULL))
{
if ((_get(EV_list, EV_i)>EV_pivot))
{
EV_greater = _add(EV_greater, _get(EV_list, EV_i));
}
else
{
EV_less = _add(EV_less, _get(EV_list, EV_i));
}
EV_temp = EV_temp->EV_next;
EV_i = (EV_i+1);
}
_freeList(EV_list);
return _concatLists(_quickSort(EV_less), _quickSort(EV_greater));
}
struct EV_node * _quickSortMain(struct EV_node * EV_list)
{
_printList(EV_list);
printf("%ld\n", (long)(-999));
_printList(EV_list);
printf("%ld\n", (long)(-999));
_printList(EV_list);
printf("%ld\n", (long)(-999));
return NULL;
}
long _treesearch(struct EV_tnode * EV_root,long EV_target)
{
printf("%ld\n", (long)(-1));
if ((EV_root!=NULL))
{
if ((EV_root->EV_data==EV_target))
{
return 1;
}
if ((_treesearch(EV_root->EV_left, EV_target)==1))
{
return 1;
}
if ((_treesearch(EV_root->EV_right, EV_target)==1))
{
return 1;
}
else
{
return 0;
}
}
return 0;
}
struct EV_node * _inOrder(struct EV_tnode * EV_root)
{
struct EV_node * EV_temp;
if ((EV_root!=NULL))
{
EV_temp = (struct EV_node*)malloc(sizeof(struct EV_node));
EV_temp->EV_data = EV_root->EV_data;
EV_temp->EV_next = NULL;
return _concatLists(_inOrder(EV_root->EV_left), _concatLists(EV_temp, _inOrder(EV_root->EV_right)));
}
else
{
return NULL;
}
}
long _bintreesearch(struct EV_tnode * EV_root,long EV_target)
{
printf("%ld\n", (long)(-1));
if ((EV_root!=NULL))
{
if ((EV_root->EV_data==EV_target))
{
return 1;
}
if ((EV_target<EV_root->EV_data))
{
return _bintreesearch(EV_root->EV_left, EV_target);
}
else
{
return _bintreesearch(EV_root->EV_right, EV_target);
}
}
return 0;
}
struct EV_tnode * _buildTree(struct EV_node * EV_list)
{
long EV_i;
struct EV_tnode * EV_root;
EV_root = NULL;
EV_i = 0;
while ((EV_i<_size(EV_list)))
{
EV_root = _treeadd(EV_root, _get(EV_list, EV_i));
EV_i = (EV_i+1);
}
return EV_root;
}
void _treeMain(struct EV_node * EV_list)
{
struct EV_tnode * EV_root;
struct EV_node * EV_inList;
struct EV_node * EV_postList;
EV_root = _buildTree(EV_list);
_treeprint(EV_root);
printf("%ld\n", (long)(-999));
EV_inList = _inOrder(EV_root);
_printList(EV_inList);
printf("%ld\n", (long)(-999));
_freeList(EV_inList);
EV_postList = _postOrder(EV_root);
_printList(EV_postList);
printf("%ld\n", (long)(-999));
_freeList(EV_postList);
printf("%ld\n", (long)_treesearch(EV_root, 0));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_treesearch(EV_root, 10));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_treesearch(EV_root, (-2)));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_treesearch(EV_root, 2));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_treesearch(EV_root, 3));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_treesearch(EV_root, 9));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_treesearch(EV_root, 1));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_bintreesearch(EV_root, 0));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_bintreesearch(EV_root, 10));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_bintreesearch(EV_root, (-2)));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_bintreesearch(EV_root, 2));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_bintreesearch(EV_root, 3));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_bintreesearch(EV_root, 9));
printf("%ld\n", (long)(-999));
printf("%ld\n", (long)_bintreesearch(EV_root, 1));
printf("%ld\n", (long)(-999));
_freeTree(EV_root);
}
struct EV_node * _myCopy(struct EV_node * EV_src)
{
if ((EV_src==NULL))
{
return NULL;
}
return _concatLists(_add(NULL, EV_src->EV_data), _myCopy(EV_src->EV_next));
}
long _main()
{
long EV_i;
long EV_element;
struct EV_node * EV_myList;
struct EV_node * EV_copyList1;
struct EV_node * EV_copyList2;
struct EV_node * EV_sortedList;
EV_myList = NULL;
EV_i = 0;
while ((EV_i<10))
{
scanf("%ld", &EV_element);
EV_myList = _add(EV_myList, EV_element);
EV_copyList1 = _myCopy(EV_myList);
EV_copyList2 = _myCopy(EV_myList);
EV_sortedList = _quickSortMain(EV_copyList1);
_freeList(EV_sortedList);
_treeMain(EV_copyList2);
EV_i = (EV_i+1);
}
_freeList(EV_myList);
_freeList(EV_copyList1);
_freeList(EV_copyList2);
return 0;
}
int main(void)
{
   return _main();
}

