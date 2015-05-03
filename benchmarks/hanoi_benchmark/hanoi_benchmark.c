#include<stdio.h>
#include<stdlib.h>
struct EV_plate
{
long EV_size;
struct EV_plate * EV_plateUnder;
};
struct EV_plate * EV_peg1;
struct EV_plate * EV_peg2;
struct EV_plate * EV_peg3;
long EV_numMoves;
void _move(long EV_from,long EV_to)
{
struct EV_plate * EV_plateToMove;
if ((EV_from==1))
{
EV_plateToMove = EV_peg1;
EV_peg1 = EV_peg1->EV_plateUnder;
}
else
{
if ((EV_from==2))
{
EV_plateToMove = EV_peg2;
EV_peg2 = EV_peg2->EV_plateUnder;
}
else
{
EV_plateToMove = EV_peg3;
EV_peg3 = EV_peg3->EV_plateUnder;
}
}
if ((EV_to==1))
{
EV_plateToMove->EV_plateUnder = EV_peg1;
EV_peg1 = EV_plateToMove;
}
else
{
if ((EV_to==2))
{
EV_plateToMove->EV_plateUnder = EV_peg2;
EV_peg2 = EV_plateToMove;
}
else
{
EV_plateToMove->EV_plateUnder = EV_peg3;
EV_peg3 = EV_plateToMove;
}
}
EV_numMoves = (EV_numMoves+1);
}
void _hanoi(long EV_n,long EV_from,long EV_to,long EV_other)
{
if ((EV_n==1))
{
_move(EV_from, EV_to);
}
else
{
_hanoi((EV_n-1), EV_from, EV_other, EV_to);
_move(EV_from, EV_to);
_hanoi((EV_n-1), EV_other, EV_to, EV_from);
}
}
void _printPeg(struct EV_plate * EV_peg)
{
struct EV_plate * EV_aPlate;
EV_aPlate = EV_peg;
while ((EV_aPlate!=NULL))
{
printf("%ld\n", (long)EV_aPlate->EV_size);
EV_aPlate = EV_aPlate->EV_plateUnder;
}
}
long _main()
{
long EV_count;
long EV_numPlates;
struct EV_plate * EV_aPlate;
EV_peg1 = NULL;
EV_peg2 = NULL;
EV_peg3 = NULL;
EV_numMoves = 0;
scanf("%ld", &EV_numPlates);
if ((EV_numPlates>=1))
{
EV_count = EV_numPlates;
while ((EV_count!=0))
{
EV_aPlate = (struct EV_plate*)malloc(sizeof(struct EV_plate));
EV_aPlate->EV_size = EV_count;
EV_aPlate->EV_plateUnder = EV_peg1;
EV_peg1 = EV_aPlate;
EV_count = (EV_count-1);
}
printf("%ld\n", (long)1);
_printPeg(EV_peg1);
printf("%ld\n", (long)2);
_printPeg(EV_peg2);
printf("%ld\n", (long)3);
_printPeg(EV_peg3);
_hanoi(EV_numPlates, 1, 3, 2);
printf("%ld\n", (long)1);
_printPeg(EV_peg1);
printf("%ld\n", (long)2);
_printPeg(EV_peg2);
printf("%ld\n", (long)3);
_printPeg(EV_peg3);
printf("%ld\n", (long)EV_numMoves);
while ((EV_peg3!=NULL))
{
EV_aPlate = EV_peg3;
EV_peg3 = EV_peg3->EV_plateUnder;
free(EV_aPlate);
}
}
return 0;
}
int main(void)
{
   return _main();
}

