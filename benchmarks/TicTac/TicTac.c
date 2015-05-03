#include<stdio.h>
#include<stdlib.h>
struct EV_gameBoard
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
};
void _cleanBoard(struct EV_gameBoard * EV_board)
{
EV_board->EV_a = 0;
EV_board->EV_b = 0;
EV_board->EV_c = 0;
EV_board->EV_d = 0;
EV_board->EV_e = 0;
EV_board->EV_f = 0;
EV_board->EV_g = 0;
EV_board->EV_h = 0;
EV_board->EV_i = 0;
}
void _printBoard(struct EV_gameBoard * EV_board)
{
printf("%ld ", (long)EV_board->EV_a);
printf("%ld ", (long)EV_board->EV_b);
printf("%ld\n", (long)EV_board->EV_c);
printf("%ld ", (long)EV_board->EV_d);
printf("%ld ", (long)EV_board->EV_e);
printf("%ld\n", (long)EV_board->EV_f);
printf("%ld ", (long)EV_board->EV_g);
printf("%ld ", (long)EV_board->EV_h);
printf("%ld\n", (long)EV_board->EV_i);
}
void _printMoveBoard()
{
printf("%ld\n", (long)123);
printf("%ld\n", (long)456);
printf("%ld\n", (long)789);
}
void _placePiece(struct EV_gameBoard * EV_board,long EV_turn,long EV_placement)
{
if ((EV_placement==1))
{
EV_board->EV_a = EV_turn;
}
else
{
if ((EV_placement==2))
{
EV_board->EV_b = EV_turn;
}
else
{
if ((EV_placement==3))
{
EV_board->EV_c = EV_turn;
}
else
{
if ((EV_placement==4))
{
EV_board->EV_d = EV_turn;
}
else
{
if ((EV_placement==5))
{
EV_board->EV_e = EV_turn;
}
else
{
if ((EV_placement==6))
{
EV_board->EV_f = EV_turn;
}
else
{
if ((EV_placement==7))
{
EV_board->EV_g = EV_turn;
}
else
{
if ((EV_placement==8))
{
EV_board->EV_h = EV_turn;
}
else
{
if ((EV_placement==9))
{
EV_board->EV_i = EV_turn;
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
long _checkWinner(struct EV_gameBoard * EV_board)
{
if ((EV_board->EV_a==1))
{
if ((EV_board->EV_b==1))
{
if ((EV_board->EV_c==1))
{
return 0;
}
}
}
if ((EV_board->EV_a==2))
{
if ((EV_board->EV_b==2))
{
if ((EV_board->EV_c==2))
{
return 1;
}
}
}
if ((EV_board->EV_d==1))
{
if ((EV_board->EV_e==1))
{
if ((EV_board->EV_f==1))
{
return 0;
}
}
}
if ((EV_board->EV_d==2))
{
if ((EV_board->EV_e==2))
{
if ((EV_board->EV_f==2))
{
return 1;
}
}
}
if ((EV_board->EV_g==1))
{
if ((EV_board->EV_h==1))
{
if ((EV_board->EV_i==1))
{
return 0;
}
}
}
if ((EV_board->EV_g==2))
{
if ((EV_board->EV_h==2))
{
if ((EV_board->EV_i==2))
{
return 1;
}
}
}
if ((EV_board->EV_a==1))
{
if ((EV_board->EV_d==1))
{
if ((EV_board->EV_g==1))
{
return 0;
}
}
}
if ((EV_board->EV_a==2))
{
if ((EV_board->EV_d==2))
{
if ((EV_board->EV_g==2))
{
return 1;
}
}
}
if ((EV_board->EV_b==1))
{
if ((EV_board->EV_e==1))
{
if ((EV_board->EV_h==1))
{
return 0;
}
}
}
if ((EV_board->EV_b==2))
{
if ((EV_board->EV_e==2))
{
if ((EV_board->EV_h==2))
{
return 1;
}
}
}
if ((EV_board->EV_c==1))
{
if ((EV_board->EV_f==1))
{
if ((EV_board->EV_i==1))
{
return 0;
}
}
}
if ((EV_board->EV_c==2))
{
if ((EV_board->EV_f==2))
{
if ((EV_board->EV_i==2))
{
return 1;
}
}
}
return (-1);
}
long _main()
{
long EV_turn;
long EV_space1;
long EV_space2;
long EV_winner;
long EV_i;
struct EV_gameBoard * EV_board;
EV_i = 0;
EV_turn = 0;
EV_space1 = 0;
EV_space2 = 0;
EV_winner = (-1);
EV_board = (struct EV_gameBoard*)malloc(sizeof(struct EV_gameBoard));
_cleanBoard(EV_board);
while (((EV_winner<0)&&(EV_i!=8)))
{
_printBoard(EV_board);
if ((EV_turn==0))
{
EV_turn = (EV_turn+1);
scanf("%ld", &EV_space1);
_placePiece(EV_board, 1, EV_space1);
}
else
{
EV_turn = (EV_turn-1);
scanf("%ld", &EV_space2);
_placePiece(EV_board, 2, EV_space2);
}
EV_winner = _checkWinner(EV_board);
EV_i = (EV_i+1);
}
printf("%ld\n", (long)(EV_winner+1));
return 0;
}
int main(void)
{
   return _main();
}

