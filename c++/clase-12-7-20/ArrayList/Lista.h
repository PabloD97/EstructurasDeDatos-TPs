#ifndef _LISTA_H_
#define _LISTA_H_

typedef struct ListaSt* Lista;
typedef int Elem;

Lista nuevaL();

void agregarL(Lista l, Elem x);
int longitudL(Lista l);
Elem iesimoL(Lista l, int i);
void destruirL(Lista l);

#endif // _LISTA_H_
