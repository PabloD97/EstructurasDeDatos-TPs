#ifndef _LISTA_H
#define _LISTA_H

typedef struct ListaRepr* Lista;
typedef int Elem;

// Eficiencia: O(1)
Lista vaciaL();

// Eficiencia: O(1)
void agregarAlPrincipio(Lista l, Elem x);

// Eficiencia: O(1)
int longitudL(Lista l);

// Eficiencia: O(i)
Elem iesimoL(Lista l, int i); // Precondicion: 0 <= i < longitudL(l)

Elem sacarPirmero(Lista l); // Precondicion: la lista no debe ser vacia

void agregarAlFinalL(Lista l, Elem x);

Elem sacarElUltimo(Lista l);


// Hacer estos puntos
void agregarIesimoL(Lista l, Elem x, int i);
//Elem sacarIesimoL(Lista l, int i);


// Eficiencia: O(1)
void destruirL(Lista l);

/// Practica 2 de haskell

// Eficiencia: O(n)
int sumatoria(Lista l);

Lista sucesores(Lista l);

bool conjuncion(Lista l);

bool disyuncion(Lista l);

bool pertenece(Elem x, Lista l);

int apariciones(Elem e, Lista l);

Lista losMenoresA(int filtro, Lista l);

Lista losDistintosA(Elem e, Lista l);

Lista longitudes(Lista* listas, int lengthLs);

Lista* lasDeLongitudMayorA(int filtro, Lista* ls);

Lista intercalar(Elem e, Lista l);

Lista append(Lista l1, Lista l2);

Lista aplanar(Lista* listas, int longitud);


#endif
