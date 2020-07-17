#include "Lista.h"
#include <cstdlib>

struct Nodo {
    Elem valor;
    Nodo* siguiente;
};

struct ListaRepr {
    Nodo* primero;
    int sise;
};

Lista vaciaL(){
    Lista l = new ListaRepr;
    l->primero = NULL;
    l->sise = 0;
    return l;
}

void agregarAlPrincipio(Lista l, Elem x){
    Nodo* punteroNodo = new Nodo;
    punteroNodo->valor = x;
    punteroNodo->siguiente = l->primero ;
    l->primero = punteroNodo;
    l->sise = l->sise+1;
}

int longitudL(Lista l){
    return l->sise;
}

Elem iesimoL(Lista l, int i){
    Nodo* punteroNodo = l->primero;
    int index = 0;
    while (index < i ){
        index++;
        punteroNodo = punteroNodo->siguiente;
    }

    return punteroNodo->valor;
}

Elem sacarPirmero( Lista l){
    Elem valor = l->primero->valor;
    Nodo* p = l->primero;
    l->primero = l->primero->siguiente;
    delete p;
    l->sise-- ;

    return valor;
}

void agregarAlFinalL(Lista l, Elem x){
    Nodo* punteroNodo = new Nodo;
    punteroNodo->valor = x;
    punteroNodo->siguiente = NULL;
    l->sise++;
    if( l->primero == NULL ){
        l->primero = punteroNodo;
    }
    else {
        Nodo* p = l->primero;
        while( p->siguiente != NULL ){
            p = p->siguiente;
        }
        p->siguiente = punteroNodo;
    }

}

Elem sacarElUltimo(Lista l){
    l->sise--;
    Elem valor;
    if (l->primero->siguiente == NULL ){
        valor = l->primero->valor;
        delete l->primero;
        l->primero = NULL;
    }
    else {
        Nodo* p = l->primero;
        while( p->siguiente->siguiente != NULL ){
            p = p->siguiente;
        }
        valor = p->siguiente->valor;
        delete p->siguiente;
        p->siguiente = NULL;
    }

    return valor;
}

void destruirL(Lista l){
    Nodo* punteroNodo = l->primero;
    while( punteroNodo != NULL){
        Nodo* q = punteroNodo->siguiente;
        delete punteroNodo;
        punteroNodo = q;
    }
    delete l;
}
/**
void agregarIesimoL(Lista l, Elem x, int i){

    if(i == 0 ){
        agregarAlPrincipio(l,x);
    }
    else{

        int contador = 0;


        while(contador != i){
            contador++;
        }

    }
}
**/
/// Funciones de la practica 2 de haskell
int sumatoria(Lista l){
    int res = 0;
    Nodo* punteroNodo = l->primero;

    for(int i= 0; i < l->sise ; i++ ){
        res = res + punteroNodo->valor;
        punteroNodo = punteroNodo->siguiente;
    }
    delete punteroNodo;
    return res;
}

Lista sucesores(Lista l){
    Lista res = vaciaL();
    Nodo* punteroNodo = l->primero;

    while(punteroNodo != NULL){
        agregarAlPrincipio(res,punteroNodo->valor + 1);
        punteroNodo = punteroNodo->siguiente;
    }
    delete punteroNodo;

    return res;
}

///Dada una lista de "bools" retorno true
/// si todos los valores son true
bool conjuncion(Lista l){
    bool res = true;
    Nodo* punteroNodo = l->primero;

    while( punteroNodo != NULL ){
        res = res && (punteroNodo->valor == true);
        punteroNodo = punteroNodo->siguiente;
    }


    return res;
}

bool disyuncion(Lista l){
    bool res = true;
    Nodo* punteroNodo = l->primero;

    while( punteroNodo != NULL ){
        res = res || (punteroNodo->valor == true);
        punteroNodo = punteroNodo->siguiente;
    }


    return res;
}

bool pertenece(Elem e, Lista l){

    Nodo* punteroNodo = l->primero;
    for(int i = 0; i < l->sise; i++){
        if(punteroNodo->valor == e){
            return true;
        }
        punteroNodo = punteroNodo->siguiente;
    }
    return false;
}

int apariciones(Elem e, Lista l){

    Nodo* punteroNodo = l->primero;
    int res = 0;

    while(punteroNodo != NULL){
        if(punteroNodo->valor == e){
            res++;
        }
        punteroNodo = punteroNodo->siguiente;
    }
    return res;
}

Lista losMenoresA(int filtro, Lista l){

    ListaRepr* res    = vaciaL();
    Nodo* punteroNodo = l->primero;

    for(int i = 0; i < l->sise; i++ ){
        if( punteroNodo->valor < filtro ){
            agregarAlPrincipio(res, punteroNodo->valor);
        }
        punteroNodo = punteroNodo->siguiente;
    }

    return res;
}

Lista losDistintosA(Elem e, Lista l){

    ListaRepr* res    = vaciaL();
    Nodo* punteroNodo = l->primero;

    for(int i = 0; i < l->sise; i++ ){
        if( punteroNodo->valor != e ){
            agregarAlPrincipio(res, punteroNodo->valor);
        }
        punteroNodo = punteroNodo->siguiente;
    }

    return res;
}



Lista longitudes(Lista* ls, int lengthLs){

    ListaRepr* res = vaciaL();

    for(int i=0; i < lengthLs; i++){

        ListaRepr* l = ls[i];
        agregarAlPrincipio( res, l->sise  );

    }

    return res;

}

Lista* lasDeLongitudMayorA(int filtro, Lista* ls, int lengthLs){

    Lista* res = new Lista;

    for(int i=0; i < lengthLs ; i++){
        ListaRepr* l = ls[i];
        if(l->sise > filtro){
            int lugar=0;
            res[lugar]=l;
            lugar++;
        }
    }

    return res;
}

Lista intercalar(Elem x, Lista l){/// Esta mal esto

    ListaRepr* res    = vaciaL();
    Nodo* punteroNodo = l->primero;

    for(int i = 0; i < l->sise  ; i++){

        agregarAlPrincipio(res,x);
        agregarAlPrincipio(res,punteroNodo->valor);
        punteroNodo = punteroNodo->siguiente;

    }
    delete punteroNodo;
    sacarElUltimo(res);
    return res;
}

Lista append(Lista l1, Lista l2){

    Lista copia = vaciaL();
    copia->primero = l1->primero;
/*
    Nodo* punteroNodo = l2->primero;

    while(punteroNodo != NULL){
        agregarAlFinalL(copia, punteroNodo->valor);
        punteroNodo = punteroNodo->siguiente;
    }
    delete punteroNodo;
 */ agregarAlPrincipio(copia, 1000);
    return copia;
}

void agregarTodoDeList(Lista l, Lista todo){

    Nodo* punteroNodo = todo->primero;

    while(punteroNodo != NULL){

        agregarAlFinalL(l, punteroNodo->valor);

        punteroNodo = punteroNodo->siguiente;

    }
    delete punteroNodo;
}

Lista aplanar(Lista* listas, int longitud){

    ListaRepr* res = vaciaL();

    for(int i = 0; i < longitud ; i++){

        ListaRepr* l = listas[i];
        agregarTodoDeList(res, l);
    }

    return res;

}

