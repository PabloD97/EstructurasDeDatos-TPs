#include "Lista.h"

struct ListaSt{
    int capacidad;
    int length;
    Elem* elementos;
};

Lista nuevaL(){
    Lista ls = new ListaSt;
    ls->capacidad = 1;
    ls->length = 0;
    ls->elementos = new Elem[1];


    return ls;
}

void agregarL(Lista ls, Elem x){
    if(ls->length == ls->capacidad){

        Elem* nuevo = new Elem[ls->capacidad * 2];

        for(int i = 0; i < ls->capacidad; i++){
            nuevo[i] = ls->elementos[i];
        }

        ls->capacidad = 2 * ls->capacidad;
        delete [] ls->elementos;
        ls->elementos = nuevo;

    }

    ls->elementos[ls->length] = x;
    ls->length++;
}

int longitudL(Lista l){
    return l->length;
}

Elem iesimoL(Lista l, int i){
    return l->elementos[i];
}

void destruirL(Lista l){

    delete [] l->elementos;

}


