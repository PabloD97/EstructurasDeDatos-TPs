#include <iostream>
#include "Lista.h"

using namespace std;

int main(){

    Lista l = vaciaL() ;
    agregarAlPrincipio(l,20);
    agregarAlPrincipio(l,20);
    agregarAlPrincipio(l,20);

    agregarAlPrincipio(l,20);
    agregarAlPrincipio(l,20);
    agregarAlPrincipio(l,20);

   // cout<< "longitud de la lista: " << longitudL(l) << endl;

    //agregarAlFinalL(l,50);

    Lista l2 = vaciaL();
    agregarAlPrincipio(l2,5);
    agregarAlPrincipio(l2,6);
    agregarAlPrincipio(l2,7);
    agregarAlPrincipio(l2,8);

    Lista l3 = vaciaL();
    agregarAlPrincipio(l3,1);
    agregarAlPrincipio(l3,1);
    agregarAlPrincipio(l3,1);
    agregarAlPrincipio(l3,1);

    //Lista test = append(l,l2);

    Lista* listas = new Lista;
    listas[0] = l;
    listas[1] = l2;
    listas[2] = l3;

    Lista reversita = reversa(l2);

    for(int i = 0; i < longitudL(l2) ; i++){
        cout<< iesimoL(l2, i ) <<endl;
    }


    return 0;
}
