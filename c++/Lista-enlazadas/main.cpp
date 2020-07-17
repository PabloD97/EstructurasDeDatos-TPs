#include <iostream>
#include "Lista.h"

using namespace std;

int main(){

    Lista l = vaciaL() ;
    agregarAlPrincipio(l,20);
    agregarAlPrincipio(l,10);
    agregarAlPrincipio(l,0);
    agregarAlPrincipio(l,20);
    agregarAlPrincipio(l,2);
    agregarAlPrincipio(l,20);

    cout<< "longitud de la lista: " << longitudL(l) << endl;

    agregarAlFinalL(l,50);



    Lista boleanos = vaciaL();
    agregarAlPrincipio(boleanos, true);
    agregarAlPrincipio(boleanos, true);
    agregarAlPrincipio(boleanos, true);
    agregarAlPrincipio(boleanos, false);
    cout<< "los distintos a 20 son: " <<endl;
    for(int i=0; i < 4; i++){
        cout<< iesimoL( (losDistintosA(20,l)), i) <<endl;
    }

    Lista* listas = new Lista[2];
    listas[0] = l;
    listas[1] = boleanos;
    Lista r = longitudes(listas, 2);

    cout<< iesimoL( (intercalar(7,l)), 0 ) <<endl;


    return 0;
}
