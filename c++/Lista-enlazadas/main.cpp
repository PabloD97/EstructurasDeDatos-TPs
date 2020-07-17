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

    Lista test = append(l,l2);

    Lista* listas = new Lista;
    listas[0] = test;

//    Lista testAplanar = aplanar(listas, 2);

    Lista ltest = listas[0];
    for(int i = 0; i < longitudL( ltest ) ; i++){
            cout<< iesimoL(ltest,i) <<endl;
    }


    return 0;
}
