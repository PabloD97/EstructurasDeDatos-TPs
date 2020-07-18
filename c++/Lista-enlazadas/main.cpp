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
    agregarAlPrincipio(l3,9);
    agregarAlPrincipio(l3,1);
    agregarAlPrincipio(l3,10);
    agregarAlPrincipio(l3,1);

    Lista test = takeN(2,l2);

    for(int i = 0; i < longitudL(test); i++){
        cout<< iesimoL(test,i) <<endl;
    }

    cout<< "El menor de la lista es: " << minimun(test) << endl;

    //destruirL(reversita);
    destruirL(l);
    destruirL(l2);
    destruirL(l3);


    return 0;
}
