#include <iostream>
using namespace std;

#include "Lista.h"

int main(){

    Lista ls = nuevaL();
    agregarL(ls,4);
    agregarL(ls,3);
    agregarL(ls,7);
    agregarL(ls,9);
    for(int i=0;i<longitudL(ls);i++){
        cout << iesimoL(ls,i) << endl;
    }

    return 0;
}
