#include <iostream>
using namespace std;

int sucesor(int i){
    return i+1;
}

int sumar(int x, int y){
    return x+y;
}
int maximo(int x,int y){
    if(x > y){
        return x;
    }
    else{
        return y;
    }
}



////////////////// Tuplas /////////////////
typedef int Elem;

struct TuplaSt{
    Elem fst;
    Elem snd;
};


TuplaSt* nuevaT(Elem fst, Elem snd){
    TuplaSt* t = new TuplaSt;
    t->fst = fst;
    t->snd = snd;
    return t;
}


int primera(TuplaSt* t ){
    return t->fst;
}

int segunda(TuplaSt* t ){
    return t->snd;
}

int sumarPar(TuplaSt* t){
    return t->fst + t->snd;
}

int maxDelPar(TuplaSt* t){
    return maximo(t->fst,t->snd);
}

/**Tipos enumerativos y pattern matching**/

typedef string Dir;

#define Norte "Norte"
#define Este "Este"
#define Sur "Sur"
#define Oeste "Oeste"


Dir opuesto(Dir d){
    if(d == Norte){
        return Sur;
    }
    if(d == Sur){
        return Norte;
    }
    if(d == Este){
        return Oeste;
    }
    if(d == Oeste){
        return Este;
    }
}

Dir siguiente(Dir d){
    if(d == Norte){
        return Este;
    }
    if(d == Sur){
        return Oeste;
    }
    if(d == Este){
        return Sur;
    }
    if(d == Oeste){
        return Norte;
    }
}
/** Dias de la semana **/
typedef string DiaDeSemana;

#define Lunes "lunes"
#define Martes "martes"
#define Miercoles "miercoles"
#define Jueves "jueves"
#define Viernes "viernes"
#define Sabado "sabado"
#define Domingo "domingo"

DiaDeSemana primerDia(){
    return Lunes;
}

DiaDeSemana ultimoDia(){
    return Domingo;
}

int nroDeDia(DiaDeSemana d){
    if(d == Lunes){
        return 1;
    }if(d == Martes){
        return 2;
    }if(d == Miercoles){
        return 3;
    }if(d == Jueves){
        return 4;
    }if(d ==Viernes){
        return 5;
    }if(d == Sabado){
        return 6;
    }if(d == Domingo){
        return 7;
    }
}

bool empiezaConM(DiaDeSemana d){
    return (d == Martes || d == Miercoles);
}

bool estaEnElMedio(DiaDeSemana d){
    return (d != Lunes || d != Domingo);
}

bool vieneDespues(DiaDeSemana d1, DiaDeSemana d2){
    if( d2 == Domingo && d1 == Lunes ){
        return true;
    }
    else{
        return nroDeDia(d1) > nroDeDia(d2);
    }
}

bool negar(bool b){
    return !b;
}

/** Funciones polimorficas **/

Elem loMismo(Elem e){
    return e;
}

TuplaSt* duplicar(Elem e){
    return nuevaT(e,e);
}

int main(){
/** Tuplas
    TuplaSt* t = nuevaT(4,8);
    cout<< primera(t)  << endl;
    cout<< segunda(t)  << endl;
    cout<< sumarPar(t)  << endl;
    cout<< maxDelPar(t)  << endl;
**/
/** Direcciones
    cout<< opuesto(Este) << endl;
    cout<< siguiente(Este) << endl;
**/

/** Dia de la semana
    cout << "el dia1 viene despues del dia 2 :"<< vieneDespues(Sabado, Domingo) << endl;
**/

   // cout<< negar(true) <<endl;

   cout << "(" << primera(duplicar(5))  << "," << segunda(duplicar(5)) << ")" <<endl;

    return 0;
}
