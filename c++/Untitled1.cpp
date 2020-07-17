#include <iostream>

using namespace std;

int cuatro(){
    return 4;
}

int f(int x, int y){
    return 2 * x + 3 * y;
}

int g(int x){
    int y = 2 * x; // declaracion de la variable "y"
    y = y + 2;     // asignacion
    return y + x;
}

int maximo(int x, int y){
    int resultado;
    if (x<y) {
        resultado = y;
    } else {
        resultado = x;
    }

    return resultado;
}

int sumarTodos(int desde, int hasta){
    int resultado = 0;
    int i = desde;
    while (i <= hasta) {
        resultado = resultado + i;
        i = i + 1;
    }
    return resultado;
}

bool esLindo(int x){
    return x * x - 3 *x + 18 < 104;
}

bool todosLindos(int desde, int hasta){
    bool res = true;
    for(int i = desde; i <= hasta; i++){
        res = res && esLindo(i);
    }
    return res;
}
/******* Dir *******/
typedef int Dir; // int = tipo y Dir = nuevo tipo

Dir norte(){ return 0; }
Dir este() { return 1; }
Dir sur()  { return 2; }
Dir oeste(){ return 3; }

Dir siguiente(Dir d){
    return (d + 1) % 4;
}

/************** Coordenada ******************/

struct Coordenada{
    int x;
    int y;
};

Coordenada nuevaCoordenada(int vx, int vy){
    Coordenada c;
    c.x = vx;
    c.y = vy;
    return c;
}

Coordenada desplazar(Coordenada c, Dir d){
    Coordenada res = c;
    if( d == norte()){
        res.y++;
    }
    if(d == este()){
        res.x++;
    }
    if(d == sur()){
        res.y--;
    }
    if(d == oeste()){
        res.x--;
    }
    return res;
}

int main(){

    int x;
    cout << "el valor de x es : "<< x << endl;

    return 0;
}
