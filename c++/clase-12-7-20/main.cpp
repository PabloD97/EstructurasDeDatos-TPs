#include <iostream>
using namespace std;
//Proposito:
// guarda en el arreglo c la suma componente a componente de los arreglos a y b
void sumar(int n,int* a, int* b, int* c ){
    for(int i = 0; i<n; i++){
        c[i] = a[i] + b[i];
    }
}

//Proposito:
// devuelve true si y solamante si
// los elementos del arreglo
// estan ordenado de menor a mayor.
bool estaOrdenado(int n, int* a){
    bool res=true;
    for(int i = 0; i < n - 1; i++){
        res = res && (a[i] < a[i + 1]);
    }
    return res;
}
// Proposito:
//  modifica el arreglo, dando vuelta
//  todos sus elementos

void intercambiar(int* a, int i, int j){
    int tmp = a[i];
    a[i] = a[j];
    a[j] = tmp;

}

void reverseA(int n, int* a){
    for(int i = 0; i < (n/2) ; i++){
            intercambiar(a,i,n - 1 - i);
    }
}

int promedio(int n, int* a){
    int res=0;
    for(int i = 0; i < n ; i++){
        res = res + a[i];
    }
    return (res/n);
}

int cuatosMayoresQue(int x, int n, int* a){
    int cant=0;
    for(int i = 0; i < n; i++){
        if(a[i] > x){
            cant++;
        }
    }
    return cant;
}

int main(){

    int a[4];
    a[0] = 10;
    a[1] = 20;
    a[2] = 30;
    a[3] = 40;

    int b[4];
    b[0] = 4;
    b[1] = 3;
    b[2] = 2;
    b[3] = 1;

    int c[4];

    sumar(4, a, b, c);


    cout << estaOrdenado(4,a) << endl;
    cout << estaOrdenado(4,b) << endl;

    reverseA(4,a);
    cout << promedio(4,b) << endl;
    cout << "mayores a 20: " << cuatosMayoresQue(20,4,a) << endl;

    return 0;
}
