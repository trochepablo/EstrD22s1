#include "ArrayListSt.h"
#include <iostream>
using namespace std;

//Crea una lista con 0 elementos.
//Nota: empezar el array list con capacidad 16.
ArrayList newArrayList() {
    ArrayListSt* arr = new ArrayListSt[16];
    arr->cantidad = 0;
    arr->capacidad = 16;
    arr->elementos = NULL;
    return arr;
}

//Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad) {
    ArrayListSt* arr = new ArrayListSt[capacidad];
    arr->cantidad = 0;
    arr->capacidad = capacidad;
    arr->elementos = NULL;
    return arr;
}

//Devuelve la cantidad de elementos existentes.
int lengthAL(ArrayList xs) {
    return xs->cantidad;
}

//Devuelve el iésimo elemento de la lista.
int get(int i, ArrayList xs) {
    return xs->elementos[i];
}

//Reemplaza el iésimo elemento por otro dado.
void set(int i, int x, ArrayList xs) {
    xs->elementos[i] = xs->elementos[x];
}

//Decrementa o aumenta la capacidad del array.
//Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
void resize(int capacidad, ArrayList xs) {
    ArrayListSt* resizeArr = new ArrayListSt[capacidad];
    resizeArr->cantidad = xs->cantidad;
    resizeArr->capacidad = capacidad;

    if (capacidad >= xs->capacidad)
    {
        resizeArr->elementos = xs->elementos;
    } 
    else 
    {
        for (int i = 0; i <= capacidad; i++)
        {
            resizeArr->elementos[i] = xs->elementos[i];
        }
    }
}

//Agrega un elemento al final de la lista.
void add(int x, ArrayList xs) {
    xs->elementos[(xs->cantidad)] = x;
    xs->cantidad = xs->cantidad++;
}

//Borra el último elemento de la lista.
void remove(ArrayList xs) {
    xs->elementos[(xs->cantidad-1)] = NULL;
}
