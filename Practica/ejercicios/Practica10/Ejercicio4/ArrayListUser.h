#include <iostream>
#include "ArrayListSt.h"
using namespace std;

//Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs) {
    int suma = 0;
    for (int i = 0; i < lengthAL(xs) - 1; i++)
    {
        suma = suma + get(i, xs);
    }
    return suma;
}

//Incrementa en uno todos los elementos.
void sucesores(ArrayList xs) {
    for (int i = 0; i < lengthAL(xs) - 1; i++)
    {
        set(i, (get(i, xs) + 1), xs);
    }
}

//Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs) {
    int i = 0;
    while (x == get(i, xs) && i < (lengthAL(xs) - 1))
    {
        i = i++;
    }
    return x == get(i, xs);
}

//Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs) {
    int cant = 0;
    for (int i = 0; i < (lengthAL(xs) - 1); i++)
    {
        if (x == get(i, xs))
        {
            cant = cant++;
        }
        
    }
    return cant;
}

//Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys) {
    resize(lengthAL(xs) + lengthAL(ys), xs);
    for (int i = 0; i < lengthAL(ys)-1; i++)
    {
        add(get(i, ys), xs);
    }
    return xs;
}

//Devuelve el elemento más chico de la lista.
int minimo(ArrayList xs) {
    int minimo = get(0, xs);
    for (int i = 1; i < lengthAL(xs)-1; i++)
    {
        if (minimo > get(i, xs))
        {
            minimo = get(i, xs);
        }
    }
}