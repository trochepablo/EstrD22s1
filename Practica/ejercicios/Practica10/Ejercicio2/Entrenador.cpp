#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"
using namespace std;

// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
// un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    EntrenadorSt* newEnt = new EntrenadorSt;
    newEnt->nombre = nombre;
    newEnt->cantPokemon = cantidad;
    newEnt->pokemon = pokemon;
    return newEnt;
}

//Devuelve el nombre del entrenador.
string nombreDeEntrenador(Entrenador e) {
    return e->nombre;
}

//Devuelve la cantidad de pokémon que posee el entrenador.
int cantidadDePokemon(Entrenador e) {
    return e->cantPokemon;
}

//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int cantidadPokemonDe = 0;
    for (int i = 0; i < e->cantPokemon; i++)
    {
        if (tipoDePokemon(e->pokemon[i]) == tipo)
        {
            cantidadPokemonDe = cantidadPokemonDe++;
        }
    }
    return cantidadPokemonDe;
}

// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i − 1 pokémon.
Pokemon pokemonNro(int i, Entrenador e) {
    return e->pokemon[i-1];
}

// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
// posee al menos un pokémon que le gane.
bool leGanaATodos(Entrenador e1, Entrenador e2) {
    int index = (e2->cantPokemon) - 1;
    Pokemon p1 = (e2->pokemon[index]);
    Pokemon p2 = (e1->pokemon[index]);
    while (!superaA(p1, p2) && index > -1)
    {
        index = index - 1;
    }
    return superaA(p1, p2);
}