#include <iostream>
#include "Pokemon.h"
using namespace std;

//Dado un tipo devuelve un pokémon con 100 % de energía.
Pokemon consPokemon(TipoDePokemon tipo) {
    PokeSt* newPokemon = new PokeSt;
    newPokemon->tipo = tipo;
    return newPokemon;
}

//Devuelve el tipo de un pokémon.
TipoDePokemon tipoDePokemon(Pokemon p) {
    return p->tipo;
}

//Devuelve el porcentaje de energía.
int energia(Pokemon p) {
    return p->vida;
}

//Le resta energía al pokémon.
void perderEnergia(int energia, Pokemon p) {
    if (energia >= p->vida)
    {
        p->vida = 0;
    } else {
        p->vida = p->vida - energia;
    }
}

//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2) {
    return  (p1->tipo == "agua" && p2->tipo == "fuego") ||
            (p1->tipo == "fuego" && p2->tipo == "planta") ||
            (p1->tipo == "planta" && p2->tipo == "agua");
}