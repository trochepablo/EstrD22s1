#include <iostream>
using namespace std;
// Ejercicio 5
// Dada la estructura de fracciones representada como struct en C++, definir las siguientes funciones
// sobre fracciones. Recordar probar las implementaciones en un procedimiento main.
struct Fraccion {
    int numerador;
    int denominador;
};
// // Propósito: construye una fraccion
// // Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.denominador = denominador;
    f.numerador = numerador;
    return f;
}

// // Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// // Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// // Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return f.numerador / f.denominador;
}

// // Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// // (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion result;
    result.numerador = f1.numerador * f2.numerador;
    result.denominador = f1.denominador * f2.denominador;
    return result;
}

// // Propósito: devuelve una fracción que resulta
// // de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    Fraccion reduce;
	if(p.denominador == 1) {
        reduce.numerador = p.numerador;
		reduce.denominador = p.denominador;
	} else {
        int b=2;
        while(b <= p.numerador) {
            if((p.numerador % b) == 0 && (p.denominador % b) == 0){
                reduce.numerador = p.numerador / b;
                reduce.denominador = p.denominador / b;
            } else{
                b++;
            }
	    }
    }
}


Fraccion sumF(Fraccion f1, Fraccion f2) {
    Fraccion result;
    result.numerador=f1.numerador*f2.denominador+f2.numerador*f1.denominador;
    result.denominador=f1.denominador*f2.denominador;
    return result;
}
