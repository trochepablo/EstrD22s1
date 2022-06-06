#include <iostream>
using namespace std;

void printN(int n, string s){
    for (int i=0; i < n; i++)
    {
        cout << s << endl;
    }
}

void printNR(int n, string s)
{
    if (n > 0)
    {
        cout << s << endl;
        printNR(n-1, s);
    } else {
        return;
    }
}

// 2. 
// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n){
    for (int i = n; i >= 0; i--)
    {
        cout << i << endl;
    }
}

void cuentaRegresivaR(int n){
    if (n >= 0){
        cout << n << endl;
        cuentaRegresivaR(n-1);
    } else {
        return;
    }
}

// 3. 
void desdeCeroHastaN(int n)
{
    for (int i = 0; i <= n; i++)
    {
        cout << i << endl;
    }
}

void desdeCeroHastaNM(int desde, int hasta){
    if (desde <= hasta)
    {
        cout << desde << endl;
        desdeCeroHastaNM(desde+1, hasta);
    } else {
        return;
    }
    
}

void desdeCeroHastaNR(int n)
{
    desdeCeroHastaNM(0, n);
}

// 4. 
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m){
    int mult = 0;
    for (int i = 0; i < m; i++)
    {
        mult = mult + n;
    }
    return mult;
}

int multR(int n, int m){
    if (m > 0)
    {
        return n + multR(n, m-1);
    } else {
        return 0;
    }
}


// 5. 
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void primerosN(int n, string s){
    for (int i = 0; i < n; i++)
    {
        cout << s[i];
    }
}

void primerosNRH(int desde, int hasta, string s){
    if (desde < hasta)
    {
        cout << s[desde];
        primerosNRH(desde+1, hasta, s);
    } else {
        return;
    }
    
}

void primerosNR(int n, string s){
    primerosNRH(0, n, s);
}

// Propósito: indica si un char c aparece en el string s.
// 6. 
bool pertenece(char c, string s){
    int i = 0;
    
    while (c != s[i] && s.length() >= i)
    {
        i = i+1;
    }
    
    return c == s[i];
}

bool perteneceRH(int desde, int hasta, char c, string s){
    if(desde > hasta){
        return false;
    } else {
        return c == s[desde] || perteneceRH(desde+1, hasta, c, s);
    }
}

bool perteneceR(char c, string s){
    return perteneceRH(0, s.length(), c, s);
}

// 7. 
// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s){
    int cantApariciones = 0;
    for (int i = 0; i < s.length(); i++)
    {
        if (c == s[i])
        {
            cantApariciones = cantApariciones + 1;
        }
        
    }
    return cantApariciones;
}

int aparicionesRH(int desde, int hasta, char c, string s){
    if(desde > hasta){
        return 0;
    } else {
        if (c == s[desde]) {
            return 1 + perteneceRH(desde+1, hasta, c, s);  
        } else {
            return perteneceRH(desde+1, hasta, c, s);
        }
    }
}

int aparicionesR(char c, string s){
    return perteneceRH(0, s.length(), c, s);
}