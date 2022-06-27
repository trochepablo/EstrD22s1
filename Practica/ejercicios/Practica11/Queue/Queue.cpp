#include <iostream>
#include "Queue.h"
using namespace std;

struct NodoQ {
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt { // INV. REP.:
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo. primero es NULL si ultimo es NULL
    NodoQ* ultimo; // puntero al ultimo nodo. Ultimo es distinto a NULL entonces ultimo->siguiente es igual a NULL
};

Queue emptyQ() { 
    QueueSt* queue = new QueueSt;
    queue->cantidad = 0;
    queue->primero = NULL;
    queue->ultimo = NULL;
    return queue;
}

bool isEmptyQ(Queue q) { return q->cantidad==0; }

int firstQ(Queue q) { return q->primero->elem; }

void Enqueue(int x, Queue q) {
    NodoQ* newNodo = new NodoQ;
    newNodo->elem = x;
    newNodo->siguiente = NULL;
    if (q->primero == NULL) {
        q->primero = newNodo;
        q->ultimo = newNodo;
    } else {
        q->ultimo->siguiente = newNodo;
        q->ultimo = newNodo;
    }
    q->cantidad += q->cantidad;
}

void Dequeue(Queue q) {
    // PRECOND: La cola no esta vacia
    NodoQ* aux = q->primero;
    q->primero = q->primero->siguiente;
    if (q->primero == NULL) {
        q->ultimo = NULL;
    }
    delete aux;
    q->cantidad -= q->cantidad;    
}

int lengthQ(Queue q) { return q->cantidad; }

void MergeQ(Queue q1, Queue q2) {
    while (q2->primero!=NULL)
    {
        Enqueue(q2->primero->elem, q1);
        Dequeue(q2);
    }
    delete q2;
}

void DestroyQ(Queue q) {
    while (q->primero!=NULL)
    {
        Dequeue(q);
    }
    delete q;
}