#include "Img.h"

#define DIR        int
#define HOJA       99
#define HORIZONTAL 42
#define VERTICAL   17

struct ITreeSt {     
    DIR      division;    
    Color    color;       
    ITreeSt* first;    
    ITreeSt* second;
};
 /* INV.REP.
    // COMPLETAR
    OBS: si division es
      - HOJA, entonces color es el color del bloque representado  
      - HORIZONTAL, entonces first es la parte izquierda y second la derecha
      - VERTICAL, entonces first es la parte superior y second la inferior
 */

struct ImgSt {
    // COMPLETAR
    int heigth;
    int width;
    int size;
    ITreeSt* imgTree;
};
 /* INV.REP.
    // COMPLETAR
     {- INV.REP.: IT w h s t
        * s = size t
        * s <= w*h
        * en t, 
           - cada nodo H tiene todos hijos V o S
           - cada nodo V tiene todos hijos H o S
     -}
 */

//---------------------------------------------------------
// sizeImg
//---------------------------------------------------------
int sizeImg(Img img) {
    // COMPLETAR
    return img->size;
}

//---------------------------------------------------------
// createImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
ITreeSt* loadIT(int iw, int ih
               ,int fw, int fh
               ,int n, Matrix m, DIR d) {
  for (int i = iw; i < fw; i++)
  {
    for (int j = ih; j < fh; j++)
    {
      M_getAt(m, i, j);
    }
  }
  
}

// PRECOND: w es potencia de 2, m es de w*w
Img createImg(Matrix m, int w) {
  // COMPLETAR
  int dimension = w*w;
  Img* img = new Img;
  img.heigth = m->height;
  img.width = m->width;
  img.size = dimension;
  img->imgTree = loadIT(1,1,m->width, m->height, dimension, m, HORIZONTAL);
  return img;
}

//---------------------------------------------------------
// CompressImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
// OBS: el int retornado es la cantidad final de hojas del t luego de modificarlo
int CompressIT(ITreeSt* t) {  
}

void CompressImg(Img img) {
  // COMPLETAR
}

//---------------------------------------------------------
// RenderImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
void RenderIT(int x, int y, int w, int h, ITreeSt* t) {
}

void RenderImg(Img img) {
  // COMPLETAR
}
