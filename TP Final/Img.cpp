#include "Img.h"
#include "Color.h"

#define DIR        int
#define HOJA       99
#define HORIZONTAL 42
#define VERTICAL   17
#define UNITSIZE   50

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
  if (n == 1)
  {
    ITreeSt* leave = new ITreeSt;
    leave->color = M_getAt(m, iw, ih);
    leave->division = NULL;
    leave->first = NULL;
    leave->second = NULL;
    return leave;
  }
  if (d == HORIZONTAL)
  {
    ITreeSt* node = new ITreeSt;
    node->color = NULL;
    node->division = HORIZONTAL;
    node->first = loadIT(iw, ih, fw, fh/2, n/2, m, VERTICAL);
    node->second = loadIT(iw, ih+fh/2, fw, fh/2, n/2, m, VERTICAL);
    return node;
  }
  else if (d == VERTICAL) {
    ITreeSt* node = new ITreeSt;
    node->color = NULL;
    node->division = VERTICAL;
    node->first = loadIT(iw, ih, fw, fh/2, n/2, m, HORIZONTAL);
    node->second = loadIT(iw+fw/2, ih, fw, fh/2, n/2, m, HORIZONTAL);
    return node;
  }
}

// PRECOND: w es potencia de 2, m es de w*w
Img createImg(Matrix m, int w) {
  // COMPLETAR
  int dimension = w*w;
  ImgSt* img = new ImgSt;
  img->heigth = M_height(m);
  img->width = M_width(m);
  img->size = dimension;
  img->imgTree = loadIT(1, 1, M_width(m), M_height(m), dimension, m, HORIZONTAL);
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
  if (t->color!=NULL&&t->first==NULL&&t->second==NULL)
  {
    RenderBlock(x, y, w, h, t->color);
  }
  
  if (t->color==NULL && t->division==HORIZONTAL)
  {
    RenderIT(x, y, w, (h/2), t->first);
    RenderIT(x, y+(h/2), w, (h/2), t->second);
  }
  else if (t->color==NULL && t->division==VERTICAL)
  {
    RenderIT(x, y, (w/2),h, t->first);  
    RenderIT(x+(w/2),y,(w/2), h, t->second);
  }
}

void RenderImg(Img img) {
  // COMPLETAR
  WrapSVGTagAndRenderContent(img);
}

void WrapSVGTagAndRenderContent(Img img) {
  int w = img->width;
  int h = img->heigth;
  cout << "<svg height=\"" << renderSize(h) << "\""
        << " width=\"" << renderSize(w) << "\">";
  RenderIT(0, 0, w, h, img->imgTree);
  cout << "\n</svg>" << endl;
}

int renderSize(int s){
  return (UNITSIZE * s);
}

void RenderBlock(int x, int y, int w, int h, Color c) {
  cout << "\n<rect x=\"" << renderSize(x) << '\"'
  << " y=\"" << renderSize(y) << '\"'
  << " width=\"" << renderSize(w) << '\"'
  << " height=\"" << renderSize(h) << '\"'
  << " style=\" fill:"; RenderColor(c, 4);
  cout << ";stroke-width:3;stroke:rgb(0,0,0)\""
  << " />";
}