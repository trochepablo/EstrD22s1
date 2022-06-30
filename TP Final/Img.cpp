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
 /* INV.REP.:
      * si color es null entonces first y second no son null
      * si first y second no son null entonces color es null
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
     {- INV.REP.:
        * size = size imgTree
        * size <= width*heigth
        * imgTree no es null
        * en imgTree, 
           - cada nodo H tiene todos hijos ITreeSt
           - cada nodo V tiene todos hijos ITreeSt
     -}
 */

// AUXILIARES
ITreeSt* loadIT(int iw, int ih
               ,int fw, int fh
               ,int n, Matrix m, DIR d) {
  if (n == 1)
  {
    ITreeSt* leave = new ITreeSt;
    leave->color = M_getAt(m, iw, ih);
    leave->first = NULL;
    leave->second = NULL;
    return leave;
  }
  ITreeSt* node = new ITreeSt;
  if (d == HORIZONTAL)
  {
    node->color = NULL;
    node->division = HORIZONTAL;
    node->first = loadIT(iw, ih, fw, (fh/2), (n/2), m, VERTICAL);
    node->second = loadIT(iw, ih+(fh/2), fw, (fh/2), (n/2), m, VERTICAL);
  }
  else if (d == VERTICAL) {
    node->color = NULL;
    node->division = VERTICAL;
    node->first = loadIT(iw, ih, (fw/2), fh, (n/2), m, HORIZONTAL);
    node->second = loadIT(iw+(fw/2), ih, (fw/2), fh, n/2, m, HORIZONTAL);
  }
  return node;
}

bool isLeave(ITreeSt* t) { return t->color!=NULL&&t->first==NULL&&t->second==NULL; }

int BuildNode(int n1, int n2, ITreeSt* t1, ITreeSt* t2, ITreeSt* t) {
  if (isLeave(t1) && isLeave(t2))
  {
    if (t1->color==t2->color)
    {
      t->color = t1->color;
      t->first = NULL;
      t->second = NULL;
      return 1;
    } else {
      return 2;
    }
  } else {
    return n1 + n2;
  }
}

// OBS: el int retornado es la cantidad final de hojas del t luego de modificarlo
int CompressIT(ITreeSt* t) {
  int amountOfTotalLeaves = 1;
  if (!isLeave(t))
  {
    int amountOfLeaves1 = CompressIT(t->first);
    int amountOfLeaves2 = CompressIT(t->second);
    amountOfTotalLeaves += BuildNode(amountOfLeaves1, amountOfLeaves2, t->first, t->second, t);
  }
  return amountOfTotalLeaves;
}

int renderSize(int s){
  return (UNITSIZE * s);
}

void RenderBlock(int x, int y, int w, int h, Color c) {
  cout << "\n<rect x=\"" << renderSize(x) << '\"'
  << " y=\"" << renderSize(y) << '\"'
  << " width=\"" << renderSize(w) << '\"'
  << " height=\"" << renderSize(h) << '\"'
  << " style=\" fill:"; RenderColor(c, 0);
  cout << ";stroke-width:3;stroke:rgb(0,0,0)\""
  << " />";
}

void RenderIT(int x, int y, int w, int h, ITreeSt* t) {
  if (isLeave(t))
  {
    RenderBlock(x, y, w, h, t->color);
  } 
  else if (t->color==NULL && t->division==HORIZONTAL)
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

void WrapSVGTagAndRenderContent(Img img) {
  int w = img->width;
  int h = img->heigth;
  cout << "<svg height=\"" << renderSize(h) << "\""
        << " width=\"" << renderSize(w) << "\">";
  RenderIT(0, 0, w, h, img->imgTree);
  cout << "\n</svg>" << endl;
}


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
void CompressImg(Img img) {
  img->size = CompressIT(img->imgTree);
}
//---------------------------------------------------------
// RenderImg
//---------------------------------------------------------
void RenderImg(Img img) {
  WrapSVGTagAndRenderContent(img);
}