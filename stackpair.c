#include <stdlib.h>

    typedef struct node_t {
        int key;
        struct node_t *next;
    } node;

    static node *heada, *za, *ta;
    static node *headb, *zb, *tb;

    stackpairinit_() {
         heada = (node *) malloc(sizeof *heada);
         za = (node *) malloc(sizeof *za);
         heada->next = za; heada->key=0;
         za->next = za;
         za->key = 0;

         headb = (node *) malloc(sizeof *headb);
         zb = (node *) malloc(sizeof *zb);
         headb->next = zb; headb->key=0;
         zb->next = zb;
         zb->key = 0;
       }
    stackpairflush_() {
             free(heada);
             free(headb);
             free(za);
             free(zb);
       }
    pushpair_(int* pa, int* pb) {
         int va;
         int vb;
         va = *pa;
         ta = (node *) malloc(sizeof *ta);   
         ta->key = va; ta->next = heada->next;  
         heada->next =ta;   

         vb = *pb;
         tb = (node *) malloc(sizeof *tb);   
         tb->key = vb; tb->next = headb->next;  
         headb->next =tb;   
       }
    poppair_(int* xa, int* xb) {
         ta = heada->next; heada->next = ta->next;
         *xa = ta->key;
         free(ta);
         tb = headb->next; headb->next = tb->next;
         *xb = tb->key;
         free(tb);
       }
    stackpairempty_(int* i) {
        *i = 0;
        if(heada->next == za) *i = 1;
    }

