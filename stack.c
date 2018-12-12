#include <stdlib.h>

    typedef struct node_t {
        int key;
        struct node_t *next;
    } node;

    static node *head, *z, *t;

    stackinit_() {
         head = (node *) malloc(sizeof *head);
         z = (node *) malloc(sizeof *z);
         head->next = z; head->key=0;
         z->next = z;
         z->key = 0;
    }
    push_(int* p) {
         int v;
         v = *p;
         t = (node *) malloc(sizeof *t); 
         t->key = v; t->next = head->next;  
         head->next =t; 
    }
    pop_(int* x) {
         t = head->next; head->next = t->next;
         *x = t->key;
         free(t);
    }
    stackempty_(int* i) {
        *i = 0;
            if(head->next == z) *i = 1;
    }
    stackflush_() {
         free(head);
         free(z);
    }

