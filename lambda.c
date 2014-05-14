/* Lambda
 * ======
 *
 * Based on the book "Build Your Own Lisp" by Daniel Holden
 *
 * http://www.buildyourownlisp.com/
 *
 * Copyright (c) 2014 Daniel Holden
 * Copyright (c) 2014 Joost Kremers
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
 * DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <unistd.h>
#include <editline/readline.h>
#include <editline/history.h>
#include "mpc.h"

#define LASSERT(args, cond, fmt, ...)           \
  if (!(cond)) {                                \
    lval* err = lval_err(fmt, ##__VA_ARGS__);   \
    lval_del(args);                             \
    return err;                                 \
  }

#define LASSERT_NELIST(fn, args)                                \
  if (args->cell[0]->count == 0) {                              \
    lval* err = lval_err("%s: argument cannot be empty", fn);   \
    lval_del(args);                                             \
    return err;                                                 \
  }

#define LASSERT_NARGS(fn, args, n)                                      \
  if (args->count != n) {                                               \
    lval* err = lval_err("%s: wrong number of arguments. Got %i, expected %i", fn, args->count, n); \
    lval_del(args);                                                     \
    return err;                                                         \
  }

#define LASSERT_VAR_NARGS(fn, args, n, m)                               \
  if (args->count < n || args->count > m) {                             \
    lval* err = lval_err("%s: wrong number of arguments. Got %i, expected %i-%i", fn, args->count, n, m); \
    lval_del(args);                                                     \
    return err;                                                         \
  }

#define LASSERT_TYPE(fn, args, n, expt)                                 \
  if (args->cell[n]->type != expt) {                                    \
    lval* err = lval_err("%s: wrong type argument. Got %s, expected %s", fn, ltype_name(args->cell[n]->type), ltype_name(expt)); \
    lval_del(args);                                                     \
    return err;                                                         \
  }

#ifdef DEBUG
int eval_level = 0;
#endif

/* Forward declarations */

/* Parsers */
mpc_parser_t* Number;
mpc_parser_t* Boolean;
mpc_parser_t* Symbol;
mpc_parser_t* String;
mpc_parser_t* Comment;
mpc_parser_t* Sexpr;
mpc_parser_t* Qexpr;
mpc_parser_t* Expr;
mpc_parser_t* Lambda;

/* lval and lenv */
struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

lenv* lenv_new(void);
lenv* lenv_copy(lenv* e);
void lenv_del(lenv* e);

/* Lisp Value */
typedef enum { LVAL_ERR, LVAL_NUM, LVAL_BOOL, LVAL_SYM, LVAL_STR, LVAL_FUN, LVAL_MAC, LVAL_SEXPR, LVAL_QEXPR } lval_t;
/*                 0         1         2          3         4         5         6         7           8       */

char* ltype_name(int t) {
  switch(t) {
  case LVAL_FUN: return "Function";
  case LVAL_MAC: return "Macro";
  case LVAL_NUM: return "Number";
  case LVAL_STR: return "String";
  case LVAL_BOOL: return "Boolean";
  case LVAL_ERR: return "Error";
  case LVAL_SYM: return "Symbol";
  case LVAL_SEXPR: return "S-Expression";
  case LVAL_QEXPR: return "Q-Expression";
  default: return "Unknown";
  }
}

typedef lval*(*lbuiltin)(lenv*, lval*);

struct lval {
  int type;

  /* Basic */
  double num;
  char* err;
  char* sym;
  char* str;
  bool boolean;

  /* Functions */
  lbuiltin builtin;
  lenv* env;
  lval* formals;
  lval* body;

  /* Expression */
  int count;
  lval** cell;

};

/* Constructors */
lval* lval_num(double x) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

lval* lval_str(char* s) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_STR;
  v->str = malloc(strlen(s) + 1);
  strcpy(v->str, s);
  return v;
}

lval* lval_bool(bool b) {

  /* b should be a string, either "true" or "false" */
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_BOOL;
  v->boolean = b;
  return v;
}

lval* lval_err(char* fmt, ...) {
  lval * v = malloc(sizeof(lval));
  v->type = LVAL_ERR;

  /* Create a va list and initialize it */
  va_list va;
  va_start(va, fmt);

  /* Allocate 512 bytes of space and print error */
  v->err = malloc(512);
  vsnprintf(v->err, 511, fmt, va);

  /* Reallocate */
  v->err = realloc(v->err, strlen(v->err)+1);

  va_end(va);

  return v;
}

lval* lval_sym(char* s) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

lval* lval_fun(lbuiltin func, lval_t type) {
  lval* v = malloc(sizeof(lval));
  v->type = type;
  v->builtin = func;
  return v;
}

lval* lval_lambda(lval* formals, lval* body) {
  lval* v = malloc(sizeof(lval));
  v-> type = LVAL_FUN;

  /* Set builtin to NULL */
  v->builtin = NULL;

  /* Build new environment */
  v->env = lenv_new();

  /* Set formals and body */
  v->formals = formals;
  v->body = body;
  return v;
}

lval* lval_macro(lval* formals, lval* body) {
  lval* v = malloc(sizeof(lval));
  v-> type = LVAL_MAC;

  /* Set builtin to NULL */
  v->builtin = NULL;

  /* Build new environment */
  v->env = lenv_new();

  /* Set formals and body */
  v->formals = formals;
  v->body = body;
  return v;
}

lval* lval_sexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lval* lval_qexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

/* Copy an lval */
lval* lval_copy(lval* v) {

  lval* x = malloc(sizeof(lval));
  x->type = v->type;

  switch(v->type) {

  case LVAL_NUM: x->num = v->num; break;
  case LVAL_BOOL: x->boolean = v->boolean; break;

  case LVAL_FUN:
  case LVAL_MAC:
    if(v->builtin) {
      x->builtin = v->builtin;
    } else {
      x->builtin = NULL;
      x->env = lenv_copy(v->env);
      x->formals = lval_copy(v->formals);
      x->body = lval_copy(v->body);
    }
    break;

  case LVAL_ERR: x->err = malloc(strlen(v->err) + 1); strcpy(x->err, v->err); break;
  case LVAL_SYM: x->sym = malloc(strlen(v->sym) + 1); strcpy(x->sym, v->sym); break;
  case LVAL_STR: x->str = malloc(strlen(v->str) + 1); strcpy(x->str, v->str); break;

    /* Copy lists by copying each sub-expression */
  case LVAL_SEXPR:
  case LVAL_QEXPR:
    x->count = v->count;
    x->cell = malloc(sizeof(lval*) * x->count);
    for (int i = 0; i < x->count; i++) {
      x->cell[i] = lval_copy(v->cell[i]);
    }
    break;
  }

  return x;
}

/* Delete an lval and free its memory */
void lval_del(lval* v) {

  switch (v->type) {

  case LVAL_NUM: break;
  case LVAL_BOOL: break;

  case LVAL_FUN:
  case LVAL_MAC:
    if (!v->builtin) {
      lenv_del(v->env);
      lval_del(v->formals);
      lval_del(v->body);
    }
    break;

  case LVAL_ERR: free(v->err); break;
  case LVAL_SYM: free(v->sym); break;
  case LVAL_STR: free(v->str); break;

    /* if Sexpr/Qexpr then delete all elements inside */
  case LVAL_SEXPR:
  case LVAL_QEXPR:
    for (int i = 0; i < v->count; i++) {
      lval_del(v->cell[i]);
    }
    /* Also free the memory allocated to contain the pointers */
    free(v->cell);
    break;
  }

  /* Finally free the memory allocated for the lval struct itself */
  free(v);
}

bool lval_equal(lval* x, lval* y) {

  /* Different types are always unequal */
  if (x->type != y->type) { return false; }

  switch (x->type) {
  case LVAL_NUM: return (x->num == y->num ? true : false);
  case LVAL_ERR: return (strcmp(x->err, y->err) == 0 ? true : false);
  case LVAL_SYM: return (strcmp(x->sym, y->sym) == 0 ? true : false);
  case LVAL_STR: return (strcmp(x->str, y->str) == 0 ? true : false);
  case LVAL_BOOL: return (x->boolean == y->boolean ? true : false);

    /* If Builtins compare functions, otherwise compare formals and body */
  case LVAL_FUN:
  case LVAL_MAC:
    if (x->builtin || y->builtin) {
      return (x->builtin == y->builtin ? true : false);
    } else {
      return (lval_equal(x->formals, y->formals) && lval_equal(x->body, y->body) ? true : false);
    }
    break;

  case LVAL_QEXPR:
  case LVAL_SEXPR:
    if (x->count != y->count) { return false; }
    /* If any element is not equal then the whole list is not equal */
    for (int i = 0; i < x->count; i++) {
      if (!lval_equal(x->cell[i], y->cell[i])) { return false; }
    }
    /* Otherwise lists must be equal */
    return true;
    break;
  }
  return false;
}

/* Functions for reading an lval */

lval* lval_add(lval* v, lval* x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  v->cell[v->count-1] = x;
  return v;
}

lval* lval_read_num(mpc_ast_t* t) {
  errno = 0;
  double x = strtod(t->contents, NULL);
  return errno != ERANGE ? lval_num(x) : lval_err("Invalid number");
}

lval* lval_read_str(mpc_ast_t* t) {
  /* Cut off the final quote character */
  t->contents[strlen(t->contents)-1] = '\0';
  /* Copy the string missing out the first quote character */
  char* unescaped = malloc(strlen(t->contents + 1) + 1);
  strcpy(unescaped, t->contents + 1);
  /* Pass through the unescape function */
  unescaped = mpcf_unescape(unescaped);
  /* Construct a new lval using the string */
  lval* str = lval_str(unescaped);
  /* Free the string and return */
  free(unescaped);
  return str;
}

lval* lval_read(mpc_ast_t* t) {

  /* If Number, Boolean or Symbol, return conversion to that type */
  if (strstr(t->tag, "number")) { return lval_read_num(t); }
  if (strstr(t->tag, "symbol")) { return lval_sym(t->contents); }
  if (strstr(t->tag, "boolean")) { return lval_bool(strcmp(t->contents, "true") == 0 ? true : false); }
  if (strstr(t->tag, "string")) { return lval_read_str(t); }

  /* If root (>), sexpr or qexpr then create empty list */
  lval* x = NULL;
  if (strcmp(t->tag, ">") == 0 || strstr(t->tag, "sexpr")) {
    x = lval_sexpr();
  }

  if (strstr(t->tag, "qexpr")) { x = lval_qexpr(); }

  /* Fill this list with any valid expression contained within */
  for (int i = 0; i < t->children_num; i++) {
    if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
    if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "'") == 0) { continue; }
    if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
    if (strstr(t->children[i]->tag, "comment")) { continue; }
    x = lval_add(x, lval_read(t->children[i]));
  }

  return x;
}

/* Print lval to file descriptor */
void lval_fprint(FILE* stream, lval* v);

void lval_fprint_fn(FILE* stream, lval* v) {
  /* Print the code of a user function/macro */
  /* This is for use in builtin_print */
  if (v->builtin) {
    fprintf(stream, "<builtin %s at %p>", (v->type == LVAL_FUN ? "function" : "macro"), (void*)v);
  } else {
    fprintf(stream, "(%s ", v->type == LVAL_FUN ? "\\" : "^");
    lval_fprint(stream, v->formals);
    fprintf(stream, " ");
    lval_fprint(stream, v->body);
    fprintf(stream, ")");
  }
}

void lval_fprint_expr(FILE* stream, lval* v, lval_t type) {
  if (type == LVAL_QEXPR) { fprintf(stream, "'"); }

  fprintf(stream, "(");

  for (int i = 0; i < v->count; i++) {

    /* Print Value contained within */
    lval_fprint(stream, v->cell[i]);

    /* Don't print trailing space if last element */
    if (i != (v->count-1)) {
      fprintf(stream, " ");
    }
  }
  fprintf(stream, ")");
}

void lval_fprint_str(FILE* stream, lval* v) {
  /* Make a copy of the string */
  char* escaped = malloc(strlen(v->str) + 1);
  strcpy(escaped, v->str);
  /* Pass it through the escape function */
  escaped = mpcf_escape(escaped);
  /* Print it between " characters */
  fprintf(stream, "\"%s\"", escaped);
  /* free the copied string */
  free(escaped);
}

void lval_fprint(FILE* stream, lval* v) {
  switch (v->type) {
  case LVAL_NUM: fprintf(stream, "%g", v->num); break;
  case LVAL_ERR: fprintf(stream, "Error: %s", v->err); break;
  case LVAL_BOOL: fprintf(stream, "%s", v->boolean ? "true" : "false"); break;
  case LVAL_SYM: fprintf(stream, "%s", v->sym); break;
  case LVAL_STR: lval_fprint_str(stream, v); break;
  case LVAL_FUN:
    if (v->builtin) {
      fprintf(stream, "<builtin function at %p>", (void*)v);
    } else {
      fprintf(stream, "<user defined function at %p>", (void*)v);
    }
    break;
  case LVAL_MAC:
    if (v->builtin) {
      fprintf(stream, "<builtin macro at %p>", (void*)v);
    } else {
      fprintf(stream, "<user defined macro at %p>", (void*)v);
    }
    break;
  case LVAL_SEXPR: lval_fprint_expr(stream, v, LVAL_SEXPR); break;
  case LVAL_QEXPR: lval_fprint_expr(stream, v, LVAL_QEXPR); break;
  }
}

void lval_fprintln(FILE* stream, lval* v) { lval_fprint(stream, v); fprintf(stream, "\n"); }

/* Print lval to stdout */
void lval_print_fn(lval* v) { lval_fprint_fn(stdout, v); }
void lval_print_expr(lval* v, lval_t type) { lval_fprint_expr(stdout, v, type); }
void lval_print_str(lval* v) { lval_fprint_str(stdout, v); }
void lval_print(lval* v) { lval_fprint(stdout, v); }
void lval_println(lval* v) { lval_fprintln(stdout, v); }

/* Environment struct */
struct lenv {
  lenv* par;
  int count;
  char** syms;
  char** docs;
  lval** vals;
};

lenv* lenv_new(void) {
  lenv* e = malloc(sizeof(lenv));
  e->par = NULL;
  e->count = 0;
  e->syms = NULL;
  e->docs = NULL;
  e->vals = NULL;
  return e;
}

void lenv_del(lenv* e) {
  for (int i = 0; i < e->count; i++) {
    free(e->syms[i]);
    free(e->docs[i]);
    lval_del(e->vals[i]);
  }
  free(e->syms);
  free(e->docs);
  free(e->vals);
  free(e);
}

lval* lenv_get(lenv* e, lval* k) {

  for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0) { return lval_copy(e->vals[i]); }
  }
  /* If no symbol found check in parent otherwise return error */
  if (e->par) {
    return lenv_get(e->par, k);
  } else {
    return lval_err("Unbound symbol '%s'", k->sym);
  }
}

lval* lenv_doc(lenv* e, lval* k) {

  for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0) {
      if (e->docs[i]) { return lval_str(e->docs[i]); }
      else { return lval_err("Symbol not documented"); }
    }
  }
  /* If no symbol found check in parent otherwise return error */
  if (e->par) {
    return lenv_doc(e->par, k);
  } else {
    return lval_err("Unbound symbol '%s'", k->sym);
  }
}

void lenv_put(lenv* e, lval* key, lval* val, char* doc) {

  /* Iterate over all items in environment */
  /* This is to see if variable already exists */
  for (int i = 0; i < e->count; i++) {
    /* If variable is found delete item at that position */
    /* And replace with variable supplied by user */
    if (strcmp(e->syms[i], key->sym) == 0) {
      lval_del(e->vals[i]);
      if (doc) {
        e->docs[i] = realloc(e->docs[i], strlen(doc) + 1);
        strcpy(e->docs[i], doc);
      } else {
        free(e->docs[i]);
        e->docs[i] = NULL;
      }
      e->vals[i] = lval_copy(val);
      return;
    }
  }

  /* If no existing entry found then allocate space for new entry */
  e->count++;
  e->vals = realloc(e->vals, sizeof(lval*) * e->count);
  e->docs = realloc(e->docs, sizeof(char*) * e->count);
  e->syms = realloc(e->syms, sizeof(char*) * e->count);

  /* Copy contents of lval, doc and symbol string into new location */
  e->vals[e->count-1] = lval_copy(val);
  if (doc) {
    e->docs[e->count-1] = malloc(strlen(doc) + 1);
    strcpy(e->docs[e->count-1], doc);
  } else {
    e->docs[e->count-1] = NULL;
  }
  e->syms[e->count-1] = malloc(strlen(key->sym)+1);
  strcpy(e->syms[e->count-1], key->sym);
}

void lenv_def(lenv* e, lval* key, lval* val, char* doc) {
  /* Find top environment and add (k, v) there */
  while (e->par) { e = e->par; }
  lenv_put(e, key, val, doc);
}

lenv* lenv_copy(lenv* e) {
  lenv* n = malloc(sizeof(lenv));
  n->par = e->par;
  n->count = e->count;
  n->syms = malloc(sizeof(char*) * n->count);
  n->docs = malloc(sizeof(char*) * n->count);
  n->vals = malloc(sizeof(lval*) * n->count);
  for (int i = 0; i < e->count; i++) {
    n->syms[i] = malloc(strlen(e->syms[i]) + 1);
    strcpy(n->syms[i], e->syms[i]);
    /* A symbol may have no doc string at all */
    if (e->docs[i]) {
      n->docs[i] = malloc(strlen(e->docs[i]) + 1);
      strcpy(n->docs[i], e->docs[i]);
    } else {
      n->docs[i] = NULL;
    }
    n->vals[i] = lval_copy(e->vals[i]);
  }
  return n;
}

/* Evaluation */

lval* lval_eval(lenv* e, lval* v);
lval* lval_eval_qexpr(lenv* e, lval* v);

lval* lval_pop(lval* v, int i) {
  /* Find the item at i */
  lval* x = v->cell[i];

  /* Shift the memory following the item at i over the top of it */
  memmove(&v->cell[i], &v->cell[i+1], sizeof(lval*) * (v->count-i-1));

  /* Decrease the count of items in the list */
  v->count--;

  /* Realocate the memory used */
  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  return x;
}

lval* lval_take(lval* v, int i) {
  lval* x = lval_pop(v, i);
  lval_del(v);
  return x;
}

lval* lval_join(lval* x, lval* y) {

  /* For each cell in y add it to x */
  while (y->count) {
    x = lval_add(x, lval_pop(y, 0));
  }

  /* Delete the empty y and return x */
  lval_del(y);
  return x;
}

lval* builtin_op(lenv* e, lval* a, char* op) {

  /* Ensure all arguments are numbers */
  for (int i = 0; i < a->count; i++) {
    LASSERT_TYPE(op, a, i, LVAL_NUM);
  }

  /* Pop the first element */
  lval* x = lval_pop(a, 0);

  /* If no arguments and sub then perform unary negation */
  if (strcmp(op, "-") == 0 && a->count == 0) { x->num = -x->num; }

  /* While there are still elements remaining */
  while (a->count > 0) {

    /* Pop the next element */
    lval* y = lval_pop(a, 0);

    /* Perform operation */
    if (strcmp(op, "+") == 0) { x->num += y->num; }
    if (strcmp(op, "-") == 0) { x->num -= y->num; }
    if (strcmp(op, "*") == 0) { x->num *= y->num; }
    if (strcmp(op, "pow") == 0) { x->num = pow(x->num, y->num); }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        lval_del(x); lval_del(y);
        x = lval_err("Division by zero"); break;
      }
      x->num /= y->num;
    }
    if (strcmp(op, "mod") == 0) {
      if (y->num == 0) {
        lval_del(x); lval_del(y);
        x = lval_err("Division by zero"); break;
      }
      x->num = (double)((long)x->num % (long)y->num);
    }

    /* Delete element now finished with */
    lval_del(y);
  }

  /* Delete input expression and return result */
  lval_del(a);
  return x;
}

lval* builtin_comp(lenv* e, lval* a, char* op) {

  /* For now, ensure all arguments are numbers */
  /* We should add comparison for other types later on */
  for (int i = 0; i < a->count; i++) {
    LASSERT_TYPE(op, a, i, LVAL_NUM);
  }

  /* Create a boolean lval */
  lval* v = lval_bool(false);

  /* Pop the first element */
  lval* x = lval_pop(a, 0);

  /* While there are still elements remaining */
  while (a->count > 0) {

    /* Pop the next element */
    lval* y = lval_pop(a, 0);

    /* Perform operation */
    if (strcmp(op, "=") == 0) { v->boolean = (x->num == y->num ? true : false); }
    if (strcmp(op, "/=") == 0) { v->boolean = (x->num != y->num ? true : false); }
    if (strcmp(op, "<") == 0)  { v->boolean = (x->num < y->num ? true : false); }
    if (strcmp(op, ">") == 0)  { v->boolean = (x->num > y->num ? true : false); }
    if (strcmp(op, "<=") == 0) { v->boolean = (x->num <= y->num ? true : false); }
    if (strcmp(op, ">=") == 0) { v->boolean = (x->num >= y->num ? true : false); }

    /* Delete element now finished with */
    lval_del(y);
  }

  /* Delete input expression and return result */
  lval_del(a); lval_del(x);
  return v;
}

lval* builtin_equal(lenv* e, lval* a) {
  LASSERT_NARGS("equal", a, 2);

  bool r = lval_equal(a->cell[0], a->cell[1]);
  lval_del(a);
  return lval_bool(r);
}

lval* builtin_null(lenv* e, lval* a) {
  /* Type: function */
  /* Format: null <qexpr> */
  /* Description: return true if <qexpr> is '() */

  LASSERT_NARGS("null", a, 1);
  LASSERT_TYPE("null", a, 0, LVAL_QEXPR);

  if (a->cell[0]->count == 0) { lval_del(a); return lval_bool(true); }
  else { lval_del(a); return lval_bool(false); }
}

lval* builtin_if(lenv* e, lval* a) {
  /* Type: macro */
  /* Format: if <bool> <then> (<else>) */
  /* Description: conditional evaluation */

  LASSERT_VAR_NARGS("if", a, 2, 3);

  lval *result;

  /* Evaluate first argument and make sure it's a boolean */
  a->cell[0] = lval_eval_qexpr(e, a->cell[0]);
  if (a->cell[0]->type == LVAL_ERR) { return lval_take(a, 0); }

  if (a->cell[0]->boolean) {
  if (a->cell[0]->type != LVAL_BOOL) { lval_del(a); return lval_err("if: not a boolean"); }
    result = lval_eval_qexpr(e, lval_pop(a, 1));
  } else {
    if (a->count == 3) {
      result = lval_eval_qexpr(e, lval_pop(a, 2));
    } else {
      result = lval_bool(false);
    }
  }

  lval_del(a);
  return result;
}

lval* builtin_and(lenv* e, lval* a) {
  /* Type: macro */
  /* Format: and <bool> & <bools> */
  /* Description: logical 'and' */

  lval* result;

  /* Check each argument in turn and if a false is found, return it. */
  while(a->count) {
    lval* arg = lval_pop(a, 0);

    result = lval_eval_qexpr(e, arg);
    if (result->type != LVAL_BOOL) {
      lval* err = lval_err("and: wrong type argument. Got %s, expected boolean", result->type);
      lval_del(a); lval_del(result);
      return err;
    }
    if (result->boolean == false) { lval_del(a); return result; }

    /* Delete the result of evaluating the current argument */
    lval_del(result);
  }

  /* Otherwise return true */
  lval_del(a);
  return lval_bool(true);
}

lval* builtin_or(lenv* e, lval* a) {
  /* Type: macro */
  /* Format: or <bool> & <bools> */
  /* Description: logical 'or' */

  lval* result;

  /* Check each argument in turn and if a true is found, return it. */
  while(a->count) {
    lval* arg = lval_pop(a, 0);

    result = lval_eval_qexpr(e, arg);
    if (result->type != LVAL_BOOL) {
      lval* err = lval_err("or: wrong type argument. Got %s, expected boolean", result->type);
      lval_del(a); lval_del(result);
      return err;
    }
    if (result->boolean == true) { lval_del(a); return result; }

    /* Delete the result of evaluating the current argument */
    lval_del(result);
  }

  /* Otherwise return false */
  lval_del(a);
  return lval_bool(false);
}

lval* builtin_not(lenv* e, lval* a) {
  /* Type: function */
  /* Format: not <bool> */
  /* Description: logical 'not' */

  LASSERT_NARGS("not", a, 1);
  LASSERT_TYPE("not", a, 0, LVAL_BOOL);
  lval* r = lval_take(a, 0);
  r->boolean = !r->boolean;
  return r;
}

lval* builtin_head(lenv* e, lval* a) {
  /* Type: function */
  /* Format: head <qexpr> */
  /* Description: return the first element of <qexpr> as a Q-expression */
  /* Example: head '(1 2 3 4) ==> '(1) */

  LASSERT_NELIST("head", a);
  LASSERT_NARGS("head", a, 1);
  LASSERT_TYPE("head", a, 0, LVAL_QEXPR);

  lval* v = lval_take(a, 0);
  while (v->count > 1) { lval_del(lval_pop(v, 1)); }
  return v;
}

lval* builtin_tail(lenv* e, lval* a) {
  /* Type: function */
  /* Format: tail <qexpr> */
  /* Description: return <qexpr> without its first element */
  /* Example: tail '(1 2 3 4) ==> '(2 3 4) */

  LASSERT_NELIST("tail", a);
  LASSERT_NARGS("tail", a, 1);
  LASSERT_TYPE("tail", a, 0, LVAL_QEXPR);

  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, 0));
  return v;
}

lval* builtin_last(lenv* e, lval* a) {
  /* Type: function */
  /* Format: last <qexpr> */
  /* Description: return last element of <qexpr> */
  /* Example: init '(1 2 3 4) ==> 4 */

  LASSERT_NELIST("last", a);
  LASSERT_NARGS("last", a, 1);
  LASSERT_TYPE("last", a, 0, LVAL_QEXPR);

  lval* v = lval_take(a, 0);
  int n = v->count-1;
  return lval_eval(e, lval_take(v, n));
}

lval* builtin_init(lenv* e, lval* a) {
  /* Type: function */
  /* Format: init <qexpr> */
  /* Description: return <qexpr> without its last element */
  /* Example: init '(1 2 3 4) ==> '(1 2 3) */

  LASSERT_NELIST("init", a);
  LASSERT_NARGS("init", a, 1);
  LASSERT_TYPE("init", a, 0, LVAL_QEXPR);

  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, v->count-1));

  return v;
}

lval* builtin_list(lenv* e, lval* a) {
  /* Type: function */
  /* Format: list & <expr>* */
  /* Description: return <expr>* as a list */
  /* Example: list 1 2 3 4 ==> '(1 2 3 4) */

  a->type = LVAL_QEXPR;
  return a;
}

lval* builtin_eval(lenv* e, lval* a) {
  /* Type: function */
  /* Format: eval <qexpr> */
  /* Description: return the result of evaluating <qexpr> */
  /* Example: eval '(+ 2 3) ==> 5 */

  LASSERT_NARGS("eval", a, 1);

  lval* x = lval_take(a, 0);

  /* If x is a Q-Expression, make it evaluable */
  if (x->type == LVAL_QEXPR) { x->type = LVAL_SEXPR; }

  return lval_eval(e, x);
}

lval* builtin_join(lenv* e, lval* a) {

  for (int i = 0; i < a->count; i++) {
    LASSERT_TYPE("join", a, i, LVAL_QEXPR);
  }

  lval* x = lval_pop(a, 0);

  while (a->count) {
    x = lval_join(x, lval_pop(a, 0));
  }

  lval_del(a);
  return x;
}

lval* builtin_cons(lenv* e, lval* a) {
  LASSERT_NARGS("cons", a, 2);
  LASSERT_TYPE("cons", a, 1, LVAL_QEXPR);

  /* Take the first element and turn it into a qexpr */
  lval* v = lval_add(lval_qexpr(), lval_pop(a, 0));

  /* Join it with the second (now first) element in a */
  v = lval_join(v, lval_pop(a, 0));

  lval_del(a);
  return v;
}

lval* builtin_len(lenv* e, lval* a) {
  LASSERT_NARGS("len", a, 1);
  LASSERT_TYPE("len", a, 0, LVAL_QEXPR);

  /* The length is simply the number of children */
  lval* l = lval_num((double)a->cell[0]->count);

  lval_del(a);
  return l;
}

lval* builtin_nth(lenv* e, lval* a) {
  /* Type: function */
  /* Format: nth <n> <qexpr> */
  /* Description: return nth element of <qexpr>; zero-based */
  /* If n<0, count from end of list */
  /* nth 2 '(1 2 3 4) ==> 3 */
  /* nth -2 '(1 2 3 4) ==> 3 */

  LASSERT_NARGS("nth", a, 2);
  LASSERT_TYPE("nth", a, 0, LVAL_NUM);
  LASSERT_TYPE("nth", a, 1, LVAL_QEXPR);

  int n = a->cell[0]->num;
  if (n < 0) { n += a->cell[1]->count; }
  LASSERT(a, (0 <= n) && (n < a->cell[1]->count), "nth: argument out of bounds");

  lval* v = lval_take(a, 1);
  lval* r = lval_eval(e, lval_take(v, n));
  return r;
}

lval* builtin_set(lenv* e, lval* a) {
  LASSERT_TYPE("set", a, 0, LVAL_QEXPR);

  /* First argument must be a symbol list */
  lval* syms = a->cell[0];
  for (int i = 0; i < syms->count; i++) {
    LASSERT(a, (syms->cell[i]->type == LVAL_SYM), "set: cannot define non-symbol");
  }

  /* Check correct number of symbols and values */
  LASSERT(a, (syms->count == a->count-1), "set: incorrect number of values to symbols");

  for (int i = 0; i < syms->count; i++) {
    lenv_put(e, syms->cell[i], a->cell[i+1], NULL);
  }

  lval_del(a);
  return lval_sexpr();
}

lval* builtin_def(lenv* e, lval* a) {
  LASSERT_VAR_NARGS("def", a, 2, 3);
  LASSERT_TYPE("def", a, 0, LVAL_QEXPR);

  char* doc = NULL;
  lval* sym = a->cell[0];

  /* First argument must be a list with one symbol */
  if (sym->count != 1) { lval_del(a); return lval_err("def: wrong number of symbols"); }
  if (sym->cell[0]->type != LVAL_SYM) { lval_del(a); return lval_err("def: cannot define non-symbol"); }

  /* Test the third argument */
  /* If it's a boolean, we ignore it; that allows us to  */
  if (a->count == 3 && a->cell[2]->type != LVAL_BOOL) {
    LASSERT(a, (a->cell[2]->type == LVAL_STR), "def: documentation is not of type string. Got %s instead", ltype_name(a->cell[2]->type));
    doc = a->cell[2]->str;
  }

  /* Put the definition in the global environment */
  lenv_def(e, sym->cell[0], a->cell[1], doc);

  lval_del(a);
  return lval_sexpr();
}

lval* builtin_lambda(lenv* e, lval* a) {
  LASSERT_NARGS("\\", a, 2);
  LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
  LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);

  /* Check if first Q-Expr contains only symbols */
  for (int i = 0; i < a->cell[0]->count; i++) {
    LASSERT(a, (a->cell[0]->cell[i]->type == LVAL_SYM),
            "Cannot define %s", ltype_name(a->cell[0]->cell[i]->type));
  }

  /* Pop both arguments and pass them to lval_lambda */
  lval* formals = lval_pop(a, 0);
  lval* body = lval_pop(a, 0);
  lval_del(a);

  return lval_lambda(formals, body);
}

lval* builtin_macro(lenv* e, lval* a) {
  LASSERT_NARGS("\\", a, 2);
  LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
  LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);

  /* Check if first Q-Expr contains only symbols */
  for (int i = 0; i < a->cell[0]->count; i++) {
    LASSERT(a, (a->cell[0]->cell[i]->type == LVAL_SYM),
            "Cannot define %s", ltype_name(a->cell[0]->cell[i]->type));
  }

  /* Pop both arguments and pass them to lval_macro */
  lval* formals = lval_pop(a, 0);
  lval* body = lval_pop(a, 0);
  lval_del(a);

  return lval_macro(formals, body);
}

lval* builtin_load(lenv* e, lval* a) {
  LASSERT_NARGS("load", a, 1);
  LASSERT_TYPE("load", a, 0, LVAL_STR);
lval* builtin_quote(lenv* e, lval* a) {
  LASSERT_NARGS("`", a, 1);

  lval* v = lval_take(a, 0);
  switch (v->type) {
  case LVAL_QEXPR: break;
  case LVAL_SEXPR: v->type = LVAL_QEXPR; break;
  default: v = lval_add(lval_qexpr(), v);
  }

  return v;
}


  /* Parse file given by string name */
  mpc_result_t r;
  if (mpc_parse_contents(a->cell[0]->str, Lambda, &r)) {

    /* Read contents */
    lval* expr = lval_read(r.output);
    mpc_ast_delete(r.output);

    /* Evaluate each expression */
    while(expr->count) {
      lval* x = lval_eval(e, lval_pop(expr, 0));
      /* If evaluation leads to error, print it */
      if (x->type == LVAL_ERR) { lval_println(x); }
      lval_del(x);
    }

    /* Delete expression and arguments */
    lval_del(expr);
    lval_del(a);

    /* Return empty list */
    return lval_sexpr();

  } else {
    /* Get parse error as string */
    char* err_msg = mpc_err_string(r.error);
    mpc_err_delete(r.error);

    /* Create new error message using it */
    lval* err = lval_err("Could not load library %s", err_msg);

    /* Cleanup and return error */
    free(err_msg);
    lval_del(a);
    return err;
  }
}

lval* builtin_puts(lenv* e, lval* a) {
  /* Print a string */

  LASSERT_NARGS("puts", a, 1);
  LASSERT_TYPE("puts", a, 0, LVAL_STR);

  puts(a->cell[0]->str);

  lval_del(a);
  return lval_sexpr();
}

lval* builtin_print(lenv* e, lval* a) {

  /* Print each argument followed by a space */
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->type == LVAL_FUN || a->cell[i]->type == LVAL_MAC) { lval_print_fn(a->cell[i]); }
    else { lval_print(a->cell[i]); }
    putchar(' ');
  }

  /* Print a newline and delete arguments */
  putchar('\n');
  lval_del(a);

  return lval_sexpr();
}

lval* builtin_read(lenv* e, lval* a) {
  /* Convert a string into a Q-expression */
  /* read <string> */

  LASSERT_NARGS("read", a, 1);
  LASSERT_TYPE("read", a, 0, LVAL_STR);

  lval* s = lval_take(a, 0);

  mpc_result_t r;
  lval* v;

  /* Try to parse the input as code and set type to LVAL_QEXPR if successful*/
  if (mpc_parse("<stdin>", s->str, Lambda, &r)) {
    v = lval_read(r.output);
    v->type = LVAL_QEXPR;
    mpc_ast_delete(r.output);
  } else {
    /* Otherwise display the error and return '() */
    mpc_err_print(r.error);
    mpc_err_delete(r.error);
    v = lval_qexpr();
  }

  lval_del(s);
  return v;
}

lval* builtin_input(lenv* e, lval* a) {
  /* Read a string and return it */
  /* readstr <prompt> */

  LASSERT_NARGS("input", a, 1);
  LASSERT_TYPE("input", a, 0, LVAL_STR);

  char* input = readline(a->cell[0]->str);

  lval_del(a);
  return lval_str(input);
}

lval* builtin_error(lenv* e, lval* a) {
  LASSERT_NARGS("error", a, 1);
  LASSERT_TYPE("error", a, 0, LVAL_STR);

  /* Construct error from first argument */
  lval* err = lval_err(a->cell[0]->str);

  lval_del(a);
  return err;
}

lval* builtin_typeof(lenv* e, lval* a) {
  /* Print the type of a */

  LASSERT_NARGS("typeof", a, 1);

  lval* v = lval_str(ltype_name(a->cell[0]->type));
  lval_del(a);
  return v;
}

lval* builtin_doc(lenv* e, lval* a) {
  /* Return the doc string of a */

  LASSERT_TYPE("doc", a, 1, LVAL_QEXPR);
  LASSERT(a, (a->cell[0]->cell[0]->type == LVAL_SYM), "doc: type %s has no doc string", ltype_name(a->cell[0]->type));

  lval* v = lenv_doc(e, a->cell[0]->cell[0]);

  lval_del(a);
  return v;
}

lval* builtin_exit(lenv* e, lval* a) {
  /* If there is no argument, exit unconditionally */
  /* Note, it's currently not possible to have a function without arguments! */
  if (a->count == 0) { exit(0); }

  /* Otherwise see if there is a number and use it as the exit code*/
  LASSERT_TYPE("exit", a, 0, LVAL_NUM);
  exit((int)a->cell[0]->num);
}

lval* builtin_add(lenv* e, lval* a) { return builtin_op(e, a, "+"); }
lval* builtin_sub(lenv* e, lval* a) { return builtin_op(e, a, "-"); }
lval* builtin_mul(lenv* e, lval* a) { return builtin_op(e, a, "*"); }
lval* builtin_div(lenv* e, lval* a) { return builtin_op(e, a, "/"); }
lval* builtin_pow(lenv* e, lval* a) { return builtin_op(e, a, "pow"); }
lval* builtin_mod(lenv* e, lval* a) { return builtin_op(e, a, "mod"); }

lval* builtin_eq(lenv* e, lval* a) { return builtin_comp(e, a, "="); }
lval* builtin_nq(lenv* e, lval* a) { return builtin_comp(e, a, "/="); }
lval* builtin_lt(lenv* e, lval* a) { return builtin_comp(e, a, "<"); }
lval* builtin_gt(lenv* e, lval* a) { return builtin_comp(e, a, ">"); }
lval* builtin_le(lenv* e, lval* a) { return builtin_comp(e, a, "<="); }
lval* builtin_ge(lenv* e, lval* a) { return builtin_comp(e, a, ">="); }

void lenv_add_builtin(lenv* e, char* name, lbuiltin func, char* doc) {
  lval* k = lval_sym(name);
  lval* v = lval_fun(func, LVAL_FUN);
  lenv_put(e, k, v, doc);
  lval_del(k); lval_del(v);
}

void lenv_add_builtin_mac(lenv* e, char* name, lbuiltin mac, char* doc) {
  lval* k = lval_sym(name);
  lval* v = lval_fun(mac, LVAL_MAC);
  lenv_put(e, k, v, doc);
  lval_del(k); lval_del(v);
}

void lenv_add_builtins(lenv* e) {

  /* List functions */
  lenv_add_builtin(e, "list", builtin_list, "Construct a list.");
  lenv_add_builtin(e, "head", builtin_head, NULL);
  lenv_add_builtin(e, "tail", builtin_tail, NULL);
  lenv_add_builtin(e, "last", builtin_last, NULL);
  lenv_add_builtin(e, "eval", builtin_eval, NULL);
  lenv_add_builtin(e, "join", builtin_join, NULL);
  lenv_add_builtin(e, "init", builtin_init, NULL);
  lenv_add_builtin(e, "cons", builtin_cons, NULL);
  lenv_add_builtin(e, "len", builtin_len, NULL);
  lenv_add_builtin(e, "nth", builtin_nth, NULL);

  /* Variable functions */
  lenv_add_builtin(e, "def", builtin_def, NULL);
  lenv_add_builtin(e, "set", builtin_set, NULL);
  lenv_add_builtin_mac(e, "doc", builtin_doc, NULL);

  /* General functions */
  lenv_add_builtin(e, "\\", builtin_lambda, NULL);
  lenv_add_builtin(e, "^", builtin_macro, NULL);
  lenv_add_builtin(e, "exit", builtin_exit, NULL);
  lenv_add_builtin(e, "typeof", builtin_typeof, NULL);
  lenv_add_builtin(e, "load", builtin_load, NULL);
  lenv_add_builtin(e, "`", builtin_quote, NULL);
  lenv_add_builtin(e, "print", builtin_print, NULL);
  lenv_add_builtin(e, "error", builtin_error, NULL);
  lenv_add_builtin(e, "puts", builtin_puts, NULL);
  lenv_add_builtin(e, "read", builtin_read, NULL);
  lenv_add_builtin(e, "input", builtin_input, NULL);

  /* Mathematical functions */
  lenv_add_builtin(e, "+", builtin_add, NULL);
  lenv_add_builtin(e, "-", builtin_sub, NULL);
  lenv_add_builtin(e, "*", builtin_mul, NULL);
  lenv_add_builtin(e, "/", builtin_div, NULL);
  lenv_add_builtin(e, "pow", builtin_pow, NULL);
  lenv_add_builtin(e, "mod", builtin_mod, NULL);

  /* Comparison functions */
  lenv_add_builtin_mac(e, "if", builtin_if, NULL);
  lenv_add_builtin(e, "=", builtin_eq, NULL);
  lenv_add_builtin(e, "/=", builtin_nq, NULL);
  lenv_add_builtin(e, "<", builtin_lt, NULL);
  lenv_add_builtin(e, ">", builtin_gt, NULL);
  lenv_add_builtin(e, "<=", builtin_le, NULL);
  lenv_add_builtin(e, ">=", builtin_ge, NULL);
  lenv_add_builtin(e, "equal", builtin_equal, NULL);
  lenv_add_builtin(e, "null", builtin_null, NULL);

  /* Logical functions */
  lenv_add_builtin_mac(e, "and", builtin_and, NULL);
  lenv_add_builtin_mac(e, "or", builtin_or, NULL);
  lenv_add_builtin(e, "not", builtin_not, NULL);
}

lval* lval_call(lenv* e, lval* f, lval* a) {

  /* If builtin then simply apply that */
  if (f->builtin) { return f->builtin(e, a); }

  /* Record argument counts */
  int given = a->count;
  int total = f->formals->count;

  /* While arguments still remain to be processed */
  while (a->count) {

    /* If we ran out of formal arguments to bind */
    if (f->formals->count == 0) {
      lval_del(a); return lval_err("Too many arguments. Got %i, expected %i", given, total);
    }

    /* Pop the first symbol from the formals */
    lval* sym = lval_pop(f->formals, 0);

    /* Special case to deal with & */
    if (strcmp(sym->sym, "&") == 0) {

      /* Ensure & is followed by another symbol */
      if (f->formals->count != 1) {
        lval_del(a);
        return lval_err("Invalid function format. Incorrect number of symbols after '&'");
      }

      /* Next formal should be bound to remaining arguments */
      lval* nsym = lval_pop(f->formals, 0);
      lenv_put(f->env, nsym, builtin_list(e, a), NULL);
      lval_del(sym); lval_del(nsym);
      break;
    }

    /* Pop the next argument from the list */
    lval* val = lval_pop(a, 0);

    /* Bind a copy into the function's environment */
    lenv_put(f->env, sym, val, NULL);

    /* Delete symbol and value */
    lval_del(sym); lval_del(val);
  }

  /* Argument list is now bound so can be cleaned up */
  lval_del(a);

  /* If & remains in formal list it should be bound to the empty list */
  if (f->formals->count > 0 && strcmp(f->formals->cell[0]->sym, "&") == 0) {

    /* Check to ensure that & is not passed invalidly */
    if (f->formals->count != 2) {
      return lval_err("Invalid function format. Incorrect number of symbols after '&'");
    }

    /* Pop and delete & symbol */
    lval_del(lval_pop(f->formals, 0));

    /* Pop next symbol and create empty list */
    lval* sym = lval_pop(f->formals, 0);
    lval* val = lval_qexpr();

    /* Bind to environment and delete */
    lenv_put(f->env, sym, val, NULL);
    lval_del(sym); lval_del(val);
  }

  /* If all formals have been bound, evaluate */
  if (f->formals->count == 0) {

    /* Set function environment parent to current evaluation environment */
    f->env->par = e;

    /* Evaluate and return */
    return builtin_eval(f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
  } else {

    /* Otherwise return partially evaluated copy */
    return lval_copy(f);
  }
}

lval* lval_eval_sexpr(lenv* e, lval* v) {

  /* Empty expressions */
  if (v->count == 0) { return v; }

  /* Evaluate first child */
  v->cell[0] = lval_eval(e, v->cell[0]);
  if (v->cell[0]->type == LVAL_ERR) { return lval_take(v, 0); }

  /* Check if we're dealing with a macro */
  if (v->cell[0]->type == LVAL_MAC) {
    /* Convert all arguments into Q-expressions */
    for (int i = 1; i < v->count; i++) {
      switch (v->cell[i]->type) {
      case LVAL_QEXPR: break;
      case LVAL_SEXPR: v->cell[i]->type = LVAL_QEXPR; break;
        /* If we find a !, replace it with the result of evaluation of the next argument */
      case LVAL_SYM: if (strcmp(v->cell[i]->sym, "!") == 0) {
          if (i == v->count-1) { return lval_err("Invalid use of !"); }
          v->cell[i] = lval_eval(e, lval_pop(v, i+1));
          break;
        }
      default: v->cell[i] = lval_add(lval_qexpr(), v->cell[i]);
      }
    }
  } else {
    /* Evaluate other children */
    for (int i = 1; i < v->count; i++) {
      v->cell[i] = lval_eval(e, v->cell[i]);
      if (v->cell[i]->type == LVAL_ERR) { return lval_take(v, i); }
    }
  }

  /* Single expression */
  if (v->count == 1) { return lval_take(v, 0); }

  /* See if first element is a function after evaluation */
  lval* f = lval_pop(v, 0);
  if (f->type != LVAL_FUN && f->type != LVAL_MAC) {
    lval* err = lval_err("Not a function. Got type %s instead", ltype_name(f->type));
    lval_del(f); lval_del(v);
    return err;
  }

  lval* result = lval_call(e, f, v);
  lval_del(f);

  return result;
}

lval* lval_eval_qexpr(lenv* e, lval* v) {
  if (v->type == LVAL_QEXPR) { v->type = LVAL_SEXPR; }
  return lval_eval(e, v);
}

lval* lval_eval(lenv* e, lval* v) {
#ifdef DEBUG
  fprintf(stderr, "[%3i] %*s%s", eval_level, eval_level, "", "Evaluating: "); lval_fprintln(stderr, v);
  eval_level++;
#endif

  /* Symbols an Sexpressions are treated separately */
  if (v->type == LVAL_SYM) {
    lval* result = lenv_get(e, v);
    lval_del(v);

#ifdef DEBUG
    eval_level--;
    fprintf(stderr, "[%3i] %*s%s", eval_level, eval_level, "", "Returning: "); lval_fprintln(stderr, result);
#endif

    return result;
  }

  if (v->type == LVAL_SEXPR) {
    lval* result = lval_eval_sexpr(e, v);

#ifdef DEBUG
    eval_level--;
    fprintf(stderr, "[%3i] %*s%s", eval_level, eval_level, "", "Returning: "); lval_fprintln(stderr, result);
#endif

    return result; }

  /* All other lval types remain the same */

#ifdef DEBUG
  eval_level--;
  fprintf(stderr, "[%3i] %*s%s", eval_level, eval_level, "", "Returning: "); lval_fprintln(stderr, v);
#endif

  return v;
}

/* Main program */

int main(int argc, char** argv) {

  /* Create some parsers */
  Number  = mpc_new("number");
  Boolean = mpc_new("boolean");
  Symbol  = mpc_new("symbol");
  String  = mpc_new("string");
  Comment = mpc_new("comment");
  Sexpr   = mpc_new("sexpr");
  Qexpr   = mpc_new("qexpr");
  Expr    = mpc_new("expr");
  Lambda  = mpc_new("lambda");

  /* Define them with the following language */
  mpca_lang(MPCA_LANG_DEFAULT,
    "                                                                   \
      number  : /-?[0-9]+\\.?[0-9]*/ ;                                  \
      boolean : \"true\" | \"false\" ;                                  \
      symbol  : /[a-zA-Z0-9_+\\`\\-*\\/\\\\=<>&%$^]+/ | '!' ;           \
      string  : /\"(\\\\.|[^\"])*\"/ ;                                  \
      comment : /;[^\\r\\n]*/ ;                                         \
      sexpr   : '(' <expr>* ')' ;                                       \
      qexpr   : '\'' '(' <expr>* ')' ;                                  \
      expr    : <number> | <boolean> | <symbol> | <string> | <comment>  \
              | <sexpr> | <qexpr> ;                                     \
      lambda  : /^/ <expr>+ /$/ ;                                       \
    ",
            Number, Boolean, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lambda);

  /* Create and set up an environment */
  lenv* e = lenv_new();
  lenv_add_builtins(e);

  /* Interactive prompt */
  if (argc == 1) {

    puts("Lambda Version 0.1");

    /* Load standard prelude if it exists */
    if( access( "prelude.l", R_OK ) != -1 ) {
      /* Create an argument list with a single argument being the filename */
      lval* args = lval_add(lval_sexpr(), lval_str("prelude.l"));
      lval* x = builtin_load(e, args);
      /* If the result is an error, print it */
      if (x->type == LVAL_ERR) { puts("Error loading prelude"); lval_println(x); }
      else { puts("Loaded prelude"); }
      lval_del(x);
    } else { puts("Warning: Could not load prelude"); }

    putchar('\n');

    while (1) {

      char* input = readline("lambda> ");
      add_history(input);

#ifdef DEBUG
      fprintf(stderr, "Input: %s\n", input);
#endif

      /* Attempt to parse the user input */
      mpc_result_t r;
      if (mpc_parse("<stdin>", input, Lambda, &r)) {

        /* Print the AST if needed */
        /* mpc_ast_print(r.output); */

        /* Evaluate the AST and print the result */
        lval* result = lval_eval(e, lval_read(r.output));
        printf(">> ");
        lval_println(result);
        lval_del(result);
        putchar('\n');

        mpc_ast_delete(r.output);
      } else {
        /* Otherwise print the error */
        mpc_err_print(r.error);
        mpc_err_delete(r.error);
      }

      free(input);
    }
  }

  /* Supplied with list of files */
  if (argc >= 2) {

    /* Loop over each supplied fielname */
    for (int i = 1; i < argc; i++) {

      /* Create an argument list with a single argument being the filename */
      lval* args = lval_add(lval_sexpr(), lval_str(argv[i]));
      lval* x = builtin_load(e, args);

      /* If the result is an error, print it */
      if (x->type == LVAL_ERR) { lval_println(x); }
      lval_del(x);
    }
  }

  /* Delete the environment */
  lenv_del(e);

  /* Undefine and delete our parsers */
  mpc_cleanup(9, Number, Boolean, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lambda);

  return 0;
}
