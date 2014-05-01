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
    lval* err = lval_err("%s: Wrong type argument. Argument %i is of type %s, expected %s", fn, n, ltype_name(args->cell[n]->type), ltype_name(expt)); \
    lval_del(args);                                                     \
    return err;                                                         \
  }

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
typedef enum { LVAL_ERR, LVAL_NUM, LVAL_BOOL, LVAL_SYM, LVAL_STR, LVAL_FUN, LVAL_SEXPR, LVAL_QEXPR } lval_t;

char* ltype_name(int t) {
  switch(t) {
  case LVAL_FUN: return "Function";
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

lval* lval_fun(lbuiltin func) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
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
  return errno != ERANGE ? lval_num(x) : lval_err("invalid number");
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

/* Print lval */
void lval_print(lval* v);

void lval_print_fn(lval* v) {
  /* Print the code of a user function */
  /* This is for use in builtin_print */
  if (v->builtin) {
    printf("<builtin function at %p>", (void*)v);
  } else {
    printf("(\\ "); lval_print(v->formals); putchar(' '); lval_print(v->body); putchar(')');
  }
}

void lval_print_expr(lval* v, lval_t type) {
  if (type == LVAL_QEXPR) { putchar('\''); }

  putchar('(');

  for (int i = 0; i < v->count; i++) {

    /* Print Value contained within */
    lval_print(v->cell[i]);

    /* Don't print trailing space if last element */
    if (i != (v->count-1)) {
      putchar(' ');
    }
  }
  putchar(')');
}

void lval_print_str(lval* v) {
  /* Make a copy of the string */
  char* escaped = malloc(strlen(v->str) + 1);
  strcpy(escaped, v->str);
  /* Pass it through the escape function */
  escaped = mpcf_escape(escaped);
  /* Print it between " characters */
  printf("\"%s\"", escaped);
  /* free the copied string */
  free(escaped);
}

void lval_print(lval* v) {
  switch (v->type) {
  case LVAL_NUM: printf("%g", v->num); break;
  case LVAL_ERR: printf("Error: %s", v->err); break;
  case LVAL_BOOL: printf("%s", v->boolean ? "true" : "false"); break;
  case LVAL_SYM: printf("%s", v->sym); break;
  case LVAL_STR: lval_print_str(v); break;
  case LVAL_FUN:
    if (v->builtin) {
      printf("<builtin function at %p>", (void*)v);
    } else {
      printf("<user defined function at %p>", (void*)v);
    }
    break;
  case LVAL_SEXPR: lval_print_expr(v, LVAL_SEXPR); break;
  case LVAL_QEXPR: lval_print_expr(v, LVAL_QEXPR); break;
  }
}

void lval_println(lval* v) { lval_print(v); putchar('\n'); }

/* Environment struct */
struct lenv {
  lenv* par;
  int count;
  char** syms;
  lval** vals;
};

lenv* lenv_new(void) {
  lenv* e = malloc(sizeof(lval));
  e->par = NULL;
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}

void lenv_del(lenv* e) {
  for (int i = 0; i < e->count; i++) {
    free(e->syms[i]);
    lval_del(e->vals[i]);
  }
  free(e->syms);
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

void lenv_put(lenv* e, lval* k, lval* v) {

  /* Iterate over all items in environment */
  /* This is to see if variable already exists */
  for (int i = 0; i < e->count; i++) {
    /* If variable is found delete item at that position */
    /* And replace with variable supplied by user */
    if (strcmp(e->syms[i], k->sym) == 0) {
      lval_del(e->vals[i]);
      e->vals[i] = lval_copy(v);
      return;
    }
  }

  /* If no existing entry found then allocate space for new entry */
  e->count++;
  e->vals = realloc(e->vals, sizeof(lval*) * e->count);
  e->syms = realloc(e->syms, sizeof(char*) * e->count);

  /* Copy contents of lval and symbol string into new location */
  e->vals[e->count-1] = lval_copy(v);
  e->syms[e->count-1] = malloc(strlen(k->sym)+1);
  strcpy(e->syms[e->count-1], k->sym);
}

void lenv_def(lenv* e, lval* k, lval* v) {
  /* Find top environment and add (k, v) there */
  while (e->par) { e = e->par; }
  lenv_put(e, k, v);
}

lenv* lenv_copy(lenv* e) {
  lenv* n = malloc(sizeof(lenv));
  n->par = e->par;
  n->count = e->count;
  n->syms = malloc(sizeof(char*) * n->count);
  n->vals = malloc(sizeof(lval*) * n->count);
  for (int i = 0; i < e->count; i++) {
    n->syms[i] = malloc(strlen(e->syms[i]) + 1);
    strcpy(n->syms[i], e->syms[i]);
    n->vals[i] = lval_copy(e->vals[i]);
  }
  return n;
}

/* Evaluation */

lval* lval_eval(lenv* e, lval* v);

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
    if (strcmp(op, "^") == 0) { x->num = pow(x->num, y->num); }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        lval_del(x); lval_del(y);
        x = lval_err("Division by zero"); break;
      }
      x->num /= y->num;
    }
    if (strcmp(op, "%") == 0) {
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

lval* builtin_if(lenv* e, lval* a) {
  LASSERT_VAR_NARGS("if", a, 2, 3);
  LASSERT_TYPE("if", a, 0, LVAL_BOOL);

  /* Mark both expressions as evaluable if they are Q-Expressions */
  if (a->cell[1]->type == LVAL_QEXPR) { a->cell[1]->type = LVAL_SEXPR; }
  if (a->count == 3 && a->cell[2]->type == LVAL_QEXPR) { a->cell[2]->type = LVAL_SEXPR; }

  lval *r;

  if (a->cell[0]->boolean) {
    r = lval_eval(e, lval_pop(a, 1));
  } else {
    if (a->count == 3) {
      r = lval_eval(e, lval_pop(a, 2));
    } else {
      r = lval_bool(false);
    }
  }

  lval_del(a);
  return r;
}

lval* builtin_and(lenv* e, lval* a) {
  /* Error checking is done in a separate loop, so that we check *all* argument types */
  for (int i = 0; i < a->count; i++) { LASSERT_TYPE("and", a, i, LVAL_BOOL); }

  /* Now check each argument again and return if we find a false */
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->boolean == false) { lval_del(a); return lval_bool(false); }
  }

  /* Otherwise return true */
  lval_del(a);
  return lval_bool(true);
}

lval* builtin_or(lenv* e, lval* a) {
  for (int i = 0; i < a->count; i++) { LASSERT_TYPE("or", a, i, LVAL_BOOL); }

  /* Now check each argument again and return if we find a true */
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->boolean == true) { lval_del(a); return lval_bool(true); }
  }

  /* Otherwise return false */
  lval_del(a);
  return lval_bool(false);
}

lval* builtin_not(lenv* e, lval* a) {
  LASSERT_NARGS("not", a, 1);
  LASSERT_TYPE("not", a, 0, LVAL_BOOL);
  lval* r = lval_take(a, 0);
  r->boolean = !r->boolean;
  return r;
}

lval* builtin_head(lenv* e, lval* a) {
  LASSERT_NELIST("head", a);
  LASSERT_NARGS("head", a, 1);
  LASSERT_TYPE("head", a, 0, LVAL_QEXPR);

  lval* v = lval_take(a, 0);
  while (v->count > 1) { lval_del(lval_pop(v, 1)); }
  return v;
}

lval* builtin_tail(lenv* e, lval* a) {
  LASSERT_NELIST("tail", a);
  LASSERT_NARGS("tail", a, 1);
  LASSERT_TYPE("tail", a, 0, LVAL_QEXPR);

  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, 0));
  return v;
}

lval* builtin_init(lenv* e, lval* a) {
  LASSERT_NELIST("init", a);
  LASSERT_NARGS("init", a, 1);
  LASSERT_TYPE("init", a, 0, LVAL_QEXPR);

  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, v->count-1));

  lval_del(a);
  return v;
}

lval* builtin_list(lenv* e, lval* a) {
  a->type = LVAL_QEXPR;
  return a;
}

lval* builtin_eval(lenv* e, lval* a) {
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
  LASSERT_TYPE("cons", a, 0, LVAL_QEXPR);

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

lval* builtin_var(lenv* e, lval* a, char* func) {
  LASSERT_TYPE(func, a, 0, LVAL_QEXPR);

  /* First argument must be a symbol list */
  lval* syms = a->cell[0];
  for (int i = 0; i < syms->count; i++) {
    LASSERT(a, (syms->cell[i]->type == LVAL_SYM), "def: Cannot define non-symbol");
  }

  /* Check correct number of symbols and values */
  LASSERT(a, (syms->count == a->count-1), "def: incorrect number of values to symbols");

  for (int i = 0; i < syms->count; i++) {
    if (strcmp(func, "def") == 0) { lenv_def(e, syms->cell[i], a->cell[i+1]); }
    if (strcmp(func, "set") == 0) { lenv_put(e, syms->cell[i], a->cell[i+1]); }
  }

  lval_del(a);
  return lval_sexpr();
}

lval* builtin_def(lenv* e, lval* a) { return builtin_var(e, a, "def"); }
lval* builtin_put(lenv* e, lval* a) { return builtin_var(e, a, "set"); }

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

lval* builtin_load(lenv* e, lval* a) {
  LASSERT_NARGS("load", a, 1);
  LASSERT_TYPE("load", a, 0, LVAL_STR);

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
    if (a->cell[i]->type == LVAL_FUN) { lval_print_fn(a->cell[i]); }
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
lval* builtin_pow(lenv* e, lval* a) { return builtin_op(e, a, "^"); }
lval* builtin_mod(lenv* e, lval* a) { return builtin_op(e, a, "%"); }

lval* builtin_eq(lenv* e, lval* a) { return builtin_comp(e, a, "="); }
lval* builtin_nq(lenv* e, lval* a) { return builtin_comp(e, a, "/="); }
lval* builtin_lt(lenv* e, lval* a) { return builtin_comp(e, a, "<"); }
lval* builtin_gt(lenv* e, lval* a) { return builtin_comp(e, a, ">"); }
lval* builtin_le(lenv* e, lval* a) { return builtin_comp(e, a, "<="); }
lval* builtin_ge(lenv* e, lval* a) { return builtin_comp(e, a, ">="); }

void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
  lval* k = lval_sym(name);
  lval* v = lval_fun(func);
  lenv_put(e, k, v);
  lval_del(k); lval_del(v);
}

void lenv_add_builtins(lenv* e) {

  /* List functions */
  lenv_add_builtin(e, "list", builtin_list);
  lenv_add_builtin(e, "head", builtin_head);
  lenv_add_builtin(e, "tail", builtin_tail);
  lenv_add_builtin(e, "eval", builtin_eval);
  lenv_add_builtin(e, "join", builtin_join);
  lenv_add_builtin(e, "init", builtin_init);
  lenv_add_builtin(e, "cons", builtin_cons);
  lenv_add_builtin(e, "len", builtin_len);

  /* Variable functions */
  lenv_add_builtin(e, "def", builtin_def);
  lenv_add_builtin(e, "set", builtin_put);

  /* General functions */
  lenv_add_builtin(e, "\\", builtin_lambda);
  lenv_add_builtin(e, "exit", builtin_exit);
  lenv_add_builtin(e, "typeof", builtin_typeof);
  lenv_add_builtin(e, "load", builtin_load);
  lenv_add_builtin(e, "print", builtin_print);
  lenv_add_builtin(e, "error", builtin_error);
  lenv_add_builtin(e, "puts", builtin_puts);
  lenv_add_builtin(e, "read", builtin_read);
  lenv_add_builtin(e, "input", builtin_input);

  /* Mathematical functions */
  lenv_add_builtin(e, "+", builtin_add);
  lenv_add_builtin(e, "-", builtin_sub);
  lenv_add_builtin(e, "*", builtin_mul);
  lenv_add_builtin(e, "/", builtin_div);
  lenv_add_builtin(e, "^", builtin_pow);
  lenv_add_builtin(e, "%", builtin_mod);

  /* Comparison functions */
  lenv_add_builtin(e, "if", builtin_if);
  lenv_add_builtin(e, "=", builtin_eq);
  lenv_add_builtin(e, "/=", builtin_nq);
  lenv_add_builtin(e, "<", builtin_lt);
  lenv_add_builtin(e, ">", builtin_gt);
  lenv_add_builtin(e, "<=", builtin_le);
  lenv_add_builtin(e, ">=", builtin_ge);
  lenv_add_builtin(e, "equal", builtin_equal);

  /* Logical functions */
  lenv_add_builtin(e, "and", builtin_and);
  lenv_add_builtin(e, "or", builtin_or);
  lenv_add_builtin(e, "not", builtin_not);
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
      lenv_put(f->env, nsym, builtin_list(e, a));
      lval_del(sym); lval_del(nsym);
      break;
    }

    /* Pop the next argument from the list */
    lval* val = lval_pop(a, 0);

    /* Bind a copy into the function's environment */
    lenv_put(f->env, sym, val);

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
    lenv_put(f->env, sym, val);
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

  /* Evaluate children */
  for (int i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(e, v->cell[i]);
  }

  /* Error checking */
  for (int i = 0; i < v->count; i++) {
    if (v->cell[i]->type == LVAL_ERR) { return lval_take(v, i); }
  }

  /* Empty expressions */
  if (v->count == 0) { return v; }

  /* Single expression */
  if (v->count == 1) { return lval_eval(e, lval_take(v, 0)); }

  /* See if first element is a function after evaluation */
  lval* f = lval_pop(v, 0);
  if (f->type != LVAL_FUN) {
    lval* err = lval_err("Not a function. Got type %s instead", ltype_name(f->type));
    lval_del(f); lval_del(v);
    return err;
  }

  lval* result = lval_call(e, f, v);
  lval_del(f);
  return result;
}

lval* lval_eval(lenv* e, lval* v) {
  /* Symbols an Sexpressions are treated separately */
  if (v->type == LVAL_SYM) {
    lval* x = lenv_get(e, v);
    lval_del(v);
    return x;
  }

  if (v->type == LVAL_SEXPR) { return lval_eval_sexpr(e, v); }

  /* All other lval types remain the same */
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
  mpca_lang(MPC_LANG_DEFAULT,
    "                                                                   \
      number  : /-?[0-9]+\\.?[0-9]*/ ;                                  \
      boolean : \"true\" | \"false\" ;                                  \
      symbol  : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&%^]+/ ;                    \
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

    while (1) {

      char* input = readline("lambda> ");
      add_history(input);

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
