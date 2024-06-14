;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

;; Prettyprinter and tree editor

(defparameter *prettyprint* 
  '(
    #"
// Prettyprint"#

#-(or badge gfx)
#"
const int PPINDENT = 2;
const int PPWIDTH = 80;"#

#+badge
#"
const int PPINDENT = 2;
const int PPWIDTH = 42;"#

#+gfx
#"
const int PPINDENT = 2;
const int PPWIDTH = 80;
const int GFXPPWIDTH = 52; // 320 pixel wide screen
int ppwidth = PPWIDTH;"#

#"
void pcount (char c) {
  if (c == '\n') PrintCount++;
  PrintCount++;
}

/*
  atomwidth - calculates the character width of an atom
*/
uint8_t atomwidth (object *obj) {
  PrintCount = 0;
  printobject(obj, pcount);
  return PrintCount;
}

/*
  basewidth - calculates the character width of an integer printed in a given base
*/
uint8_t basewidth (object *obj, uint8_t base) {
  PrintCount = 0;
  pintbase(obj->integer, base, pcount);
  return PrintCount;
}

/*
  quoted - tests whether an object is quoted
*/
bool quoted (object *obj) {
  return (consp(obj) && car(obj) != NULL && car(obj)->name == sym(QUOTE) && consp(cdr(obj)) && cddr(obj) == NULL);
}

/*
  subwidth - returns the space left from w after printing object
*/
int subwidth (object *obj, int w) {
  if (atom(obj)) return w - atomwidth(obj);
  if (quoted(obj)) obj = car(cdr(obj));
  return subwidthlist(obj, w - 1);
}

/*
  subwidth - returns the space left from w after printing a list
*/
int subwidthlist (object *form, int w) {
  while (form != NULL && w >= 0) {
    if (atom(form)) return w - (2 + atomwidth(form));
    w = subwidth(car(form), w - 1);
    form = cdr(form);
  }
  return w;
}"#

#-gfx
#"
/*
  superprint - handles pretty-printing
*/
void superprint (object *form, int lm, pfun_t pfun) {
  if (atom(form)) {
    if (symbolp(form) && form->name == sym(NOTHING)) printsymbol(form, pfun);
    else printobject(form, pfun);
  } else if (quoted(form)) {
    pfun('\'');
    superprint(car(cdr(form)), lm + 1, pfun);
  } else {
    lm = lm + PPINDENT;
    bool fits = (subwidth(form, PPWIDTH - lm - PPINDENT) >= 0);
    int special = 0, extra = 0; bool separate = true;
    object *arg = car(form);
    if (symbolp(arg) && builtinp(arg->name)) {
      uint8_t minmax = getminmax(builtin(arg->name));
      if (minmax == 0327 || minmax == 0313) special = 2; // defun, setq, setf, defvar
      else if (minmax == 0317 || minmax == 0017 || minmax == 0117 || minmax == 0123) special = 1;
    }
    while (form != NULL) {
      if (atom(form)) { pfstring(PSTR(" . "), pfun); printobject(form, pfun); pfun(')'); return; }
      else if (separate) { 
        pfun('(');
        separate = false;
      } else if (special) {
        pfun(' ');
        special--; 
      } else if (fits) {
        pfun(' ');
      } else { pln(pfun); indent(lm, ' ', pfun); }
      superprint(car(form), lm+extra, pfun);
      form = cdr(form);
    }
    pfun(')');
  }
}"#

#+gfx
#"
/*
  superprint - handles pretty-printing
*/
void superprint (object *form, int lm, pfun_t pfun) {
  if (atom(form)) {
    if (symbolp(form) && form->name == sym(NOTHING)) printsymbol(form, pfun);
    else printobject(form, pfun);
  } else if (quoted(form)) {
    pfun('\'');
    superprint(car(cdr(form)), lm + 1, pfun);
  } else {
    lm = lm + PPINDENT;
    bool fits = (subwidth(form, ppwidth - lm - PPINDENT) >= 0);
    int special = 0, extra = 0; bool separate = true;
    object *arg = car(form);
    if (symbolp(arg) && builtinp(arg->name)) {
      uint8_t minmax = getminmax(builtin(arg->name));
      if (minmax == 0327 || minmax == 0313) special = 2; // defun, setq, setf, defvar
      else if (minmax == 0317 || minmax == 0017 || minmax == 0117 || minmax == 0123) special = 1;
    }
    while (form != NULL) {
      if (atom(form)) { pfstring(PSTR(" . "), pfun); printobject(form, pfun); pfun(')'); return; }
      else if (separate) { 
        pfun('(');
        separate = false;
      } else if (special) {
        pfun(' ');
        special--; 
      } else if (fits) {
        pfun(' ');
      } else { pln(pfun); indent(lm, ' ', pfun); }
      superprint(car(form), lm+extra, pfun);
      form = cdr(form);
    }
    pfun(')');
  }
}"#

#"
/*
  edit - the Lisp tree editor
  Steps through a function definition, editing it a bit at a time, using single-key editing commands.
*/
object *edit (object *fun) {
  while (1) {
    if (tstflag(EXITEDITOR)) return fun;
    char c = gserial();
    if (c == 'q') setflag(EXITEDITOR);
    else if (c == 'b') return fun;
    else if (c == 'r') fun = read(gserial);
    else if (c == '\n') { pfl(pserial); superprint(fun, 0, pserial); pln(pserial); }
    else if (c == 'c') fun = cons(read(gserial), fun);
    else if (atom(fun)) pserial('!');
    else if (c == 'd') fun = cons(car(fun), edit(cdr(fun)));
    else if (c == 'a') fun = cons(edit(car(fun)), cdr(fun));
    else if (c == 'x') fun = cdr(fun);
    else pserial('?');
  }
}"#))
        