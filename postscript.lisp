;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; Postscript

(defparameter *table*
 
  '(

    #-(or avr msp430 badge esp)
    #"
// Table lookup functions

/*
  builtin - looks up a string in lookup_table[], and returns the index of its entry,
  or ENDFUNCTIONS if no match is found
*/
int builtin (char* n) {
  int entry = 0;
  while (entry < ENDFUNCTIONS) {
    if (strcasecmp(n, (char*)lookup_table[entry].string) == 0)
      return entry;
    entry++;
  }
  return ENDFUNCTIONS;
}"#

    #+(and avr (not badge))
    #"
// Table lookup functions

/*
  builtin - looks up a string in lookup_table[], and returns the index of its entry,
  or ENDFUNCTIONS if no match is found
*/
int builtin (char* n) {
  int entry = 0;
  while (entry < ENDFUNCTIONS) {
    #if defined(CPU_ATmega4809)
    if (strcasecmp(n, (char*)lookup_table[entry].string) == 0)
    #else
    if (strcasecmp_P(n, (char*)pgm_read_word(&lookup_table[entry].string)) == 0)
    #endif
      return entry;
    entry++;
  }
  return ENDFUNCTIONS;
}"#

    #+esp
    #"
// Table lookup functions

/*
  builtin - looks up a string in lookup_table[], and returns the index of its entry,
  or ENDFUNCTIONS if no match is found
*/
int builtin (char* n) {
  int entry = 0;
  while (entry < ENDFUNCTIONS) {
    if (strcasecmp_P(n, lookup_table[entry].string) == 0)
      return entry;
    entry++;
  }
  return ENDFUNCTIONS;
}"#

    #+(or msp430 badge)
    #"
// Table lookup functions

/*
  builtin - looks up a string in lookup_table[], and returns the index of its entry,
  or ENDFUNCTIONS if no match is found
*/
int builtin (char* n) {
  int entry = 0;
  while (entry < ENDFUNCTIONS) {
    if (strcasecmp_P(n, (char*)pgm_read_word(&lookup_table[entry].string)) == 0)
      return entry;
    entry++;
  }
  return ENDFUNCTIONS;
}"#

    #-(or arm riscv)
#"
int longsymbol (char *buffer) {
  char *p = SymbolTable;
  int i = 0;
  while (strcasecmp(p, buffer) != 0) {p = p + strlen(p) + 1; i++; }
  if (p == buffer) {
    // Add to symbol table?
    char *newtop = SymbolTop + strlen(p) + 1;
    if (SYMBOLTABLESIZE - (newtop - SymbolTable) < BUFFERSIZE) error2(0, PSTR("no room for long symbols"));
    SymbolTop = newtop;
  }
  if (i > 1535) error2(0, PSTR("too many long symbols"));
  return i + MAXSYMBOL; // First number unused by radix40
}"#

    #+(or arm riscv)
#"
int longsymbol (char *buffer) {
  char *p = SymbolTable;
  int i = 0;
  while (strcasecmp(p, buffer) != 0) {p = p + strlen(p) + 1; i++; }
  if (p == buffer) {
    // Add to symbol table?
    char *newtop = SymbolTop + strlen(p) + 1;
    if (SYMBOLTABLESIZE - (newtop - SymbolTable) < BUFFERSIZE) error2(0, PSTR("symbol table full"));
    SymbolTop = newtop;
  }
  return i + MAXSYMBOL; // First number unused by radix40
}"#

   #-(or avr esp)
   #"
/*
  lookupfn - looks up the entry for name in lookup_table[], and returns the function entry point
*/
intptr_t lookupfn (symbol_t name) {
  return (intptr_t)lookup_table[name].fptr;
}

/*
  getminmax - gets the byte from lookup_table[] whose nibbles specify the minimum and maximum number of arguments for name
*/
uint8_t getminmax (symbol_t name) {
  uint8_t minmax = lookup_table[name].minmax;
  return minmax;
}

/*
  checkminmax - checks that the number of arguments nargs for name is within the range specified in lookup_table[]
*/
void checkminmax (symbol_t name, int nargs) {
  uint8_t minmax = getminmax(name);
  if (nargs<(minmax >> 4)) error2(name, toofewargs);
  if ((minmax & 0x0f) != 0x0f && nargs>(minmax & 0x0f)) error2(name, toomanyargs);
}

/*
  lookupbuiltin - looks up the entry for name in lookup_table[] and returns the name of the function as a string
*/
char *lookupbuiltin (symbol_t name) {
  char *buffer = SymbolTop;
  strcpy(buffer, (char *)lookup_table[name].string);
  return buffer;
}"#

   #+esp
   #"
/*
  lookupfn - looks up the entry for name in lookup_table[], and returns the function entry point
*/
intptr_t lookupfn (symbol_t name) {
  return (intptr_t)pgm_read_ptr(&lookup_table[name].fptr);
}

/*
  getminmax - gets the byte from lookup_table[] whose nibbles specify the minimum and maximum number of arguments for name
*/
uint8_t getminmax (symbol_t name) {
  uint8_t minmax = pgm_read_byte(&lookup_table[name].minmax);
  return minmax;
}

/*
  checkminmax - checks that the number of arguments nargs for name is within the range specified in lookup_table[]
*/
void checkminmax (symbol_t name, int nargs) {
  uint8_t minmax = getminmax(name);
  if (nargs<(minmax >> 4)) error2(name, toofewargs);
  if ((minmax & 0x0f) != 0x0f && nargs>(minmax & 0x0f)) error2(name, toomanyargs);
}

/*
  lookupbuiltin - looks up the entry for name in lookup_table[] and returns the name of the function as a string
*/
char *lookupbuiltin (symbol_t name) {
  char *buffer = SymbolTop;
  strcpy_P(buffer, lookup_table[name].string);
  return buffer;
}"#

    #+(and avr (not badge))
    #"
/*
  lookupfn - looks up the entry for name in lookup_table[], and returns the function entry point
*/
intptr_t lookupfn (symbol_t name) {
  #if defined(CPU_ATmega4809)
  return (intptr_t)lookup_table[name].fptr;
  #else
  return pgm_read_word(&lookup_table[name].fptr);
  #endif
}

/*
  getminmax - gets the byte from lookup_table[] whose nibbles specify the minimum and maximum number of arguments for name
*/
uint8_t getminmax (symbol_t name) {
  #if defined(CPU_ATmega4809)
  uint8_t minmax = lookup_table[name].minmax;
  #else
  uint8_t minmax = pgm_read_byte(&lookup_table[name].minmax);
  #endif
  return minmax;
}

/*
  checkminmax - checks that the number of arguments nargs for name is within the range specified in lookup_table[]
*/
void checkminmax (symbol_t name, int nargs) {
  uint8_t minmax = getminmax(name);
  if (nargs<(minmax >> 4)) error2(name, toofewargs);
  if ((minmax & 0x0f) != 0x0f && nargs>(minmax & 0x0f)) error2(name, toomanyargs);
}

/*
  lookupbuiltin - looks up an entry in the symbol table and returns the name of the function as a string
*/
char *lookupbuiltin (symbol_t name) {
  char *buffer = SymbolTop;
  #if defined(CPU_ATmega4809)
  strcpy(buffer, (char *)(lookup_table[name].string));
  #else
  strcpy_P(buffer, (char *)(pgm_read_word(&lookup_table[name].string)));
  #endif
  return buffer;
}"#

    #+badge
    #"
/*
  lookupfn - looks up an entry in the lookup table, and returns the function entry point
*/
intptr_t lookupfn (symbol_t name) {
  return pgm_read_word(&lookup_table[name].fptr);
}

/*
  checkminmax - checks that the number of arguments nargs for name is within the range specified in lookup_table[]
*/
void checkminmax (symbol_t name, int nargs) {
  uint8_t minmax = pgm_read_byte(&lookup_table[name].minmax);
  if (nargs<(minmax >> 4)) error2(name, toofewargs);
  if ((minmax & 0x0f) != 0x0f && nargs>(minmax & 0x0f)) error2(name, toomanyargs);
}

/*
  lookupbuiltin - looks up an entry in the symbol table and returns the name of the function as a string
*/
char *lookupbuiltin (symbol_t name) {
  char *buffer = SymbolTop;
  strcpy_P(buffer, (char *)(pgm_read_word(&lookup_table[name].string)));
  return buffer;
}"#

    #"
char *lookupsymbol (symbol_t name) {
  char *p = SymbolTable;
  int i = name - MAXSYMBOL;
  while (i > 0 && p < SymbolTop) {p = p + strlen(p) + 1; i--; }
  if (p == SymbolTop) return NULL; else return p;
}

void deletesymbol (symbol_t name) {
  char *p = lookupsymbol(name);
  if (p == NULL) return;
  char *q = p + strlen(p) + 1;
  *p = '\0'; p++;
  while (q < SymbolTop) *(p++) = *(q++);
  SymbolTop = p;
}"#

    #-badge
    #"
void testescape () {
  if (Serial.read() == '~') error2(0, PSTR("escape!"));
}"#

    #+badge
    #"
void testescape () {
#if defined serialmonitor
  if (Serial.read() == '~') error(PSTR("Escape!"));
#endif
}"#))

#+(or esp arm avr riscv)
(defparameter *eval* 
  '(
#"
// Main evaluator"#

#+avr
#"
extern char __bss_end[];"#

#+arm
#"
#if defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41)
#define ENDSTACK _ebss
#else
#define ENDSTACK end
#endif

extern uint32_t ENDSTACK;  // Bottom of stack"#

#+riscv
#"
char end[0];"#

#+arm
#"
object *eval (object *form, object *env) {
  register int *sp asm ("r13");
  int TC=0;
  EVAL:
  // Enough space?
  // Serial.println((uint32_t)sp - (uint32_t)&ENDSTACK); // Find best STACKDIFF value
  if (((uint32_t)sp - (uint32_t)&ENDSTACK) < STACKDIFF) error2(0, PSTR("stack overflow"));
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);      // GC when 1/16 of workspace left
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(0, PSTR("escape!"));}
  if (!tstflag(NOESC)) testescape();"#

#+riscv
#"
object *eval (object *form, object *env) {
  register int *sp asm ("sp");
  int TC=0;
  EVAL:
  // Enough space?
  // Serial.println((uintptr_t)sp - (uintptr_t)end);
  if ((uintptr_t)sp - (uintptr_t)end < STACKDIFF) error2(0, PSTR("Stack overflow"));
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(0, PSTR("Escape!"));}
  if (!tstflag(NOESC)) testescape();"#

#+avr
#"
object *eval (object *form, object *env) {
  uint8_t sp[0];
  int TC=0;
  EVAL:
  // Enough space?
  // Serial.println((uint16_t)sp - (uint16_t)__bss_end); // Find best STACKDIFF value
  if ((uint16_t)sp - (uint16_t)__bss_end < STACKDIFF) error2(0, PSTR("stack overflow"));
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);      // GC when 1/16 of workspace left
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(0, PSTR("escape!"));}
  if (!tstflag(NOESC)) testescape();"#

#+esp
#"
object *eval (object *form, object *env) {
  int TC=0;
  EVAL:
  yield(); // Needed on ESP8266 to avoid Soft WDT Reset
  // Enough space?
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(0, PSTR("Escape!"));}
  if (!tstflag(NOESC)) testescape();"#

#"
  if (form == NULL) return nil;

  if (form->type >= NUMBER && form->type <= STRING) return form;

  if (symbolp(form)) {
    symbol_t name = form->name;
    object *pair = value(name, env);
    if (pair != NULL) return cdr(pair);
    pair = value(name, GlobalEnv);
    if (pair != NULL) return cdr(pair);
    else if (name < ENDFUNCTIONS) return form;
    error(0, PSTR("undefined"), form);
  }"#

#+(or arm riscv)
#"
  if (form->type == CODE) error2(0, PSTR("can't evaluate CODE header"));"#
  
#"
  // It's a list
  object *function = car(form);
  object *args = cdr(form);

  if (function == NULL) error(0, PSTR("illegal function"), nil);
  if (!listp(args)) error(0, PSTR("can't evaluate a dotted pair"), args);

  // List starts with a symbol?
  if (symbolp(function)) {
    symbol_t name = function->name;

    if ((name == LET) || (name == LETSTAR)) {
      int TCstart = TC;
      if (args == NULL) error2(name, noargument);
      object *assigns = first(args);
      if (!listp(assigns)) error(name, notalist, assigns);
      object *forms = cdr(args);
      object *newenv = env;
      push(newenv, GCStack);
      while (assigns != NULL) {
        object *assign = car(assigns);
        if (!consp(assign)) push(cons(assign,nil), newenv);
        else if (cdr(assign) == NULL) push(cons(first(assign),nil), newenv);
        else push(cons(first(assign),eval(second(assign),env)), newenv);
        car(GCStack) = newenv;
        if (name == LETSTAR) env = newenv;
        assigns = cdr(assigns);
      }
      env = newenv;
      pop(GCStack);
      form = tf_progn(forms,env);
      TC = TCstart;
      goto EVAL;
    }

    if (name == LAMBDA) {
      if (env == NULL) return form;
      object *envcopy = NULL;
      while (env != NULL) {
        object *pair = first(env);
        if (pair != NULL) push(pair, envcopy);
        env = cdr(env);
      }
      return cons(symbol(CLOSURE), cons(envcopy,args));
    }

    if ((name > SPECIAL_FORMS) && (name < TAIL_FORMS)) {
      return ((fn_ptr_type)lookupfn(name))(args, env);
    }

    if ((name > TAIL_FORMS) && (name < FUNCTIONS)) {
      form = ((fn_ptr_type)lookupfn(name))(args, env);
      TC = 1;
      goto EVAL;
    }

    if ((name < SPECIAL_FORMS) || ((name > KEYWORDS) && (name < USERFUNCTIONS))) error2(name, PSTR("can't be used as a function"));
  }

  // Evaluate the parameters - result in head
  object *fname = car(form);
  int TCstart = TC;
  object *head = cons(eval(fname, env), NULL);
  push(head, GCStack); // Don't GC the result list
  object *tail = head;
  form = cdr(form);
  int nargs = 0;

  while (form != NULL){
    object *obj = cons(eval(car(form),env),NULL);
    cdr(tail) = obj;
    tail = obj;
    form = cdr(form);
    nargs++;
  }

  function = car(head);
  args = cdr(head);

  if (symbolp(function)) {
    symbol_t name = function->name;
    if (name >= ENDFUNCTIONS) error(0, PSTR("not valid here"), fname);
    checkminmax(name, nargs);
    object *result = ((fn_ptr_type)lookupfn(name))(args, env);
    pop(GCStack);
    return result;
  }

  if (consp(function)) {
    symbol_t name = 0;
    if (!listp(fname)) name = fname->name;

    if (issymbol(car(function), LAMBDA)) {
      form = closure(TCstart, name, NULL, cdr(function), args, &env);
      pop(GCStack);
      int trace = tracing(fname->name);
      if (trace) {
        object *result = eval(form, env);
        indent((--(TraceDepth[trace-1]))<<1, ' ', pserial);
        pint(TraceDepth[trace-1], pserial);
        pserial(':'); pserial(' ');
        printobject(fname, pserial); pfstring(PSTR(" returned "), pserial);
        printobject(result, pserial); pln(pserial);
        return result;
      } else {
        TC = 1;
        goto EVAL;
      }
    }

    if (issymbol(car(function), CLOSURE)) {
      function = cdr(function);
      form = closure(TCstart, name, car(function), cdr(function), args, &env);
      pop(GCStack);
      TC = 1;
      goto EVAL;
    }"#

#+arm
#"
    if (car(function)->type == CODE) {
      int n = listlength(DEFCODE, second(function));
      if (nargs<n) error2(fname->name, toofewargs);
      if (nargs>n) error2(fname->name, toomanyargs);
      uint32_t entry = startblock(car(function)) + 1;
      pop(GCStack);
      return call(entry, n, args, env);
    }"#

#+riscv
#"
    if (car(function)->type == CODE) {
      int n = listlength(DEFCODE, second(function));
      if (nargs<n) error2(fname->name, toofewargs);
      if (nargs>n) error2(fname->name, toomanyargs);
      uint32_t entry = startblock(car(function));
      pop(GCStack);
      return call(entry, n, args, env);
    }"#

#"
  }
  error(0, PSTR("illegal function"), fname); return nil;
}"#))

#+ignore
(defparameter *eval* 
  '(
#"
// Main evaluator

uint8_t End;

object *eval (object *form, object *env) {
  int TC=0;
  EVAL:
  yield(); // Needed on ESP8266 to avoid Soft WDT Reset
  // Enough space?
  if (End != 0xA5) error2(0, PSTR("Stack overflow"));
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(0, PSTR("Escape!"));}
  if (!tstflag(NOESC)) testescape();
  
  if (form == NULL) return nil;

  if (integerp(form) || floatp(form) || characterp(form) || stringp(form)) return form;

  if (symbolp(form)) {
    symbol_t name = form->name;
    if (name == NIL) return nil;
    object *pair = value(name, env);
    if (pair != NULL) return cdr(pair);
    pair = value(name, GlobalEnv);
    if (pair != NULL) return cdr(pair);
    else if (name <= ENDFUNCTIONS) return form;
    error(0, PSTR("undefined"), form);
  }
  
  // It's a list
  object *function = car(form);
  object *args = cdr(form);

  if (function == NULL) error(0, PSTR("illegal function"), nil);
  if (!listp(args)) error(0, PSTR("can't evaluate a dotted pair"), args);

  // List starts with a symbol?
  if (symbolp(function)) {
    symbol_t name = function->name;

    if ((name == LET) || (name == LETSTAR)) {
      int TCstart = TC;
      if (args == NULL) error2(name, noargument);
      object *assigns = first(args);
      object *forms = cdr(args);
      object *newenv = env;
      push(newenv, GCStack);
      while (assigns != NULL) {
        object *assign = car(assigns);
        if (!consp(assign)) push(cons(assign,nil), newenv);
        else if (cdr(assign) == NULL) push(cons(first(assign),nil), newenv);
        else push(cons(first(assign),eval(second(assign),env)), newenv);
        car(GCStack) = newenv;
        if (name == LETSTAR) env = newenv;
        assigns = cdr(assigns);
      }
      env = newenv;
      pop(GCStack);
      form = tf_progn(forms,env);
      TC = TCstart;
      goto EVAL;
    }

    if (name == LAMBDA) {
      if (env == NULL) return form;
      object *envcopy = NULL;
      while (env != NULL) {
        object *pair = first(env);
        if (pair != NULL) push(pair, envcopy);
        env = cdr(env);
      }
      return cons(symbol(CLOSURE), cons(envcopy,args));
    }
    
    if (name < SPECIAL_FORMS) error2((int)function, PSTR("can't be used as a function"));

    if ((name > SPECIAL_FORMS) && (name < TAIL_FORMS)) {
      return ((fn_ptr_type)lookupfn(name))(args, env);
    }

    if ((name > TAIL_FORMS) && (name < FUNCTIONS)) {
      form = ((fn_ptr_type)lookupfn(name))(args, env);
      TC = 1;
      goto EVAL;
    }
  }

  // Evaluate the parameters - result in head
  object *fname = car(form);
  int TCstart = TC;
  object *head = cons(eval(car(form), env), NULL);
  push(head, GCStack); // Don't GC the result list
  object *tail = head;
  form = cdr(form);
  int nargs = 0;

  while (form != NULL){
    object *obj = cons(eval(car(form),env),NULL);
    cdr(tail) = obj;
    tail = obj;
    form = cdr(form);
    nargs++;
  }

  function = car(head);
  args = cdr(head);
 
  if (symbolp(function)) {
    symbol_t name = function->name;
    if (name >= ENDFUNCTIONS) error(0, PSTR("not valid here"), fname);
    if (nargs<lookupmin(name)) error2(name, PSTR("has too few arguments"));
    if (nargs>lookupmax(name)) error2(name, PSTR("has too many arguments"));
    object *result = ((fn_ptr_type)lookupfn(name))(args, env);
    pop(GCStack);
    return result;
  }

  if (consp(function) && issymbol(car(function), LAMBDA)) {
    form = closure(TCstart, fname->name, NULL, cdr(function), args, &env);
    pop(GCStack);
    int trace = tracing(fname->name);
    if (trace) {
      object *result = eval(form, env);
      indent((--(TraceDepth[trace-1]))<<1, pserial);
      pint(TraceDepth[trace-1], pserial);
      pserial(':'); pserial(' ');
      printobject(fname, pserial); pfstring(PSTR(" returned "), pserial);
      printobject(result, pserial); pln(pserial);
      return result;
    } else {
      TC = 1;
      goto EVAL;
    }
  }

  if (consp(function) && issymbol(car(function), CLOSURE)) {
    function = cdr(function);
    form = closure(TCstart, fname->name, car(function), cdr(function), args, &env);
    pop(GCStack);
    TC = 1;
    goto EVAL;
  } 
  
  error(0, PSTR("illegal function"), fname); return nil;
}"#))

(defparameter *print-functions* 

  '(#"
// Print functions

inline int maxbuffer (char *buffer) {
  return SYMBOLTABLESIZE-(buffer-SymbolTable)-1;
}"#

  #-badge
  #"
void pserial (char c) {
  LastPrint = c;
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
}"#


  #+badge
  #"
void pserial (char c) {
  LastPrint = c;
  Display(c);
  #if defined (serialmonitor)
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
  #endif
}"#

    #+avr
    #"
const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

void pcharacter (uint8_t c, pfun_t pfun) {
  if (!tstflag(PRINTREADABLY)) pfun(c);
  else {
    pfun('#'); pfun('\\');
    if (c <= 32) {
      PGM_P p = ControlCodes;
      #if defined(CPU_ATmega4809)
      while (c > 0) {p = p + strlen(p) + 1; c--; }
      #else
      while (c > 0) {p = p + strlen_P(p) + 1; c--; }
      #endif
      pfstring(p, pfun);
    } else if (c < 127) pfun(c);
    else pint(c, pfun);
  }
}"#

    #+esp
    #"
const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

void pcharacter (uint8_t c, pfun_t pfun) {
  if (!tstflag(PRINTREADABLY)) pfun(c);
  else {
    pfun('#'); pfun('\\');
    if (c <= 32) {
      PGM_P p = ControlCodes;
      while (c > 0) {p = p + strlen_P(p) + 1; c--; }
      pfstring(p, pfun);
    } else if (c < 127) pfun(c);
    else pint(c, pfun);
  }
}"#

    #-(or avr esp)
    #"
const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

/*
  pcharacter - prints a character
  If <= 32 prints character name; eg #\Space
  If < 127 prints ASCII; eg #\A
  Otherwise prints decimal; eg #\234
*/
void pcharacter (uint8_t c, pfun_t pfun) {
  if (!tstflag(PRINTREADABLY)) pfun(c);
  else {
    pfun('#'); pfun('\\');
    if (c <= 32) {
      const char *p = ControlCodes;
      while (c > 0) {p = p + strlen(p) + 1; c--; }
      pfstring(p, pfun);
    } else if (c < 127) pfun(c);
    else pint(c, pfun);
  }
}"#

    #"
/*
  pstring - prints a C string to the specified print stream
*/
void pstring (char *s, pfun_t pfun) {
  while (*s) pfun(*s++);
}"#

    #"
/*
  printstring - prints a Lisp string to the specified print stream
*/
void printstring (object *form, pfun_t pfun) {
  if (tstflag(PRINTREADABLY)) pfun('"');
  form = cdr(form);
  while (form != NULL) {
    int chars = form->chars;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (tstflag(PRINTREADABLY) && (ch == '"' || ch == '\\')) pfun('\\');
      if (ch) pfun(ch);
    }
    form = car(form);
  }
  if (tstflag(PRINTREADABLY)) pfun('"');
}"#

    #+(or msp430 badge)
    #"
/*
  pfstring - prints a string from flash memory
*/
void pfstring (PGM_P s, pfun_t pfun) {
  intptr_t p = (intptr_t)s;
  while (1) {
    char c = pgm_read_byte(p++);
    if (c == 0) return;
    pfun(c);
  }
}"#

    #+(and avr (not badge))
    #"
/*
  pfstring - prints a string from flash memory
*/
void pfstring (PGM_P s, pfun_t pfun) {
  int p = 0;
  while (1) {
    #if defined(CPU_ATmega4809)
    char c = s[p++];
    #else
    char c = pgm_read_byte(&s[p++]);
    #endif
    if (c == 0) return;
    pfun(c);
  }
}"#

    #+esp
    #"
void pfstring (PGM_P s, pfun_t pfun) {
  char str[strlen_P(s)+1];
  strcpy_P(str, s);
  pstring(str, pfun);
}"#

    #-(or avr msp430 badge esp)
    #"
void pfstring (const char *s, pfun_t pfun) {
  int p = 0;
  while (1) {
    char c = s[p++];
    if (c == 0) return;
    pfun(c);
  }
}"#

    #"
void pint (int i, pfun_t pfun) {
  int lead = 0;
  #if INT_MAX == 32767
  int p = 10000;
  #else
  int p = 1000000000;
  #endif
  if (i<0) pfun('-');
  for (int d=p; d>0; d=d/10) {
    int j = i/d;
    if (j!=0 || lead || d==1) { pfun(abs(j)+'0'); lead=1;}
    i = i - j*d;
  }
}"#

    #+(or arm riscv esp)
    #"
/*
  pintbase - prints an integer in a specified base. power2 is 1 for binary and 4 for hexadecimal
*/
void pintbase (uint32_t i, uint8_t power2, pfun_t pfun) {
  int lead = 0;
  uint32_t p = 1<<(32-power2);
  for (uint32_t d=p; d>0; d=d>>power2) {
    uint32_t j = i/d;
    if (j!=0 || lead || d==1) { pfun((j<10) ? j+'0' : j+'W'); lead=1;}
    i = i - j*d;
  }
}"#

    #+(or avr msp430)
    #"
/*
  pintbase - prints an integer in a specified base. power2 is 1 for binary and 4 for hexadecimal
*/
void pintbase (uint16_t i, uint8_t power2, pfun_t pfun) {
  int lead = 0;
  uint16_t p = 1<<(16-power2);
  for (uint16_t d=p; d>0; d=d>>power2) {
    uint16_t j = i/d;
    if (j!=0 || lead || d==1) { pfun((j<10) ? j+'0' : j+'W'); lead=1;}  
    i = i - j*d;
  }
}"#

    #+code
    #"
/*
  pinthex4 - prints a four-digit hexadecimal number with leading zeros
*/
void printhex4 (int i, pfun_t pfun) {
  int p = 0x1000;
  for (int d=p; d>0; d=d/16) {
    int j = i/d;
    pfun((j<10) ? j+'0' : j + 'W'); 
    i = i - j*d;
  }
  pfun(' ');
}"#

    #+float
    #"
void pmantissa (float f, pfun_t pfun) {
  int sig = floor(log10(f));
  int mul = pow(10, 5 - sig);
  int i = round(f * mul);
  bool point = false;
  if (i == 1000000) { i = 100000; sig++; }
  if (sig < 0) {
    pfun('0'); pfun('.'); point = true;
    for (int j=0; j < - sig - 1; j++) pfun('0');
  }
  mul = 100000;
  for (int j=0; j<7; j++) {
    int d = (int)(i / mul);
    pfun(d + '0');
    i = i - d * mul;
    if (i == 0) {
      if (!point) {
        for (int k=j; k<sig; k++) pfun('0');
        pfun('.'); pfun('0');
      }
      return;
    }
    if (j == sig && sig >= 0) { pfun('.'); point = true; }
    mul = mul / 10;
  }
}

void pfloat (float f, pfun_t pfun) {
  if (isnan(f)) { pfstring(PSTR("NaN"), pfun); return; }
  if (f == 0.0) { pfun('0'); return; }
  if (isinf(f)) { pfstring(PSTR("Inf"), pfun); return; }
  if (f < 0) { pfun('-'); f = -f; }
  // Calculate exponent
  int e = 0;
  if (f < 1e-3 || f >= 1e5) {
    e = floor(log(f) / 2.302585); // log10 gives wrong result
    f = f / pow(10, e);
  }

  pmantissa (f, pfun);

  // Exponent
  if (e != 0) {
    pfun('e');
    pint(e, pfun);
  }
}"#

    #"
inline void pln (pfun_t pfun) {
  pfun('\n');
}"#

    #"
void pfl (pfun_t pfun) {
  if (LastPrint != '\n') pfun('\n');
}"#

    #"
void plist (object *form, pfun_t pfun) {
  pfun('(');
  printobject(car(form), pfun);
  form = cdr(form);
  while (form != NULL && listp(form)) {
    pfun(' ');
    printobject(car(form), pfun);
    form = cdr(form);
  }
  if (form != NULL) {
    pfstring(PSTR(" . "), pfun);
    printobject(form, pfun);
  }
  pfun(')');
}"#

    #"
void pstream (object *form, pfun_t pfun) {
  pfun('<');
  pfstring(streamname[(form->integer)>>8], pfun);
  pfstring(PSTR("-stream "), pfun);
  pint(form->integer & 0xFF, pfun);
  pfun('>');
}"#

   #-(or arm riscv esp)
   #"
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && issymbol(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) plist(form, pfun);
  else if (integerp(form)) pint(form->integer, pfun);
  else if (symbolp(form)) { if (form->name != NOTHING) pstring(symbolname(form->name), pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (streamp(form)) pstream(form, pfun);
  else error2(0, PSTR("error in print"));
}

void prin1object (object *form, pfun_t pfun) {
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printobject(form, pfun);
  Flags = temp;
}"#

   #+(or arm riscv)
   #"
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && issymbol(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) plist(form, pfun);
  else if (integerp(form)) pint(form->integer, pfun);
  else if (floatp(form)) pfloat(form->single_float, pfun);
  else if (symbolp(form)) { if (form->name != NOTHING) pstring(symbolname(form->name), pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (arrayp(form)) printarray(form, pfun);
  else if (form->type == CODE) pfstring(PSTR("code"), pfun);
  else if (streamp(form)) pstream(form, pfun);
  else error2(0, PSTR("error in print"));
}

void prin1object (object *form, pfun_t pfun) {
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printobject(form, pfun);
  Flags = temp;
}"#

; Has LCDSTREAM
    #+msp430
    #"
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && issymbol(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) {
    pfun('(');
    printobject(car(form), pfun);
    form = cdr(form);
    while (form != NULL && listp(form)) {
      pfun(' ');
      printobject(car(form), pfun);
      form = cdr(form);
    }
    if (form != NULL) {
      pfstring(PSTR(" . "), pfun);
      printobject(form, pfun);
    }
    pfun(')');
  } else if (integerp(form)) pint(form->integer, pfun);
  else if (symbolp(form)) { if (form->name != NOTHING) pstring(symbolname(form->name), pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (streamp(form)) {
    pfun('<');
    if ((form->integer)>>8 == SPISTREAM) pfstring(PSTR("spi"), pfun);
    else if ((form->integer)>>8 == I2CSTREAM) pfstring(PSTR("i2c"), pfun);
    else if ((form->integer)>>8 == SDSTREAM) pfstring(PSTR("sd"), pfun);
    else if ((form->integer)>>8 == STRINGSTREAM) pfstring(PSTR("string"), pfun);
    else if ((form->integer)>>8 == LCDSTREAM) pfstring(PSTR("lcd"), pfun);
    else pfstring(PSTR("serial"), pfun);
    pfstring(PSTR("-stream "), pfun);
    pint((form->integer) & 0xFF, pfun);
    pfun('>');
  } else error2(0, PSTR("error in print"));
}

void prin1object (object *form, pfun_t pfun) {
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printobject(form, pfun);
  Flags = temp;
}"#

    #+ignore
    #"
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && issymbol(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) {
    pfun('(');
    printobject(car(form), pfun);
    form = cdr(form);
    while (form != NULL && listp(form)) {
      pfun(' ');
      printobject(car(form), pfun);
      form = cdr(form);
    }
    if (form != NULL) {
      pfstring(PSTR(" . "), pfun);
      printobject(form, pfun);
    }
    pfun(')');
  } else if (integerp(form)) pint(form->integer, pfun);
  else if (symbolp(form)) { if (form->name != NOTHING) pstring(symbolname(form->name), pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (streamp(form)) {
    pfun('<');
    if ((form->integer)>>8 == SPISTREAM) pfstring(PSTR("spi"), pfun);
    else if ((form->integer)>>8 == I2CSTREAM) pfstring(PSTR("i2c"), pfun);
    else if ((form->integer)>>8 == SDSTREAM) pfstring(PSTR("sd"), pfun);
    else pfstring(PSTR("serial"), pfun);
    pfstring(PSTR("-stream "), pfun);
    pint(form->integer & 0xFF, pfun);
    pfun('>');
  } else
    error2(0, PSTR("Error in print"));
}

void prin1object (object *form, pfun_t pfun) {
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printobject(form, pfun);
  Flags = temp;
}"#

; Has float but not code
    #+(or stm32 esp)
    #"
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && issymbol(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) plist(form, pfun);
  else if (integerp(form)) pint(form->integer, pfun);
  else if (floatp(form)) pfloat(form->single_float, pfun);
  else if (symbolp(form)) { if (form->name != NOTHING) pstring(symbolname(form->name), pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (arrayp(form)) printarray(form, pfun);
  else if (streamp(form)) pstream(form, pfun);
  else error2(0, PSTR("Error in print"));
}

void prin1object (object *form, pfun_t pfun) {
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printobject(form, pfun);
  Flags = temp;
}"#))

(defparameter *read-functions*

  '(

    #+badge
    #"
// For Lisp Badge
volatile int WritePtr = 0, ReadPtr = 0;
const int KybdBufSize = 333; // 42*8 - 3
char KybdBuf[KybdBufSize];
volatile uint8_t KybdAvailable = 0;"#


    #+(and avr (not badge))
    #"
// Read functions

int glibrary () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  #if defined(CPU_ATmega4809)
  char c = LispLibrary[GlobalStringIndex++];
  #else
  char c = pgm_read_byte(&LispLibrary[GlobalStringIndex++]);
  #endif
  return (c != 0) ? c : -1; // -1?
}

void loadfromlibrary (object *env) {
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    push(line, GCStack);
    eval(line, env);
    pop(GCStack);
    line = read(glibrary);
  }
}"#

    #+(or msp430 badge)
    #"
// Read functions

int glibrary () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = pgm_read_byte(&LispLibrary[GlobalStringIndex++]);
  return (c != 0) ? c : -1; // -1?
}

void loadfromlibrary (object *env) {
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    push(line, GCStack);
    eval(line, env);
    pop(GCStack);
    line = read(glibrary);
  }
}"#

    #+esp
       #"
// Read functions

int glibrary () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = pgm_read_byte(&LispLibrary[GlobalStringIndex++]);
  return (c != 0) ? c : -1; // -1?
}

void loadfromlibrary (object *env) {
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    push(line, GCStack);
    eval(line, env);
    pop(GCStack);
    line = read(glibrary);
  }
}"#

    #+(or arm stm32 riscv)
       #"
// Read functions

int glibrary () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = LispLibrary[GlobalStringIndex++];
  return (c != 0) ? c : -1; // -1?
}

void loadfromlibrary (object *env) {
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    push(line, GCStack);
    eval(line, env);
    pop(GCStack);
    line = read(glibrary);
  }
}"#

  #-badge
  #"
// For line editor
const int TerminalWidth = 80;
volatile int WritePtr = 0, ReadPtr = 0;
const int KybdBufSize = 333; // 42*8 - 3
char KybdBuf[KybdBufSize];
volatile uint8_t KybdAvailable = 0;

// Parenthesis highlighting
void esc (int p, char c) {
  Serial.write('\e'); Serial.write('[');
  Serial.write((char)('0'+ p/100));
  Serial.write((char)('0'+ (p/10) % 10));
  Serial.write((char)('0'+ p % 10));
  Serial.write(c);
}

void hilight (char c) {
  Serial.write('\e'); Serial.write('['); Serial.write(c); Serial.write('m');
}

void Highlight (int p, int wp, uint8_t invert) {
  wp = wp + 2; // Prompt
#if defined (printfreespace)
  int f = Freespace;
  while (f) { wp++; f=f/10; }
#endif
  int line = wp/TerminalWidth;
  int col = wp%TerminalWidth;
  int targetline = (wp - p)/TerminalWidth;
  int targetcol = (wp - p)%TerminalWidth;
  int up = line-targetline, left = col-targetcol;
  if (p) {
    if (up) esc(up, 'A');
    if (col > targetcol) esc(left, 'D'); else esc(-left, 'C');
    if (invert) hilight('7');
    Serial.write('('); Serial.write('\b');
    // Go back
    if (up) esc(up, 'B'); // Down
    if (col > targetcol) esc(left, 'C'); else esc(-left, 'D');
    Serial.write('\b'); Serial.write(')');
    if (invert) hilight('0');
  }
}

void processkey (char c) {
  if (c == 27) { setflag(ESCAPE); return; }    // Escape key
#if defined(vt100)
  static int parenthesis = 0, wp = 0;
  // Undo previous parenthesis highlight
  Highlight(parenthesis, wp, 0);
  parenthesis = 0;
#endif
  // Edit buffer
  if (c == '\n' || c == '\r') {
    pserial('\n');
    KybdAvailable = 1;
    ReadPtr = 0;
    return;
  }
  if (c == 8 || c == 0x7f) {     // Backspace key
    if (WritePtr > 0) {
      WritePtr--;
      Serial.write(8); Serial.write(' '); Serial.write(8);
      if (WritePtr) c = KybdBuf[WritePtr-1];
    }
  } else if (WritePtr < KybdBufSize) {
    KybdBuf[WritePtr++] = c;
    Serial.write(c);
  }
#if defined(vt100)
  // Do new parenthesis highlight
  if (c == ')') {
    int search = WritePtr-1, level = 0;
    while (search >= 0 && parenthesis == 0) {
      c = KybdBuf[search--];
      if (c == ')') level++;
      if (c == '(') {
        level--;
        if (level == 0) {parenthesis = WritePtr-search-1; wp = WritePtr; }
      }
    }
    Highlight(parenthesis, wp, 1);
  }
#endif
  return;
}

int gserial () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
#if defined(lineeditor)
  while (!KybdAvailable) {
    while (!Serial.available());
    char temp = Serial.read();
    processkey(temp);
  }
  if (ReadPtr != WritePtr) return KybdBuf[ReadPtr++];
  KybdAvailable = 0;
  WritePtr = 0;
  return '\n';
#else
  while (!Serial.available());
  char temp = Serial.read();
  if (temp != '\n') pserial(temp);
  return temp;
#endif
}"#

  #+badge
  #"
int gserial () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  #if defined (serialmonitor)
  while (!Serial.available() && !KybdAvailable);
  if (Serial.available()) {
    char temp = Serial.read();
    if (temp != '\n') Serial.print(temp); // pserial(temp);
    return temp;
  } else {
    if (ReadPtr != WritePtr) {
      char temp = KybdBuf[ReadPtr++];
      Serial.write(temp);
      return temp;
    }
    KybdAvailable = 0;
    WritePtr = 0;
    return '\n';
  }
  #else
  while (!KybdAvailable);
  if (ReadPtr != WritePtr) return KybdBuf[ReadPtr++];
  KybdAvailable = 0;
  WritePtr = 0;
  return '\n';
  #endif
}"#

  #+(or avr msp430)
  #"
object *nextitem (gfun_t gfun) {
  int ch = gfun();
  while(issp(ch)) ch = gfun();

  if (ch == ';') {
    while(ch != '(') ch = gfun();
    ch = '(';
  }
  if (ch == '\n') ch = gfun();
  if (ch == -1) return nil;
  if (ch == ')') return (object *)KET;
  if (ch == '(') return (object *)BRA;
  if (ch == '\'') return (object *)QUO;
  if (ch == '.') return (object *)DOT;

  // Parse string
  if (ch == '"') return readstring('"', gfun);

  // Parse symbol, character, or number
  int index = 0, base = 10, sign = 1;
  char *buffer = SymbolTop;
  int bufmax = maxbuffer(buffer); // Max index
  unsigned int result = 0;
  if (ch == '+' || ch == '-') {
    buffer[index++] = ch;
    if (ch == '-') sign = -1;
    ch = gfun();
  }

  // Parse reader macros
  else if (ch == '#') {
    ch = gfun();
    char ch2 = ch & ~0x20; // force to upper case
    if (ch == '\\') { // Character
      base = 0; ch = gfun();
      if (issp(ch) || ch == ')' || ch == '(') return character(ch);
      else LastChar = ch;
    } else if (ch == '|') {
      do { while (gfun() != '|'); }
      while (gfun() != '#');
      return nextitem(gfun);
    } else if (ch2 == 'B') base = 2;
    else if (ch2 == 'O') base = 8;
    else if (ch2 == 'X') base = 16;
    else if (ch == '\'') return nextitem(gfun);
    else if (ch == '.') {
      setflag(NOESC);
      object *result = eval(read(gfun), NULL);
      clrflag(NOESC);
      return result;
    } else error2(0, PSTR("illegal character after #"));
    ch = gfun();
  }

  int isnumber = (digitvalue(ch)<base);
  buffer[2] = '\0'; // In case symbol is one letter

  while(!issp(ch) && ch != ')' && ch != '(' && index < bufmax) {
    buffer[index++] = ch;
    int temp = digitvalue(ch);
    result = result * base + temp;
    isnumber = isnumber && (digitvalue(ch)<base);
    ch = gfun();
  }"#

  #+avr
  #"
  buffer[index] = '\0';
  if (ch == ')' || ch == '(') LastChar = ch;

  if (isnumber) {
    if (base == 10 && result > ((unsigned int)INT_MAX+(1-sign)/2)) 
      error2(0, PSTR("Number out of range"));
    return number(result*sign);
  } else if (base == 0) {
    if (index == 1) return character(buffer[0]);
    PGM_P p = ControlCodes; char c = 0;
    while (c < 33) {
      #if defined(CPU_ATmega4809)
      if (strcasecmp(buffer, p) == 0) return character(c);
      p = p + strlen(p) + 1; c++;
      #else
      if (strcasecmp_P(buffer, p) == 0) return character(c);
      p = p + strlen_P(p) + 1; c++;
      #endif
    }
    if (index == 3) return character((buffer[0]*10+buffer[1])*10+buffer[2]-5328);
    error2(0, PSTR("unknown character"));
  }
  
  int x = builtin(buffer);
  if (x == NIL) return nil;
  if (x < ENDFUNCTIONS) return newsymbol(x);
  else if (index < 4 && valid40(buffer)) return newsymbol(pack40(buffer));
  else return newsymbol(longsymbol(buffer));
}"#

  #+msp430
  #"
  buffer[index] = '\0';
  if (ch == ')' || ch == '(') LastChar = ch;

  if (isnumber) {
    if (base == 10 && result > ((unsigned int)INT_MAX+(1-sign)/2)) 
      error2(0, PSTR("Number out of range"));
    return number(result*sign);
  } else if (base == 0) {
    if (index == 1) return character(buffer[0]);
    PGM_P p = ControlCodes; char c = 0;
    while (c < 33) {
      if (strcasecmp_P(buffer, p) == 0) return character(c);
      p = p + strlen_P(p) + 1; c++;
    }
    if (index == 3) return character((buffer[0]*10+buffer[1])*10+buffer[2]-5328);
    error2(0, PSTR("unknown character"));
  }
  
  int x = builtin(buffer);
  if (x == NIL) return nil;
  if (x < ENDFUNCTIONS) return newsymbol(x);
  else if (index < 4 && valid40(buffer)) return newsymbol(pack40(buffer));
  else return newsymbol(longsymbol(buffer));
}"#

  #+float
  #"
object *nextitem (gfun_t gfun) {
  int ch = gfun();
  while(issp(ch)) ch = gfun();

  if (ch == ';') {
    while(ch != '(') ch = gfun();
    ch = '(';
  }
  if (ch == '\n') ch = gfun();
  if (ch == -1) return nil;
  if (ch == ')') return (object *)KET;
  if (ch == '(') return (object *)BRA;
  if (ch == '\'') return (object *)QUO;

  // Parse string
  if (ch == '"') return readstring('"', gfun);

  // Parse symbol, character, or number
  int index = 0, base = 10, sign = 1;
  char *buffer = SymbolTop;
  int bufmax = maxbuffer(buffer); // Max index
  unsigned int result = 0;
  bool isfloat = false;
  float fresult = 0.0;

  if (ch == '+') {
    buffer[index++] = ch;
    ch = gfun();
  } else if (ch == '-') {
    sign = -1;
    buffer[index++] = ch;
    ch = gfun();
  } else if (ch == '.') {
    buffer[index++] = ch;
    ch = gfun();
    if (ch == ' ') return (object *)DOT;
    isfloat = true;
  }

  // Parse reader macros
  else if (ch == '#') {
    ch = gfun();
    char ch2 = ch & ~0x20; // force to upper case
    if (ch == '\\') { // Character
      base = 0; ch = gfun();
      if (issp(ch) || ch == ')' || ch == '(') return character(ch);
      else LastChar = ch;
    } else if (ch == '|') {
      do { while (gfun() != '|'); }
      while (gfun() != '#');
      return nextitem(gfun);
    } else if (ch2 == 'B') base = 2;
    else if (ch2 == 'O') base = 8;
    else if (ch2 == 'X') base = 16;
    else if (ch == '\'') return nextitem(gfun);
    else if (ch == '.') {
      setflag(NOESC);
      object *result = eval(read(gfun), NULL);
      clrflag(NOESC);
      return result;
    }
    else if (ch == '(') { LastChar = ch; return readarray(1, read(gfun)); }
    else if (ch == '*') return readbitarray(gfun);
    else if (ch >= '1' && ch <= '9' && (gfun() & ~0x20) == 'A') return readarray(ch - '0', read(gfun));
    else error2(0, PSTR("illegal character after #"));
    ch = gfun();
  }
  int valid; // 0=undecided, -1=invalid, +1=valid
  if (ch == '.') valid = 0; else if (digitvalue(ch)<base) valid = 1; else valid = -1;
  bool isexponent = false;
  int exponent = 0, esign = 1;
  buffer[2] = '\0'; buffer[3] = '\0'; buffer[4] = '\0'; buffer[5] = '\0'; // In case symbol is < 5 letters
  float divisor = 10.0;

  while(!issp(ch) && ch != ')' && ch != '(' && index < bufmax) {
    buffer[index++] = ch;
    if (base == 10 && ch == '.' && !isexponent) {
      isfloat = true;
      fresult = result;
    } else if (base == 10 && (ch == 'e' || ch == 'E')) {
      if (!isfloat) { isfloat = true; fresult = result; }
      isexponent = true;
      if (valid == 1) valid = 0; else valid = -1;
    } else if (isexponent && ch == '-') {
      esign = -esign;
    } else if (isexponent && ch == '+') {
    } else {
      int digit = digitvalue(ch);
      if (digitvalue(ch)<base && valid != -1) valid = 1; else valid = -1;
      if (isexponent) {
        exponent = exponent * 10 + digit;
      } else if (isfloat) {
        fresult = fresult + digit / divisor;
        divisor = divisor * 10.0;
      } else {
        result = result * base + digit;
      }
    }
    ch = gfun();
  }"#

  #+(and float (not esp))
  #"
  buffer[index] = '\0';
  if (ch == ')' || ch == '(') LastChar = ch;
  if (isfloat && valid == 1) return makefloat(fresult * sign * pow(10, exponent * esign));
  else if (valid == 1) {
    if (base == 10 && result > ((unsigned int)INT_MAX+(1-sign)/2))
      return makefloat((float)result*sign);
    return number(result*sign);
  } else if (base == 0) {
    if (index == 1) return character(buffer[0]);
    const char* p = ControlCodes; char c = 0;
    while (c < 33) {
      if (strcasecmp(buffer, p) == 0) return character(c);
      p = p + strlen(p) + 1; c++;
    }
    if (index == 3) return character((buffer[0]*10+buffer[1])*10+buffer[2]-5328);
    error2(0, PSTR("unknown character"));
  }

  int x = builtin(buffer);
  if (x == NIL) return nil;
  if (x < ENDFUNCTIONS) return newsymbol(x);
  else if (index <= 6 && valid40(buffer)) return newsymbol(pack40(buffer));
  else return newsymbol(longsymbol(buffer));
}"#

  #+esp
  #"
  buffer[index] = '\0';
  if (ch == ')' || ch == '(') LastChar = ch;
  if (isfloat && valid == 1) return makefloat(fresult * sign * pow(10, exponent * esign));
  else if (valid == 1) {
    if (base == 10 && result > ((unsigned int)INT_MAX+(1-sign)/2))
      return makefloat((float)result*sign);
    return number(result*sign);
  } else if (base == 0) {
    if (index == 1) return character(buffer[0]);
    PGM_P p = ControlCodes; char c = 0;
    while (c < 33) {
      if (strcasecmp_P(buffer, p) == 0) return character(c);
      p = p + strlen_P(p) + 1; c++;
    }
    if (index == 3) return character((buffer[0]*10+buffer[1])*10+buffer[2]-5328);
    error2(0, PSTR("unknown character"));
  }

  int x = builtin(buffer);
  if (x == NIL) return nil;
  if (x < ENDFUNCTIONS) return newsymbol(x);
  else if (index <= 6 && valid40(buffer)) return newsymbol(pack40(buffer));
  else return newsymbol(longsymbol(buffer));
}"#

  #"
object *readrest (gfun_t gfun) {
  object *item = nextitem(gfun);
  object *head = NULL;
  object *tail = NULL;

  while (item != (object *)KET) {
    if (item == (object *)BRA) {
      item = readrest(gfun);
    } else if (item == (object *)QUO) {
      item = cons(symbol(QUOTE), cons(read(gfun), NULL));
    } else if (item == (object *)DOT) {
      tail->cdr = read(gfun);
      if (readrest(gfun) != NULL) error2(0, PSTR("malformed list"));
      return head;
    } else {
      object *cell = cons(item, NULL);
      if (head == NULL) head = cell;
      else tail->cdr = cell;
      tail = cell;
      item = nextitem(gfun);
    }
  }
  return head;
}"#

  #"
/*
  read - recursively reads a Lisp object from the stream gfun and returns it
*/
object *read (gfun_t gfun) {
  object *item = nextitem(gfun);
  if (item == (object *)KET) error2(0, PSTR("incomplete list"));
  if (item == (object *)BRA) return readrest(gfun);
  if (item == (object *)DOT) return read(gfun);
  if (item == (object *)QUO) return cons(symbol(QUOTE), cons(read(gfun), NULL));
  return item;
}"#))


(defparameter *setup* '(

#"
// Setup"#

#+avr
#"
/*
  initenv - initialises the uLisp environment
*/
void initenv () {
  GlobalEnv = NULL;
  tee = symbol(TEE);
}

void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  pfstring(PSTR("uLisp 3.5 "), pserial); pln(pserial);
}"#

#+esp
#"
/*
  initgfx - initialises the graphics
*/
void initgfx () {
#if defined(gfxsupport)
  Wire.begin();
  tft.begin(SSD1306_SWITCHCAPVCC, 0x3C);
  tft.fillScreen(COLOR_BLACK);
  tft.display();
#endif
}

/*
  initenv - initialises the uLisp environment
*/
void initenv () {
  GlobalEnv = NULL;
  tee = symbol(TEE);
}

void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  pfstring(PSTR("uLisp 3.5 "), pserial); pln(pserial);
}"#

#+riscv
#"
/*
  initgfx - initialises the graphics
*/
void initgfx () {
#if defined(gfxsupport)
  tft.begin(15000000, COLOR_BLACK);
  tft.setRotation(2);
#endif
}

/*
  initenv - initialises the uLisp environment
*/
void initenv () {
  GlobalEnv = NULL;
  tee = symbol(TEE);
}

void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  pfstring(PSTR("uLisp 3.5 "), pserial); pln(pserial);
}"#

#+arm
#"
/*
  initgfx - initialises the graphics
*/
void initgfx () {
#if defined(gfxsupport)
  tft.initR(INITR_BLACKTAB);
  tft.setRotation(1);
  pinMode(TFT_BACKLIGHT, OUTPUT);
  digitalWrite(TFT_BACKLIGHT, HIGH);
  tft.fillScreen(ST77XX_BLACK);
#endif
}

/*
  initenv - initialises the uLisp environment
*/
void initenv () {
  GlobalEnv = NULL;
  tee = symbol(TEE);
}

void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  pfstring(PSTR("uLisp 3.5 "), pserial); pln(pserial);
}"#))

(defparameter *repl* #"
// Read/Evaluate/Print loop

void repl (object *env) {
  for (;;) {
    randomSeed(micros());
    gc(NULL, env);
    #if defined (printfreespace)
    pint(Freespace, pserial);
    #endif
    if (BreakLevel) {
      pfstring(PSTR(" : "), pserial);
      pint(BreakLevel, pserial);
    }
    pserial('>'); pserial(' ');
    object *line = read(gserial);
    if (BreakLevel && line == nil) { pln(pserial); return; }
    if (line == (object *)KET) error2(0, PSTR("unmatched right bracket"));
    push(line, GCStack);
    pfl(pserial);
    line = eval(line, env);
    pfl(pserial);
    printobject(line, pserial);
    pop(GCStack);
    pfl(pserial);
    pln(pserial);
  }
}"#)

(defparameter *loop* '(

#+avr
#"
void loop () {
  if (!setjmp(exception)) {
    #if defined(resetautorun)
    volatile int autorun = 12; // Fudge to keep code size the same
    #else
    volatile int autorun = 13;
    #endif
    if (autorun == 12) autorunimage();
  }
  // Come here after error
  delay(100); while (Serial.available()) Serial.read();
  clrflag(NOESC); BreakLevel = 0;
  for (int i=0; i<TRACEMAX; i++) TraceDepth[i] = 0;
  #if defined(sdcardsupport)
  SDpfile.close(); SDgfile.close();
  #endif
  #if defined(lisplibrary)
  if (!tstflag(LIBRARYLOADED)) { setflag(LIBRARYLOADED); loadfromlibrary(NULL); }
  #endif
  repl(NULL);
}"#

#+esp
#"
void loop () {
  if (!setjmp(exception)) {
    #if defined(resetautorun)
    volatile int autorun = 12; // Fudge to keep code size the same
    #else
    volatile int autorun = 13;
    #endif
    if (autorun == 12) autorunimage();
  }
  // Come here after error
  delay(100); while (Serial.available()) Serial.read();
  clrflag(NOESC); BreakLevel = 0;
  for (int i=0; i<TRACEMAX; i++) TraceDepth[i] = 0;
  #if defined(sdcardsupport)
  SDpfile.close(); SDgfile.close();
  #endif
  #if defined(lisplibrary)
  if (!tstflag(LIBRARYLOADED)) { setflag(LIBRARYLOADED); loadfromlibrary(NULL); }
  #endif
  client.stop();
  repl(NULL);
}"#

#-(or esp avr)
#"
void loop () {
  if (!setjmp(exception)) {
    #if defined(resetautorun)
    volatile int autorun = 12; // Fudge to keep code size the same
    #else
    volatile int autorun = 13;
    #endif
    if (autorun == 12) autorunimage();
  }
  // Come here after error
  delay(100); while (Serial.available()) Serial.read();
  clrflag(NOESC); BreakLevel = 0;
  for (int i=0; i<TRACEMAX; i++) TraceDepth[i] = 0;
  #if defined(sdcardsupport)
  SDpfile.close(); SDgfile.close();
  #endif
  #if defined(lisplibrary)
  if (!tstflag(LIBRARYLOADED)) { setflag(LIBRARYLOADED); loadfromlibrary(NULL); }
  #endif
  repl(NULL);
}"#))