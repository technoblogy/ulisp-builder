;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; Postscript

(defparameter *table*
 
  '(

    #-avr-nano
    #"
#if !defined(extensions)
// Table cross-reference functions

tbl_entry_t *tables[] = {lookup_table, NULL};
const unsigned int tablesizes[] = { arraysize(lookup_table), 0 };

const tbl_entry_t *table (int n) {
  return tables[n];
}

unsigned int tablesize (int n) {
  return tablesizes[n];
}
#endif"#

#"
// Table lookup functions"#

    #+avr-nano
#"
/*
  lookupbuiltin - looks up a string in lookup_table[], and returns the index of its entry,
  or ENDFUNCTIONS if no match is found
*/
builtin_t lookupbuiltin (char* n) {
  int entries = arraysize(lookup_table);
  for (int entry = 0; entry < entries; entry++) {
    #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
    if (strcasecmp(n, (char*)lookup_table[entry].string) == 0)
    #else
    if (strcasecmp_P(n, (char*)pgm_read_ptr(&lookup_table[entry].string)) == 0)
    #endif
    return (builtin_t)entry;
  }
  return ENDFUNCTIONS;
}"#

    #+(or avr esp)
#"
/*
  lookupbuiltin - looks up a string in lookup_table[], and returns the index of its entry,
  or ENDFUNCTIONS if no match is found
*/
builtin_t lookupbuiltin (char* c) {
  unsigned int end = 0, start;
  for (int n=0; n<2; n++) {
    start = end;
    int entries = tablesize(n);
    end = end + entries;
    for (int i=0; i<entries; i++) {
      if (strcasecmp_P(c, (char*)pgm_read_ptr(&(table(n)[i].string))) == 0) {
      return (builtin_t)(start + i); }
    }
  }
  return ENDFUNCTIONS;
}"#


#+(or arm riscv)
    #"
/*
  lookupbuiltin - looks up a string in lookup_table[], and returns the index of its entry,
  or ENDFUNCTIONS if no match is found
*/
builtin_t lookupbuiltin (char* c) {
  unsigned int end = 0, start;
  for (int n=0; n<2; n++) {
    start = end;
    int entries = tablesize(n);
    end = end + entries;
    for (int i=0; i<entries; i++) {
      if (strcasecmp(c, (char*)(table(n)[i].string)) == 0)
        return (builtin_t)(start + i);
    }
  }
  return ENDFUNCTIONS;
}"#

   #+avr-nano
    #"
/*
  lookupfn - looks up the entry for name in lookup_table[], and returns the function entry point
*/
intptr_t lookupfn (builtin_t name) {
  #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
  return (intptr_t)lookup_table[name].fptr;
  #else
  return (intptr_t)pgm_read_ptr(&lookup_table[name].fptr);
  #endif
}"#

   #+(or avr esp)
    #"
/*
  lookupfn - looks up the entry for name in lookup_table[], and returns the function entry point
*/
intptr_t lookupfn (builtin_t name) {
  int n = name<tablesize(0);
  return (intptr_t)pgm_read_ptr(&table(n?0:1)[n?name:name-tablesize(0)].fptr);
}"#

   #+(or arm riscv)
    #"
/*
  lookupfn - looks up the entry for name in lookup_table[], and returns the function entry point
*/
intptr_t lookupfn (builtin_t name) {
  int n = name<tablesize(0);
  return (intptr_t)table(n?0:1)[n?name:name-tablesize(0)].fptr;
}"#

    #+avr-nano
    #"
/*
  getminmax - gets the minmax byte from lookup_table[] whose octets specify the type of function
  and minimum and maximum number of arguments for name
*/
uint8_t getminmax (builtin_t name) {
  #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
  return lookup_table[name].minmax;
  #else
  return pgm_read_byte(&lookup_table[name].minmax);
  #endif
}"#

    #+(or avr esp)
    #"
/*
  getminmax - gets the minmax byte from lookup_table[] whose octets specify the type of function
  and minimum and maximum number of arguments for name
*/
uint8_t getminmax (builtin_t name) {
  int n = name<tablesize(0);
  return pgm_read_byte(&table(n?0:1)[n?name:name-tablesize(0)].minmax);
}"#

    #+(or arm riscv)
    #"
/*
  getminmax - gets the minmax byte from lookup_table[] whose octets specify the type of function
  and minimum and maximum number of arguments for name
*/
uint8_t getminmax (builtin_t name) {
  int n = name<tablesize(0);
  return table(n?0:1)[n?name:name-tablesize(0)].minmax;
}"#

   #"
/*
  checkminmax - checks that the number of arguments nargs for name is within the range specified by minmax
*/
void checkminmax (builtin_t name, int nargs) {
  if (!(name < ENDFUNCTIONS)) error2(PSTR("not a builtin"));
  uint8_t minmax = getminmax(name);
  if (nargs<((minmax >> 3) & 0x07)) error2(toofewargs);
  if ((minmax & 0x07) != 0x07 && nargs>(minmax & 0x07)) error2(toomanyargs);
}"#

    #+(or avr esp)
    #"
/*
  lookupdoc - looks up the documentation string for the built-in function name
*/
char *lookupdoc (builtin_t name) {
  int n = name<tablesize(0);
  return (char*)pgm_read_ptr(&table(n?0:1)[n?name:name-tablesize(0)].doc);
}"#

    #+(or arm riscv)
    #"
/*
  lookupdoc - looks up the documentation string for the built-in function name
*/
char *lookupdoc (builtin_t name) {
  int n = name<tablesize(0);
  return (char*)table(n?0:1)[n?name:name-tablesize(0)].doc;
}"#

    #+(or avr esp)
    #"
/*
  findsubstring - tests whether a specified substring occurs in the name of a built-in function
*/
bool findsubstring (char *part, builtin_t name) {
  int n = name<tablesize(0);
  PGM_P s = (char*)pgm_read_ptr(&table(n?0:1)[n?name:name-tablesize(0)].string);
  int l = strlen_P(s);
  int m = strlen(part);
  for (int i = 0; i <= l-m; i++) {
    int j = 0;
    while (j < m && pgm_read_byte(&s[i+j]) == part[j]) j++;
    if (j == m) return true;
  }
  return false;
}"#

    #+(or arm riscv)
    #"
/*
  findsubstring - tests whether a specified substring occurs in the name of a built-in function
*/
boolean findsubstring (char *part, builtin_t name) {
  int n = name<tablesize(0);
  return (strstr(table(n?0:1)[n?name:name-tablesize(0)].string, part) != NULL);
}"#

    #-badge
    #"
/*
  testescape - tests whether the '~' escape character has been typed
*/
void testescape () {
  if (Serial.read() == '~') error2(PSTR("escape!"));
}"#

    #+badge
    #"
/*
  testescape - tests whether the '~' escape character has been typed
*/
void testescape () {
  if (Serial.read() == '~') { Context = NIL; error2(PSTR("escape!")); }
}"#

    #+avr-nano
    #"
/*
  keywordp - check that obj is a keyword
*/
bool keywordp (object *obj) {
  if (!(symbolp(obj) && builtinp(obj->name))) return false;
  builtin_t name = builtin(obj->name);
  #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
  PGM_P s = lookup_table[name].string;
  char c = s[0];
  #else
  PGM_P s = (char*)pgm_read_ptr(&lookup_table[name].string);
  char c = pgm_read_byte(&s[0]);
  #endif
  return (c == ':');
}"#

    #+(or avr esp)
    #"
/*
  keywordp - check that obj is a keyword
*/
bool keywordp (object *obj) {
  if (!(symbolp(obj) && builtinp(obj->name))) return false;
  builtin_t name = builtin(obj->name);
  int n = name<tablesize(0);
  PGM_P s = (char*)pgm_read_ptr(&table(n?0:1)[n?name:name-tablesize(0)].string);
  char c = pgm_read_byte(&s[0]);
  return (c == ':');
}"#

    #+(or riscv arm)
    #"
/*
  keywordp - check that obj is a keyword
*/
bool keywordp (object *obj) {
  if (!(symbolp(obj) && builtinp(obj->name))) return false;
  builtin_t name = builtin(obj->name);
  int n = name<tablesize(0);
  PGM_P s = table(n?0:1)[n?name:name-tablesize(0)].string;
  char c = s[0];
  return (c == ':');
}"#))


(defparameter *eval* 
  '(#"
// Main evaluator"#

#+(or avr avr-nano)
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

#+(or avr avr-nano)
#"
/*
  eval - the main Lisp evaluator
*/
object *eval (object *form, object *env) {
  uint8_t sp[0];
  int TC=0;
  EVAL:
  // Enough space?
  //Serial.println((uint16_t)sp - (uint16_t)__bss_end); // Find best STACKDIFF value
  if ((uint16_t)sp - (uint16_t)__bss_end < STACKDIFF) { Context = NIL; error2(PSTR("stack overflow")); }
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);      // GC when 1/16 of workspace left
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(PSTR("escape!"));}
  if (!tstflag(NOESC)) testescape();"#

#+arm
#"
/*
  eval - the main Lisp evaluator
*/
object *eval (object *form, object *env) {
  register int *sp asm ("r13");
  int TC=0;
  EVAL:
  // Enough space?
  // Serial.println((uint32_t)sp - (uint32_t)&ENDSTACK); // Find best STACKDIFF value
  if (((uint32_t)sp - (uint32_t)&ENDSTACK) < STACKDIFF) { Context = NIL; error2(PSTR("stack overflow")); }
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);      // GC when 1/16 of workspace left
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(PSTR("escape!"));}
  if (!tstflag(NOESC)) testescape();"#

#+riscv
#"
/*
  eval - the main Lisp evaluator
*/
object *eval (object *form, object *env) {
  register int *sp asm ("sp");
  int TC=0;
  EVAL:
  // Enough space?
  // Serial.println((uintptr_t)sp - (uintptr_t)end);
  if ((uintptr_t)sp - (uintptr_t)end < STACKDIFF) { Context = NIL; error2(PSTR("stack overflow")); }
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(PSTR("escape!"));}
  if (!tstflag(NOESC)) testescape();"#

#+ignore ; was badge
#"
/*
  eval - the main Lisp evaluator
*/
object *eval (object *form, object *env) {
  uint8_t sp[0];
  int TC=0;
  EVAL:
  // Enough space?
  // Serial.println((uint16_t)sp - (uint16_t)__bss_end); // Find best STACKDIFF value
  if ((uint16_t)sp - (uint16_t)__bss_end < STACKDIFF) { Context = NIL; error2(PSTR("stack overflow")); }
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);      // GC when 1/16 of workspace left
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(PSTR("escape!"));}
  #if defined (serialmonitor)
  if (!tstflag(NOESC)) testescape();
  #endif"#

#+esp
#"
/*
  eval - the main Lisp evaluator
*/
object *eval (object *form, object *env) {
  static unsigned long start = 0;
  int TC=0;
  EVAL:
#if defined(ESP8266)
  (void) start;
  yield();  // Needed on ESP8266 to avoid Soft WDT Reset
#elif defined(ARDUINO_ESP32C3_DEV)
  if (millis() - start > 4000) { delay(1); start = millis(); }
#else
  (void) start;
#endif
  // Enough space?
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(PSTR("escape!"));}
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
    else if (builtinp(name)) return form;
    Context = NIL;
    error(PSTR("undefined"), form);
  }"#

#+(or avr avr-nano arm riscv)
#"
  #if defined(CODESIZE)
  if (form->type == CODE) error2(PSTR("can't evaluate CODE header"));
  #endif"#
  
#"
  // It's a list
  object *function = car(form);
  object *args = cdr(form);

  if (function == NULL) error(PSTR("illegal function"), nil);
  if (!listp(args)) error(PSTR("can't evaluate a dotted pair"), args);

  // List starts with a builtin symbol?
  if (symbolp(function) && builtinp(function->name)) {
    builtin_t name = builtin(function->name);

    if ((name == LET) || (name == LETSTAR)) {
      int TCstart = TC;
      if (args == NULL) error2(noargument);
      object *assigns = first(args);
      if (!listp(assigns)) error(notalist, assigns);
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
      return cons(bsymbol(CLOSURE), cons(envcopy,args));
    }
    uint8_t fntype = getminmax(name)>>6;

    if (fntype == SPECIAL_FORMS) {
      Context = name;
      return ((fn_ptr_type)lookupfn(name))(args, env);
    }

    if (fntype == TAIL_FORMS) {
      Context = name;
      form = ((fn_ptr_type)lookupfn(name))(args, env);
      TC = 1;
      goto EVAL;
    }
    if (fntype == OTHER_FORMS) error(PSTR("can't be used as a function"), function);
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
    builtin_t bname = builtin(function->name);
    if (!builtinp(function->name)) error(PSTR("not valid here"), fname);
    Context = bname;
    checkminmax(bname, nargs);
    object *result = ((fn_ptr_type)lookupfn(bname))(args, env);
    pop(GCStack);
    return result;
  }

  if (consp(function)) {
    symbol_t name = sym(NIL);
    if (!listp(fname)) name = fname->name;

    if (isbuiltin(car(function), LAMBDA)) {
      form = closure(TCstart, name, function, args, &env);
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

    if (isbuiltin(car(function), CLOSURE)) {
      function = cdr(function);
      form = closure(TCstart, name, function, args, &env);
      pop(GCStack);
      TC = 1;
      goto EVAL;
    }"#

#+avr
#"
    #if defined(CODESIZE)
    if (car(function)->type == CODE) {
      int n = listlength(second(function));
      if (nargs<n) errorsym2(fname->name, toofewargs);
      if (nargs>n) errorsym2(fname->name, toomanyargs);
      uint32_t entry = startblock(car(function));
      pop(GCStack);
      return call(entry, n, args, env);
    }
    #endif"#

#+arm
#"
    if (car(function)->type == CODE) {
      int n = listlength(second(function));
      if (nargs<n) errorsym2(fname->name, toofewargs);
      if (nargs>n) errorsym2(fname->name, toomanyargs);
      uint32_t entry = startblock(car(function)) + 1;
      pop(GCStack);
      return call(entry, n, args, env);
    }"#

#+riscv
#"
    if (car(function)->type == CODE) {
      int n = listlength(second(function));
      if (nargs<n) errorsym2(fname->name, toofewargs);
      if (nargs>n) errorsym2(fname->name, toomanyargs);
      uint32_t entry = startblock(car(function));
      pop(GCStack);
      return call(entry, n, args, env);
    }"#

#"
  }
  error(PSTR("illegal function"), fname); return nil;
}"#))

(defparameter *print-functions* 

  '(#"
// Print functions"#

  #-badge
  #"
/*
  pserial - prints a character to the serial port
*/
void pserial (char c) {
  LastPrint = c;
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
}"#


  #+badge
  #"
/*
  pserial - prints a character to the serial port
*/
void pserial (char c) {
  LastPrint = c;
  Display(c);
  #if defined (serialmonitor)
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
  #endif
}"#

    #+(or avr avr-nano)
    #"
const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

/*
  pcharacter - prints a character to a stream, escaping special characters if PRINTREADABLY is false
  If <= 32 prints character name; eg #\Space
  If < 127 prints ASCII; eg #\A
  Otherwise prints decimal; eg #\234
*/
void pcharacter (uint8_t c, pfun_t pfun) {
  if (!tstflag(PRINTREADABLY)) pfun(c);
  else {
    pfun('#'); pfun('\\');
    if (c <= 32) {
      PGM_P p = ControlCodes;
      #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
      while (c > 0) {p = p + strlen(p) + 1; c--; }
      #else
      while (c > 0) {p = p + strlen_P(p) + 1; c--; }
      #endif
      pfstring(p, pfun);
    } else if (c < 127) pfun(c);
    else pint(c, pfun);
  }
}"#

    #+ignore ; was badge
    #"
const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

/*
  pcharacter - prints a character to a stream, escaping special characters if PRINTREADABLY is false
  If <= 32 prints character name; eg #\Space
  If < 127 prints ASCII; eg #\A
  Otherwise prints decimal; eg #\234
*/
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

    #+esp
    #"
const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

/*
  pcharacter - prints a character to a stream, escaping special characters if PRINTREADABLY is false
  If <= 32 prints character name; eg #\Space
  If < 127 prints ASCII; eg #\A
  Otherwise prints decimal; eg #\234
*/
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

    #+(or arm riscv)
    #"
const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

/*
  pcharacter - prints a character to a stream, escaping special characters if PRINTREADABLY is false
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
  pstring - prints a C string to the specified stream
*/
void pstring (char *s, pfun_t pfun) {
  while (*s) pfun(*s++);
}"#

    #"
/*
  plispstring - prints a Lisp string object to the specified stream
*/
void plispstring (object *form, pfun_t pfun) {
  plispstr(form->name, pfun);
}

/*
  plispstr - prints a Lisp string name to the specified stream
*/
void plispstr (symbol_t name, pfun_t pfun) {
  object *form = (object *)name;
  while (form != NULL) {
    int chars = form->chars;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (tstflag(PRINTREADABLY) && (ch == '"' || ch == '\\')) pfun('\\');
      if (ch) pfun(ch);
    }
    form = car(form);
  }
}

/*
  printstring - prints a Lisp string object to the specified stream
  taking account of the PRINTREADABLY flag
*/
void printstring (object *form, pfun_t pfun) {
  if (tstflag(PRINTREADABLY)) pfun('"');
  plispstr(form->name, pfun);
  if (tstflag(PRINTREADABLY)) pfun('"');
}"#

  #+avr-nano
  #"
/*
  pbuiltin - prints a built-in symbol to the specified stream
*/
void pbuiltin (builtin_t name, pfun_t pfun) {
  int p = 0;
  #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
  PGM_P s = lookup_table[name].string;
  #else
  PGM_P s = (char*)pgm_read_ptr(&lookup_table[name].string);
  #endif
  while (1) {
    #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
    char c = s[p++];
    #else
    char c = pgm_read_byte(&s[p++]);
    #endif
    if (c == 0) return;
    pfun(c);
  }
}"#

  #+(or avr esp)
  #"
/*
  pbuiltin - prints a built-in symbol to the specified stream
*/
void pbuiltin (builtin_t name, pfun_t pfun) {
  int p = 0;
  int n = name<tablesize(0);
  PGM_P s = (char*)pgm_read_ptr(&table(n?0:1)[n?name:name-tablesize(0)].string); 
  while (1) {
    char c = pgm_read_byte(&s[p++]);
    if (c == 0) return;
    pfun(c);
  }
}"#

  #+(or arm riscv)
  #"
/*
  pbuiltin - prints a built-in symbol to the specified stream
*/
void pbuiltin (builtin_t name, pfun_t pfun) {
  int p = 0;
  int n = name<tablesize(0);
  PGM_P s = table(n?0:1)[n?name:name-tablesize(0)].string;
  while (1) {
    char c = s[p++];
    if (c == 0) return;
    pfun(c);
  }
}"#

  #+(or avr avr-nano)
  #"
/*
  pradix40 - prints a radix 40 symbol to the specified stream
*/
void pradix40 (symbol_t name, pfun_t pfun) {
  uint16_t x = untwist(name);
  for (int d=1600; d>0; d = d/40) {
    uint16_t j = x/d;
    char c = fromradix40(j);
    if (c == 0) return;
    pfun(c); x = x - j*d;
  }
}"#

  #+(or arm esp riscv)
  #"
/*
  pradix40 - prints a radix 40 symbol to the specified stream
*/
void pradix40 (symbol_t name, pfun_t pfun) {
  uint32_t x = untwist(name);
  for (int d=102400000; d>0; d = d/40) {
    uint32_t j = x/d;
    char c = fromradix40(j);
    if (c == 0) return;
    pfun(c); x = x - j*d;
  }
}"#

  #"
/*
  printsymbol - prints any symbol from a symbol object to the specified stream
*/
void printsymbol (object *form, pfun_t pfun) {
  psymbol(form->name, pfun);
}"#

  #+(or avr avr-nano)
  #"
/*
  psymbol - prints any symbol from a symbol name to the specified stream
*/
void psymbol (symbol_t name, pfun_t pfun) {
  if ((name & 0x03) == 0) plispstr(name, pfun);
  else {
    uint16_t value = untwist(name);
    if (value < PACKEDS) error2(PSTR("invalid symbol"));
    else if (value >= BUILTINS) pbuiltin((builtin_t)(value-BUILTINS), pfun);
    else pradix40(name, pfun);
  }
}"#

  #+(or arm esp riscv)
  #"
/*
  psymbol - prints any symbol from a symbol name to the specified stream
*/
void psymbol (symbol_t name, pfun_t pfun) {
  if ((name & 0x03) == 0) plispstr(name, pfun);
  else {
    uint32_t value = untwist(name);
    if (value < PACKEDS) error2(PSTR("invalid symbol"));
    else if (value >= BUILTINS) pbuiltin((builtin_t)(value-BUILTINS), pfun);
    else pradix40(name, pfun);
  }
}"#

  #+avr-nano
  #"
/*
  pfstring - prints a string from flash memory to the specified stream
*/
void pfstring (PGM_P s, pfun_t pfun) {
  int p = 0;
  while (1) {
    #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
    char c = s[p++];
    #else
    char c = pgm_read_byte(&s[p++]);
    #endif
    if (c == 0) return;
    pfun(c);
  }
}"#

  #+avr
  #"
/*
  pfstring - prints a string from flash memory to the specified stream
*/
void pfstring (PGM_P s, pfun_t pfun) {
  int p = 0;
  while (1) {
    char c = pgm_read_byte(&s[p++]);
    if (c == 0) return;
    pfun(c);
  }
}"#

    #+(or arm riscv)
    #"
/*
  pfstring - prints a string from flash memory to the specified stream
*/
void pfstring (const char *s, pfun_t pfun) {
  int p = 0;
  while (1) {
    char c = s[p++];
    if (c == 0) return;
    pfun(c);
  }
}"#

#+msp430
    #"
/*
  pfstring - prints a string from flash memory to the specified stream
*/
void pfstring (PGM_P s, pfun_t pfun) {
  intptr_t p = (intptr_t)s;
  while (1) {
    char c = pgm_read_byte(p++);
    if (c == 0) return;
    pfun(c);
  }
}"#

    #+esp
    #"
/*
  pfstring - prints a string from flash memory to the specified stream
*/
void pfstring (PGM_P s, pfun_t pfun) {
  int p = 0;
  while (1) {
    char c = pgm_read_byte(&s[p++]);
    if (c == 0) return;
    pfun(c);
  }
}"#

  #+(or avr avr-nano)
  #"
/*
  pint - prints an integer in decimal to the specified stream
*/
void pint (int i, pfun_t pfun) {
  uint16_t j = i;
  if (i<0) { pfun('-'); j=-i; }
  pintbase(j, 10, pfun);
}

/*
  pintbase - prints an integer in base 'base' to the specified stream
*/
void pintbase (uint16_t i, uint8_t base, pfun_t pfun) {
  uint8_t lead = 0; uint16_t p = 10000;
  if (base == 2) p = 0x8000; else if (base == 16) p = 0x1000;
  for (uint16_t d=p; d>0; d=d/base) {
    uint16_t j = i/d;
    if (j!=0 || lead || d==1) { pfun((j<10) ? j+'0' : j+'W'); lead=1;}
    i = i - j*d;
  }
}"#

  #+(or arm esp riscv)
  #"
/*
  pint - prints an integer in decimal to the specified stream
*/
void pint (int i, pfun_t pfun) {
  uint32_t j = i;
  if (i<0) { pfun('-'); j=-i; }
  pintbase(j, 10, pfun);
}

/*
  pintbase - prints an integer in base 'base' to the specified stream
*/
void pintbase (uint32_t i, uint8_t base, pfun_t pfun) {
  int lead = 0; uint32_t p = 1000000000;
  if (base == 2) p = 0x80000000; else if (base == 16) p = 0x10000000;
  for (uint32_t d=p; d>0; d=d/base) {
    uint32_t j = i/d;
    if (j!=0 || lead || d==1) { pfun((j<10) ? j+'0' : j+'W'); lead=1;}
    i = i - j*d;
  }
}"#

    #+(or avr avr-nano)
    #"
/*
  pinthex2 - prints a two-digit hexadecimal number with leading zeros to the specified stream
*/
void printhex2 (int i, pfun_t pfun) {
  for (unsigned int d=0x10; d>0; d=d>>4) {
    unsigned int j = i/d;
    pfun((j<10) ? j+'0' : j+'W'); 
    i = i - j*d;
  }
}"#

    #+(or riscv arm)
    #"
/*
  pinthex4 - prints a four-digit hexadecimal number with leading zeros to the specified stream
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
/*
  pmantissa - prints the mantissa of a floating-point number to the specified stream
*/
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

/*
  pfloat - prints a floating-point number to the specified stream
*/
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
/*
  pln - prints a newline to the specified stream
*/
inline void pln (pfun_t pfun) {
  pfun('\n');
}"#

    #"
/*
  pfl - prints a newline to the specified stream if a newline has not just been printed
*/
void pfl (pfun_t pfun) {
  if (LastPrint != '\n') pfun('\n');
}"#

    #"
/*
  plist - prints a list to the specified stream
*/
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

    #+avr-nano
    #"
/*
  pstream - prints a stream name to the specified stream
*/
void pstream (object *form, pfun_t pfun) {
  pfun('<');
  #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
  PGM_P s = streamname[(form->integer)>>8];
  #else
  PGM_P s = (char*)pgm_read_ptr(&streamname[(form->integer)>>8]);
  #endif
  pfstring(s, pfun);
  pfstring(PSTR("-stream "), pfun);
  pint(form->integer & 0xFF, pfun);
  pfun('>');
}"#

    #+(or avr esp)
    #"
/*
  pstream - prints a stream name to the specified stream
*/
void pstream (object *form, pfun_t pfun) {
  pfun('<');
  PGM_P s = (char*)pgm_read_ptr(&streamname[(form->integer)>>8]);
  pfstring(s, pfun);
  pfstring(PSTR("-stream "), pfun);
  pint(form->integer & 0xFF, pfun);
  pfun('>');
}"#

    #+(or arm riscv)
    #"
/*
  pstream - prints a stream name to the specified stream
*/
void pstream (object *form, pfun_t pfun) {
  pfun('<');
  pfstring(streamname[(form->integer)>>8], pfun);
  pfstring(PSTR("-stream "), pfun);
  pint(form->integer & 0xFF, pfun);
  pfun('>');
}"#

   #+avr-nano
   #"
/*
  printobject - prints any Lisp object to the specified stream
*/
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && isbuiltin(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) plist(form, pfun);
  else if (integerp(form)) pint(form->integer, pfun);
  else if (symbolp(form)) { if (form->name != sym(NOTHING)) printsymbol(form, pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  #if defined(CODESIZE)
  else if (form->type == CODE) pfstring(PSTR("code"), pfun);
  #endif
  else if (streamp(form)) pstream(form, pfun);
  else error2(PSTR("error in print"));
}"#

   #+avr
   #"
/*
  printobject - prints any Lisp object to the specified stream
*/
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && isbuiltin(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) plist(form, pfun);
  else if (integerp(form)) pint(form->integer, pfun);
  else if (symbolp(form)) { if (form->name != sym(NOTHING)) printsymbol(form, pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (arrayp(form)) printarray(form, pfun);
  #if defined(CODESIZE)
  else if (form->type == CODE) pfstring(PSTR("code"), pfun);
  #endif
  else if (streamp(form)) pstream(form, pfun);
  else error2(PSTR("error in print"));
}"#

   #+(or arm riscv)
   #"
/*
  printobject - prints any Lisp object to the specified stream
*/
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && isbuiltin(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) plist(form, pfun);
  else if (integerp(form)) pint(form->integer, pfun);
  else if (floatp(form)) pfloat(form->single_float, pfun);
  else if (symbolp(form)) { if (form->name != sym(NOTHING)) printsymbol(form, pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (arrayp(form)) printarray(form, pfun);
  else if (form->type == CODE) pfstring(PSTR("code"), pfun);
  else if (streamp(form)) pstream(form, pfun);
  else error2(PSTR("error in print"));
}"#

; Has LCDSTREAM
    #+msp430
    #"
/*
  printobject - prints any Lisp object to the specified stream
*/
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
  } else error2(PSTR("error in print"));
}"#

    #+esp
    #"
/*
  printobject - prints any Lisp object to the specified stream
*/
void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && isbuiltin(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) plist(form, pfun);
  else if (integerp(form)) pint(form->integer, pfun);
  else if (floatp(form)) pfloat(form->single_float, pfun);
  else if (symbolp(form)) { if (form->name != sym(NOTHING)) printsymbol(form, pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (arrayp(form)) printarray(form, pfun);
  else if (streamp(form)) pstream(form, pfun);
  else error2(PSTR("error in print"));
}"#

#"
/*
  prin1object - prints any Lisp object to the specified stream escaping special characters
*/
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

    #+(or avr avr-nano)
    #"
// Read functions

/*
  glibrary - reads a character from the Lisp Library
*/
int glibrary () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
  char c = LispLibrary[GlobalStringIndex++];
  #else
  char c = pgm_read_byte(&LispLibrary[GlobalStringIndex++]);
  #endif
  return (c != 0) ? c : -1; // -1?
}

/*
  loadfromlibrary - reads and evaluates a form from the Lisp Library
*/
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

/*
  glibrary - reads a character from the Lisp Library
*/
int glibrary () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = LispLibrary[GlobalStringIndex++];
  return (c != 0) ? c : -1; // -1?
}

/*
  loadfromlibrary - reads and evaluates a form from the Lisp Library
*/
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

/*
  glibrary - reads a character from the Lisp Library
*/
int glibrary () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = pgm_read_byte(&LispLibrary[GlobalStringIndex++]);
  return (c != 0) ? c : -1; // -1?
}

/*
  loadfromlibrary - reads and evaluates a form from the Lisp Library
*/
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

/*
  glibrary - reads a character from the Lisp Library
*/
int glibrary () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = pgm_read_byte(&LispLibrary[GlobalStringIndex++]);
  return (c != 0) ? c : -1; // -1?
}

/*
  loadfromlibrary - reads and evaluates a form from the Lisp Library
*/
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

/*
  Highlight - handles parenthesis highlighting with the line editor
*/
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

/*
  processkey - handles keys in the line editor
*/
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
}"#

 #+(and (or avr avr-nano) (not badge))
 #"
/*
  gserial - gets a character from the serial port
*/
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
#elif defined(CPU_ATmega328P) || defined(CPU_ATtiny3227)
  while (!Serial.available());
  char temp = Serial.read();
  if (temp != '\n') pserial(temp);
  return temp;
#else
  unsigned long start = millis();
  while (!Serial.available()) if (millis() - start > 1000) clrflag(NOECHO);
  char temp = Serial.read();
  if (temp != '\n' && !tstflag(NOECHO)) pserial(temp);
  return temp;
#endif
}"#

 #-(or avr avr-nano badge esp)
 #"
/*
  gserial - gets a character from the serial port
*/
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
  unsigned long start = millis();
  while (!Serial.available()) if (millis() - start > 1000) clrflag(NOECHO);
  char temp = Serial.read();
  if (temp != '\n' && !tstflag(NOECHO)) pserial(temp);
  return temp;
#endif
}"#

 #+esp
 #"
/*
  gserial - gets a character from the serial port
*/
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
  unsigned long start = millis();
  while (!Serial.available()) { delay(1); if (millis() - start > 1000) clrflag(NOECHO); }
  char temp = Serial.read();
  if (temp != '\n' && !tstflag(NOECHO)) pserial(temp);
  return temp;
#endif
}"#

  #+badge
  #"
/*
  gserial - gets a character from the serial port
*/
int gserial () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  #if defined (serialmonitor)
  unsigned long start = millis();
  while (!Serial.available() && !KybdAvailable) if (millis() - start > 1000) clrflag(NOECHO);
  if (Serial.available()) {
    char temp = Serial.read();
    if (temp != '\n' && !tstflag(NOECHO)) Serial.print(temp); // Don't print on Lisp Badge
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

  #+avr-nano
  #"
/*
  nextitem - reads the next token from the specified stream
*/
object *nextitem (gfun_t gfun) {
  int ch = gfun();
  while(issp(ch)) ch = gfun();

  #if defined(CPU_ATmega328P) || defined(CPU_ATtiny3227)
  if (ch == ';') {
    while(ch != '(') ch = gfun();
  }
  #else
  if (ch == ';') {
    do { ch = gfun(); if (ch == ';' || ch == '(') setflag(NOECHO); }
    while(ch != '(');
  }
  #endif
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
  char buffer[BUFFERSIZE];
  int bufmax = BUFFERSIZE-1; // Max index
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
      if (issp(ch) || isbr(ch)) return character(ch);
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
    } else error2(PSTR("illegal character after #"));
    ch = gfun();
  }

  int isnumber = (digitvalue(ch)<base);
  buffer[2] = '\0'; // In case symbol is one letter

  while(!issp(ch) && !isbr(ch) && index < bufmax) {
    buffer[index++] = ch;
    int temp = digitvalue(ch);
    result = result * base + temp;
    isnumber = isnumber && (digitvalue(ch)<base);
    ch = gfun();
  }"#

  #+avr
  #"
/*
  nextitem - reads the next token from the specified stream
*/
object *nextitem (gfun_t gfun) {
  int ch = gfun();
  while(issp(ch)) ch = gfun();

  #if defined(CPU_ATmega328P)
  if (ch == ';') {
    while(ch != '(') ch = gfun();
  }
  #else
  if (ch == ';') {
    do { ch = gfun(); if (ch == ';' || ch == '(') setflag(NOECHO); }
    while(ch != '(');
  }
  #endif
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
  char buffer[BUFFERSIZE];
  int bufmax = BUFFERSIZE-1; // Max index
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
      if (issp(ch) || isbr(ch)) return character(ch);
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
    else error2(PSTR("illegal character after #"));
    ch = gfun();
  }

  int isnumber = (digitvalue(ch)<base);

  while(!issp(ch) && !isbr(ch) && index < bufmax) {
    buffer[index++] = ch;
    int temp = digitvalue(ch);
    result = result * base + temp;
    isnumber = isnumber && (digitvalue(ch)<base);
    ch = gfun();
  }"#

  #+(or arm esp riscv)
  #"
/*
  nextitem - reads the next token from the specified stream
*/
object *nextitem (gfun_t gfun) {
  int ch = gfun();
  while(issp(ch)) ch = gfun();

  if (ch == ';') {
    do { ch = gfun(); if (ch == ';' || ch == '(') setflag(NOECHO); }
    while(ch != '(');
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
  char buffer[BUFFERSIZE];
  int bufmax = BUFFERSIZE-3; // Max index
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
      if (issp(ch) || isbr(ch)) return character(ch);
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
    else error2(PSTR("illegal character after #"));
    ch = gfun();
  }
  int valid; // 0=undecided, -1=invalid, +1=valid
  if (ch == '.') valid = 0; else if (digitvalue(ch)<base) valid = 1; else valid = -1;
  bool isexponent = false;
  int exponent = 0, esign = 1;
  float divisor = 10.0;

  while(!issp(ch) && !isbr(ch) && index < bufmax) {
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

  #+(or avr avr-nano)
  #"
  buffer[index] = '\0';
  if (isbr(ch)) LastChar = ch;

  if (isnumber) {
    if (base == 10 && result > ((unsigned int)INT_MAX+(1-sign)/2)) 
      error2(PSTR("Number out of range"));
    return number(result*sign);
  } else if (base == 0) {
    if (index == 1) return character(buffer[0]);
    PGM_P p = ControlCodes; char c = 0;
    while (c < 33) {
      #if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
      if (strcasecmp(buffer, p) == 0) return character(c);
      p = p + strlen(p) + 1; c++;
      #else
      if (strcasecmp_P(buffer, p) == 0) return character(c);
      p = p + strlen_P(p) + 1; c++;
      #endif
    }
    if (index == 3) return character((buffer[0]*10+buffer[1])*10+buffer[2]-5328);
    error2(PSTR("unknown character"));
  }"#
  
  #+avr-nano
  #"
  builtin_t x = lookupbuiltin(buffer);
  if (x == NIL) return nil;
  if (x != ENDFUNCTIONS) return bsymbol(x);
  if (index <= 3 && valid40(buffer)) return intern(twist(pack40(buffer)));
  buffer[index+1] = '\0'; // For internlong
  return internlong(buffer);
}"#

  #+avr
  #"
  builtin_t x = lookupbuiltin(buffer);
  if (x == NIL) return nil;
  if (x != ENDFUNCTIONS) return bsymbol(x);
  if (index <= 3 && valid40(buffer)) return intern(twist(pack40(buffer)));
  return internlong(buffer);
}"#

  #+(or arm riscv)
  #"
  buffer[index] = '\0';
  if (isbr(ch)) LastChar = ch;
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
    error2(PSTR("unknown character"));
  }

  builtin_t x = lookupbuiltin(buffer);
  if (x == NIL) return nil;
  if (x != ENDFUNCTIONS) return bsymbol(x);
  if (index <= 6 && valid40(buffer)) return intern(twist(pack40(buffer)));
  return internlong(buffer);
}"#

  #+esp
  #"
  buffer[index] = '\0';
  if (isbr(ch)) LastChar = ch;
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
    error2(PSTR("unknown character"));
  }

  builtin_t x = lookupbuiltin(buffer);
  if (x == NIL) return nil;
  if (x != ENDFUNCTIONS) return bsymbol(x);
  if (index <= 6 && valid40(buffer)) return intern(twist(pack40(buffer)));
  return internlong(buffer);
}"#

  #+msp430
  #"
  buffer[index] = '\0';
  if (isbr(ch)) LastChar = ch;

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

  #"
/*
  readrest - reads the remaining tokens from the specified stream
*/
object *readrest (gfun_t gfun) {
  object *item = nextitem(gfun);
  object *head = NULL;
  object *tail = NULL;

  while (item != (object *)KET) {
    if (item == (object *)BRA) {
      item = readrest(gfun);
    } else if (item == (object *)QUO) {
      item = cons(bsymbol(QUOTE), cons(read(gfun), NULL));
    } else if (item == (object *)DOT) {
      tail->cdr = read(gfun);
      if (readrest(gfun) != NULL) error2(PSTR("malformed list"));
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
  if (item == (object *)KET) error2(PSTR("incomplete list"));
  if (item == (object *)BRA) return readrest(gfun);
  if (item == (object *)DOT) return read(gfun);
  if (item == (object *)QUO) return cons(bsymbol(QUOTE), cons(read(gfun), NULL));
  return item;
}"#))


(defparameter *setup1* '(

#"
// Setup"#

#"
/*
  initenv - initialises the uLisp environment
*/
void initenv () {
  GlobalEnv = NULL;
  tee = bsymbol(TEE);
}"#

#+arm
#"
/*
  initgfx - initialises the graphics
*/
void initgfx () {
  #if defined(gfxsupport)
  #if defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
  tft.initR(INITR_BLACKTAB);
  tft.setRotation(1);
  pinMode(TFT_BACKLIGHT, OUTPUT);
  digitalWrite(TFT_BACKLIGHT, HIGH);
  tft.fillScreen(0);
  #elif defined(ARDUINO_WIO_TERMINAL)
  tft.init();
  tft.setRotation(3);
  tft.fillScreen(TFT_BLACK);
  #elif defined(ARDUINO_NRF52840_CLUE)
  tft.init(240, 240);
  tft.setRotation(1);
  tft.fillScreen(0);
  pinMode(34, OUTPUT); // Backlight
  digitalWrite(34, HIGH);
  #endif
  #endif
}"#

#+esp
#"
/*
  initgfx - initialises the graphics
*/
void initgfx () {
  #if defined(gfxsupport)
  tft.init(135, 240);
  #if defined(ARDUINO_ADAFRUIT_FEATHER_ESP32S2_TFT)
  pinMode(TFT_I2C_POWER, OUTPUT);
  digitalWrite(TFT_I2C_POWER, HIGH);
  tft.setRotation(3);
  #else
  tft.setRotation(1);
  #endif
  tft.fillScreen(ST77XX_BLACK);
  pinMode(TFT_BACKLITE, OUTPUT);
  digitalWrite(TFT_BACKLITE, HIGH);
  #endif
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
}"#))

#+(and (or avr avr-nano) (not badge))
(defparameter *setup2* #"
// Entry point from the Arduino IDE
void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  pfstring(PSTR("uLisp ~a "), pserial); pln(pserial);
}"#)

#+badge
(defparameter *setup2* #"
// Entry point from the Arduino IDE
void setup () {
  InitDisplay();
  InitKybd();
  #if defined (serialmonitor)
  pinMode(8, INPUT_PULLUP); // RX0
  Serial.begin(9600);
  int start = millis();
  while (millis() - start < 5000) { if (Serial) break; }
  #endif
  initworkspace();
  initenv();
  initsleep();
  pfstring(PSTR("uLisp ~a "), pserial); pln(pserial);
}"#)

#+arm
(defparameter *setup2* #"
// Entry point from the Arduino IDE
void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  pfstring(PSTR("uLisp ~a "), pserial); pln(pserial);
}"#)

#+esp
(defparameter *setup2* #"
// Entry point from the Arduino IDE
void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  pfstring(PSTR("uLisp ~a "), pserial); pln(pserial);
}"#)

#+riscv
(defparameter *setup2* #"
// Entry point from the Arduino IDE
void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  pfstring(PSTR("uLisp ~a "), pserial); pln(pserial);
}"#)

(defparameter *repl* '(

#"
// Read/Evaluate/Print loop"#

 #+(or avr avr-nano)
 #"
/*
  repl - the Lisp Read/Evaluate/Print loop
*/
void repl (object *env) {
  for (;;) {
    RandomSeed = micros();
    gc(NULL, env);
    #if defined(printfreespace)
    pint(Freespace, pserial);
    #endif
    if (BreakLevel) {
      pfstring(PSTR(" : "), pserial);
      pint(BreakLevel, pserial);
    }
    pserial('>'); pserial(' ');
    Context = NIL;
    object *line = read(gserial);
    if (BreakLevel && line == nil) { pln(pserial); return; }
    if (line == (object *)KET) error2(PSTR("unmatched right bracket"));
    push(line, GCStack);
    pfl(pserial);
    line = eval(line, env);
    pfl(pserial);
    printobject(line, pserial);
    pop(GCStack);
    pfl(pserial);
    pln(pserial);
  }
}"#

#+arm
#"
/*
  repl - the Lisp Read/Evaluate/Print loop
*/
void repl (object *env) {
  for (;;) {
    randomSeed(micros());
    gc(NULL, env);
    #if defined(printfreespace)
    pint(Freespace, pserial);
    #endif
    if (BreakLevel) {
      pfstring(PSTR(" : "), pserial);
      pint(BreakLevel, pserial);
    }
    pserial('>'); pserial(' ');
    Context = NIL;
    object *line = read(gserial);
    #if defined(CPU_NRF52840)
    Serial.flush();
    #endif
    if (BreakLevel && line == nil) { pln(pserial); return; }
    if (line == (object *)KET) error2(PSTR("unmatched right bracket"));
    push(line, GCStack);
    pfl(pserial);
    line = eval(line, env);
    pfl(pserial);
    printobject(line, pserial);
    pop(GCStack);
    pfl(pserial);
    pln(pserial);
  }
}"#

#+(or esp riscv)
#"
/*
  repl - the Lisp Read/Evaluate/Print loop
*/
void repl (object *env) {
  for (;;) {
    randomSeed(micros());
    gc(NULL, env);
    #if defined(printfreespace)
    pint(Freespace, pserial);
    #endif
    if (BreakLevel) {
      pfstring(PSTR(" : "), pserial);
      pint(BreakLevel, pserial);
    }
    pserial('>'); pserial(' ');
    Context = NIL;
    object *line = read(gserial);
    if (BreakLevel && line == nil) { pln(pserial); return; }
    if (line == (object *)KET) error2(PSTR("unmatched right bracket"));
    push(line, GCStack);
    pfl(pserial);
    line = eval(line, env);
    pfl(pserial);
    printobject(line, pserial);
    pop(GCStack);
    pfl(pserial);
    pln(pserial);
  }
}"#))

(defparameter *loop* '(

#-errors
#"
/*
  loop - the Arduino IDE main execution loop
*/
void loop () {
  if (!setjmp(exception)) {
    #if defined(resetautorun)
    volatile int autorun = 12; // Fudge to keep code size the same
    #else
    volatile int autorun = 13;
    #endif
    if (autorun == 12) autorunimage();
  }
  ulispreset();
  repl(NULL);
}"#

#+errors
#"
/*
  loop - the Arduino IDE main execution loop
*/
void loop () {
  if (!setjmp(toplevel_handler)) {
    #if defined(resetautorun)
    volatile int autorun = 12; // Fudge to keep code size the same
    #else
    volatile int autorun = 13;
    #endif
    if (autorun == 12) autorunimage();
  }
  ulispreset();
  repl(NULL);
}"#

#-wifi
#"
void ulispreset () {
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
}"#

#+(and wifi arm)
#"
void ulispreset () {
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
  #if defined(ULISP_WIFI)
  client.stop();
  #endif
}"#

#+(and wifi (not arm))
#"
void ulispreset () {
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
}"#))