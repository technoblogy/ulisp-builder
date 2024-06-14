;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

(defparameter *error-handling* '(

#"
// Error handling

/*
  errorsub - used by all the error routines.
  Prints: "Error: 'fname' string", where fname is the name of the Lisp function in which the error occurred.
*/
void errorsub (symbol_t fname, PGM_P string) {
  pfl(pserial); pfstring(PSTR("Error: "), pserial);
  if (fname != sym(NIL)) {
    pserial('\'');
    psymbol(fname, pserial);
    pserial('\''); pserial(' ');
  }
  pfstring(string, pserial);
}"#

#-errors
#"
void errorend () { pln(pserial); GCStack = NULL; longjmp(exception, 1); }"#

#+errors
#"
void errorend () { GCStack = NULL; longjmp(*handler, 1); }"#

#-errors
#"
/*
  errorsym - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string: symbol", where fname is the name of the user Lisp function in which the error occurred,
  and symbol is the object generating the error.
*/
void errorsym (symbol_t fname, PGM_P string, object *symbol) {
  errorsub(fname, string);
  pserial(':'); pserial(' ');
  printobject(symbol, pserial);
  errorend();
}

/*
  errorsym2 - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string", where fname is the name of the user Lisp function in which the error occurred.
*/
void errorsym2 (symbol_t fname, PGM_P string) {
  errorsub(fname, string);
  errorend();
}"#

#+errors
#"
/*
  errorsym - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string: symbol", where fname is the name of the user Lisp function in which the error occurred,
  and symbol is the object generating the error.
*/
void errorsym (symbol_t fname, PGM_P string, object *symbol) {
  if (!tstflag(MUFFLEERRORS)) {
    errorsub(fname, string);
    pserial(':'); pserial(' ');
    printobject(symbol, pserial);
    pln(pserial);
  }
  errorend();
}

/*
  errorsym2 - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string", where fname is the name of the user Lisp function in which the error occurred.
*/
void errorsym2 (symbol_t fname, PGM_P string) {
  if (!tstflag(MUFFLEERRORS)) {
    errorsub(fname, string);
    pln(pserial);
  }
  errorend();
}"#

#"
/*
  error - prints an error message and reenters the REPL.
  Prints: "Error: 'Context' string: symbol", where Context is the name of the built-in Lisp function in which the error occurred,
  and symbol is the object generating the error.
*/
void error (PGM_P string, object *symbol) {
  errorsym(sym(Context), string, symbol);
}

/*
  error2 - prints an error message and reenters the REPL.
  Prints: "Error: 'Context' string", where Context is the name of the built-in Lisp function in which the error occurred.
*/
void error2 (PGM_P string) {
  errorsym2(sym(Context), string);
}"#

#-errors
#"
/*
  formaterr - displays a format error with a ^ pointing to the error
*/
void formaterr (object *formatstr, PGM_P string, uint8_t p) {
  pln(pserial); indent(4, ' ', pserial); printstring(formatstr, pserial); pln(pserial);
  indent(p+5, ' ', pserial); pserial('^');
  error2(string);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}"#

#+errors
#"
/*
  formaterr - displays a format error with a ^ pointing to the error
*/
void formaterr (object *formatstr, PGM_P string, uint8_t p) {
  pln(pserial); indent(4, ' ', pserial); printstring(formatstr, pserial); pln(pserial);
  indent(p+5, ' ', pserial); pserial('^');
  error2(string);
  pln(pserial);
  GCStack = NULL;
  longjmp(*handler, 1);
}"#

#"
// Save space as these are used multiple times
const char notanumber[] PROGMEM = "argument is not a number";
const char notaninteger[] PROGMEM = "argument is not an integer";
const char notastring[] PROGMEM = "argument is not a string";
const char notalist[] PROGMEM = "argument is not a list";
const char notasymbol[] PROGMEM = "argument is not a symbol";
const char notproper[] PROGMEM = "argument is not a proper list";
const char toomanyargs[] PROGMEM = "too many arguments";
const char toofewargs[] PROGMEM = "too few arguments";
const char noargument[] PROGMEM = "missing argument";
const char nostream[] PROGMEM = "missing stream argument";
const char overflow[] PROGMEM = "arithmetic overflow";
const char divisionbyzero[] PROGMEM = "division by zero";
const char indexnegative[] PROGMEM = "index can't be negative";
const char invalidarg[] PROGMEM = "invalid argument";
const char invalidkey[] PROGMEM = "invalid keyword";
const char illegalclause[] PROGMEM = "illegal clause";
const char invalidpin[] PROGMEM = "invalid pin";
const char oddargs[] PROGMEM = "odd number of arguments";
const char indexrange[] PROGMEM = "index out of range";
const char canttakecar[] PROGMEM = "can't take car";
const char canttakecdr[] PROGMEM = "can't take cdr";
const char unknownstreamtype[] PROGMEM = "unknown stream type";"#))

(defparameter *setup-workspace* #"
// Set up workspace

/*
  initworkspace - initialises the workspace into a linked list of free objects
*/
void initworkspace () {
  Freelist = NULL;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    car(obj) = NULL;
    cdr(obj) = Freelist;
    Freelist = obj;
    Freespace++;
  }
}

/*
  myalloc - returns the first object from the linked list of free objects
*/
object *myalloc () {
  if (Freespace == 0) { Context = NIL; error2(PSTR("no room")); }
  object *temp = Freelist;
  Freelist = cdr(Freelist);
  Freespace--;
  return temp;
}

/*
  myfree - adds obj to the linked list of free objects.
  inline makes gc significantly faster
*/
inline void myfree (object *obj) {
  car(obj) = NULL;
  cdr(obj) = Freelist;
  Freelist = obj;
  Freespace++;
}"#)

(defparameter *make-objects* 
 
  '(#"
// Make each type of object

/*
  number - make an integer object with value n and return it
*/
object *number (int n) {
  object *ptr = myalloc();
  ptr->type = NUMBER;
  ptr->integer = n;
  return ptr;
}"#

 #+float
 #"
/*
  makefloat - make a floating point object with value f and return it
*/
object *makefloat (float f) {
  object *ptr = myalloc();
  ptr->type = FLOAT;
  ptr->single_float = f;
  return ptr;
}"#

 #"
/*
  character - make a character object with value c and return it
*/
object *character (uint8_t c) {
  object *ptr = myalloc();
  ptr->type = CHARACTER;
  ptr->chars = c;
  return ptr;
}"#

 #"
/*
  cons - make a cons with arg1 and arg2 return it
*/
object *cons (object *arg1, object *arg2) {
  object *ptr = myalloc();
  ptr->car = arg1;
  ptr->cdr = arg2;
  return ptr;
}"#

 #"
/*
  symbol - make a symbol object with value name and return it
*/
object *symbol (symbol_t name) {
  object *ptr = myalloc();
  ptr->type = SYMBOL;
  ptr->name = name;
  return ptr;
}"#

 #"
/*
  bsymbol - make a built-in symbol
*/
inline object *bsymbol (builtin_t name) {
  return intern(twist(name+BUILTINS));
}"#

 #+(or avr arm riscv)
 #"
/*
  codehead - make a code header object with value entry and return it
*/
object *codehead (int entry) {
  object *ptr = myalloc();
  ptr->type = CODE;
  ptr->integer = entry;
  return ptr;
}"#

 #"
/*
  intern - looks through the workspace for an existing occurrence of symbol name and returns it,
  otherwise calls symbol(name) to create a new symbol.
*/
object *intern (symbol_t name) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (obj->type == SYMBOL && obj->name == name) return obj;
  }
  return symbol(name);
}"#

 #+avr-nano
 #"
/*
  eqsymbols - compares the long string/symbol obj with the string in buffer.
*/
bool eqsymbols (object *obj, char *buffer) {
  object *arg = cdr(obj);
  int i = 0;
  while (!(arg == NULL && buffer[i] == 0)) {
    if (arg == NULL || buffer[i] == 0 || arg->chars != (buffer[i]<<8 | buffer[i+1])) return false;
    arg = car(arg);
    i = i + 2;
  }
  return true;
}"#

 #+avr
 #"
/*
  eqsymbols - compares the long string/symbol obj with the string in buffer.
*/
bool eqsymbols (object *obj, char *buffer) {
  object *arg = cdr(obj);
  int i = 0;
  while (!(arg == NULL && buffer[i] == 0)) {
    if (arg == NULL || buffer[i] == 0) return false;
    int test = 0, shift = 8;
    for (int j=0; j<2; j++, i++) {
      if (buffer[i] == 0) break;
      test = test | buffer[i]<<shift;
      shift = shift - 8;
    }
    if (arg->chars != test) return false;
    arg = car(arg);
  }
  return true;
}"#

 #+(or arm esp riscv)
 #"
/*
  eqsymbols - compares the long string/symbol obj with the string in buffer.
*/
bool eqsymbols (object *obj, char *buffer) {
  object *arg = cdr(obj);
  int i = 0;
  while (!(arg == NULL && buffer[i] == 0)) {
    if (arg == NULL || buffer[i] == 0) return false;
    int test = 0, shift = 24;
    for (int j=0; j<4; j++, i++) {
      if (buffer[i] == 0) break;
      test = test | buffer[i]<<shift;
      shift = shift - 8;
    }
    if (arg->chars != test) return false;
    arg = car(arg);
  }
  return true;
}"#

 #"
/*
  internlong - looks through the workspace for an existing occurrence of the long symbol in buffer and returns it,
  otherwise calls lispstring(buffer) to create a new symbol.
*/
object *internlong (char *buffer) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (obj->type == SYMBOL && longsymbolp(obj) && eqsymbols(obj, buffer)) return obj;
  }
  object *obj = lispstring(buffer);
  obj->type = SYMBOL;
  return obj;
}"#

 #"
/*
  stream - makes a stream object defined by streamtype and address, and returns it
*/
object *stream (uint8_t streamtype, uint8_t address) {
  object *ptr = myalloc();
  ptr->type = STREAM;
  ptr->integer = streamtype<<8 | address;
  return ptr;
}"#

 #"
/*
  newstring - makes an empty string object and returns it
*/
object *newstring () {
  object *ptr = myalloc();
  ptr->type = STRING;
  ptr->chars = 0;
  return ptr;
}"#))

(defparameter *garbage-collection* '(

#"
// Garbage collection"#

#+avr-nano
#"
/*
  markobject - recursively marks reachable objects, starting from obj
*/
void markobject (object *obj) {
  MARK:
  if (obj == NULL) return;
  if (marked(obj)) return;

  object* arg = car(obj);
  unsigned int type = obj->type;
  mark(obj);

  if (type >= PAIR || type == ZZERO) { // cons
    markobject(arg);
    obj = cdr(obj);
    goto MARK;
  }

  if ((type == STRING) || (type == SYMBOL && longsymbolp(obj))) {
    obj = cdr(obj);
    while (obj != NULL) {
      arg = car(obj);
      mark(obj);
      obj = arg;
    }
  }
}"#

#-avr-nano
#"
/*
  markobject - recursively marks reachable objects, starting from obj
*/
void markobject (object *obj) {
  MARK:
  if (obj == NULL) return;
  if (marked(obj)) return;

  object* arg = car(obj);
  unsigned int type = obj->type;
  mark(obj);

  if (type >= PAIR || type == ZZERO) { // cons
    markobject(arg);
    obj = cdr(obj);
    goto MARK;
  }

  if (type == ARRAY) {
    obj = cdr(obj);
    goto MARK;
  }

  if ((type == STRING) || (type == SYMBOL && longsymbolp(obj))) {
    obj = cdr(obj);
    while (obj != NULL) {
      arg = car(obj);
      mark(obj);
      obj = arg;
    }
  }
}"#

#"
/*
  sweep - goes through the workspace freeing objects that have not been marked,
  and unmarks marked objects
*/
void sweep () {
  Freelist = NULL;
  Freespace = 0;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    if (!marked(obj)) myfree(obj); else unmark(obj);
  }
}

/*
  gc - performs garbage collection by calling markobject() on each of the pointers to objects in use,
  followed by sweep() to free unused objects.
*/
void gc (object *form, object *env) {
  #if defined(printgcs)
  int start = Freespace;
  #endif
  markobject(tee);
  markobject(GlobalEnv);
  markobject(GCStack);
  markobject(form);
  markobject(env);
  sweep();
  #if defined(printgcs)
  pfl(pserial); pserial('{'); pint(Freespace - start, pserial); pserial('}');
  #endif
}"#))

#+avr
(defparameter *feature-list* #"
// Features

const char arrays[] PROGMEM = ":arrays";
const char doc[] PROGMEM = ":documentation";
const char machinecode[] PROGMEM = ":machine-code";
const char errorhandling[] PROGMEM = ":error-handling";

/*
  copyprogmemstring - copy a PROGMEM string to RAM.
*/
char *copyprogmemstring (PGM_P s, char *buffer) {
  int max = BUFFERSIZE-1;
  int i = 0;
  do {
    char c = pgm_read_byte(s++);
    buffer[i++] = c;
    if (c == 0) break;
  } while (i<max);
  return buffer;
}

/*
  features - create a list of features symbols from const strings.
*/
object *features () {
  char buffer[BUFFERSIZE];
  object *result = NULL;
  push(internlong(copyprogmemstring(errorhandling, buffer)), result);
  #if defined(CODESIZE)
  push(internlong(copyprogmemstring(machinecode, buffer)), result);
  #endif
  push(internlong(copyprogmemstring(doc, buffer)), result);
  push(internlong(copyprogmemstring(arrays, buffer)), result);
  return result;
}"#)

#+avr-nano
(defparameter *feature-list* "")

#+arm
(defparameter *feature-list* #"
// Features

const char floatingpoint[] = ":floating-point";
const char arrays[] = ":arrays";
const char doc[] = ":documentation";
const char machinecode[] = ":machine-code";
const char errorhandling[] = ":error-handling";
const char wifi[] = ":wi-fi";
const char gfx[] = ":gfx";

/*
  features - create a list of features symbols from const strings.
*/
object *features () {
  object *result = NULL;
  push(internlong((char *)gfx), result);
  push(internlong((char *)wifi), result);
  push(internlong((char *)errorhandling), result);
  push(internlong((char *)machinecode), result);
  push(internlong((char *)doc), result);
  push(internlong((char *)arrays), result);
  push(internlong((char *)floatingpoint), result);
  return result;
}"#)

#+esp
(defparameter *feature-list* #"
// Features

const char floatingpoint[] = ":floating-point";
const char arrays[] = ":arrays";
const char doc[] = ":documentation";
const char errorhandling[] = ":error-handling";
const char wifi[] = ":wi-fi";
const char gfx[] = ":gfx";

/*
  features - create a list of features symbols from const strings.
*/
object *features () {
  object *result = NULL;
  push(internlong((char *)gfx), result);
  push(internlong((char *)wifi), result);
  push(internlong((char *)errorhandling), result);
  push(internlong((char *)doc), result);
  push(internlong((char *)arrays), result);
  push(internlong((char *)floatingpoint), result);
  return result;
}"#)

#+riscv
(defparameter *feature-list* #"
// Features

const char floatingpoint[] = ":floating-point";
const char arrays[] = ":arrays";
const char doc[] = ":documentation";
const char machinecode[] = ":machine-code";
const char errorhandling[] = ":error-handling";
const char wifi[] = ":wi-fi";
const char gfx[] = ":gfx";

/*
  features - create a list of features symbols from const strings.
*/
object *features () {
  object *result = NULL;
  push(internlong((char *)gfx), result);
  push(internlong((char *)wifi), result);
  push(internlong((char *)errorhandling), result);
  push(internlong((char *)machinecode), result);
  push(internlong((char *)doc), result);
  push(internlong((char *)arrays), result);
  push(internlong((char *)floatingpoint), result);
  return result;
}"#)

(defparameter *tracing* #"
// Tracing

/*
  tracing - returns a number between 1 and TRACEMAX if name is being traced, or 0 otherwise
*/
int tracing (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) return i+1;
    i++;
  }
  return 0;
}

/*
  trace - enables tracing of symbol name and adds it to the array TraceFn[].
*/
void trace (symbol_t name) {
  if (tracing(name)) error(PSTR("already being traced"), symbol(name));
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == 0) { TraceFn[i] = name; TraceDepth[i] = 0; return; }
    i++;
  }
  error2(PSTR("already tracing " stringify(TRACEMAX) " functions"));
}

/*
  untrace - disables tracing of symbol name and removes it from the array TraceFn[].
*/
void untrace (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) { TraceFn[i] = 0; return; }
    i++;
  }
  error(PSTR("not tracing"), symbol(name));
}"#)

(defparameter *helper-functions* '(

 #"
// Helper functions

/*
  consp - implements Lisp consp
*/
bool consp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type >= PAIR || type == ZZERO;
}

/*
  atom - implements Lisp atom
*/
#define atom(x) (!consp(x))

/*
  listp - implements Lisp listp
*/
bool listp (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type >= PAIR || type == ZZERO;
}

/*
  improperp - tests whether x is an improper list
*/
#define improperp(x) (!listp(x))

object *quote (object *arg) {
  return cons(bsymbol(QUOTE), cons(arg,NULL));
}"#

  #"
// Radix 40 encoding

/*
  builtin - converts a symbol name to builtin
*/
builtin_t builtin (symbol_t name) {
  return (builtin_t)(untwist(name) - BUILTINS);
}

/*
 sym - converts a builtin to a symbol name
*/
symbol_t sym (builtin_t x) {
  return twist(x + BUILTINS);
}

/*
  toradix40 - returns a number from 0 to 39 if the character can be encoded, or -1 otherwise.
*/
int8_t toradix40 (char ch) {
  if (ch == 0) return 0;
  if (ch >= '0' && ch <= '9') return ch-'0'+1;
  if (ch == '-') return 37; if (ch == '*') return 38; if (ch == '$') return 39;
  ch = ch | 0x20;
  if (ch >= 'a' && ch <= 'z') return ch-'a'+11;
  return -1; // Invalid
}

/*
  fromradix40 - returns the character encoded by the number n.
*/
char fromradix40 (char n) {
  if (n >= 1 && n <= 10) return '0'+n-1;
  if (n >= 11 && n <= 36) return 'a'+n-11;
  if (n == 37) return '-'; if (n == 38) return '*'; if (n == 39) return '$';
  return 0;
}"#

  #+avr-nano
  #"
/*
  pack40 - packs three radix40-encoded characters from buffer into a 16-bit number and returns it.
*/
uint16_t pack40 (char *buffer) {
  return (((toradix40(buffer[0]) * 40) + toradix40(buffer[1])) * 40 + toradix40(buffer[2]));
}"#

  #+(or avr msp430 badge)
  #"
/*
  pack40 - packs three radix40-encoded characters from buffer into a 16-bit number and returns it.
*/
uint32_t pack40 (char *buffer) {
  int x = 0, j = 0;
  for (int i=0; i<3; i++) {
    x = x * 40 + toradix40(buffer[j]);
    if (buffer[j] != 0) j++;
  }
  return x;
}"#

  #+(or arm esp riscv)
  #"
/*
  pack40 - packs six radix40-encoded characters from buffer into a 32-bit number and returns it.
*/
uint32_t pack40 (char *buffer) {
  int x = 0, j = 0;
  for (int i=0; i<6; i++) {
    x = x * 40 + toradix40(buffer[j]);
    if (buffer[j] != 0) j++;
  }
  return x;
}"#

  #+avr-nano
  #"
/*
  valid40 - returns true if the symbol in buffer can be encoded as three radix40-encoded characters.
*/
bool valid40 (char *buffer) {
 return (toradix40(buffer[0]) >= 11 && toradix40(buffer[1]) >= 0 && toradix40(buffer[2]) >= 0);
}"#

  #+(or avr msp430 badge)
  #"
/*
  valid40 - returns true if the symbol in buffer can be encoded as three radix40-encoded characters.
*/
bool valid40 (char *buffer) {
  int t = 11;
  for (int i=0; i<3; i++) {
    if (toradix40(buffer[i]) < t) return false;
    if (buffer[i] == 0) break;
    t = 0;
  }
  return true;
}"#

  #+(or arm esp riscv)
  #"
/*
  valid40 - returns true if the symbol in buffer can be encoded as six radix40-encoded characters.
*/
bool valid40 (char *buffer) {
  int t = 11;
  for (int i=0; i<6; i++) {
    if (toradix40(buffer[i]) < t) return false;
    if (buffer[i] == 0) break;
    t = 0;
  }
  return true;
}"#

   #"
/*
  digitvalue - returns the numerical value of a hexadecimal digit, or 16 if invalid.
*/
int8_t digitvalue (char d) {
  if (d>='0' && d<='9') return d-'0';
  d = d | 0x20;
  if (d>='a' && d<='f') return d-'a'+10;
  return 16;
}"#

    #"
/*
  checkinteger - check that obj is an integer and return it
*/
int checkinteger (object *obj) {
  if (!integerp(obj)) error(notaninteger, obj);
  return obj->integer;
}"#

    #+arrays
    #"
/*
  checkbitvalue - check that obj is an integer equal to 0 or 1 and return it
*/
int checkbitvalue (object *obj) {
  if (!integerp(obj)) error(notaninteger, obj);
  int n = obj->integer;
  if (n & ~1) error(PSTR("argument is not a bit value"), obj);
  return n;
}"#

    #+float
    #"
/*
  checkintfloat - check that obj is an integer or floating-point number and return the number
*/
float checkintfloat (object *obj) {
  if (integerp(obj)) return (float)obj->integer;
  if (!floatp(obj)) error(notanumber, obj);
  return obj->single_float;
}"#

    #"
/*
  checkchar - check that obj is a character and return the character
*/
int checkchar (object *obj) {
  if (!characterp(obj)) error(PSTR("argument is not a character"), obj);
  return obj->chars;
}

/*
  checkstring - check that obj is a string
*/
object *checkstring (object *obj) {
  if (!stringp(obj)) error(notastring, obj);
  return obj;
}

int isstream (object *obj){
  if (!streamp(obj)) error(PSTR("not a stream"), obj);
  return obj->integer;
}

int isbuiltin (object *obj, builtin_t n) {
  return symbolp(obj) && obj->name == sym(n);
}

bool builtinp (symbol_t name) {
  return (untwist(name) >= BUILTINS);
}

int checkkeyword (object *obj) {
  if (!keywordp(obj)) error(PSTR("argument is not a keyword"), obj);
  builtin_t kname = builtin(obj->name);
  uint8_t context = getminmax(kname);
  if (context != 0 && context != Context) error(invalidkey, obj);
  return ((int)lookupfn(kname));
}

/*
  checkargs - checks that the number of objects in the list args
  is within the range specified in the symbol lookup table
*/
void checkargs (object *args) {
  int nargs = listlength(args);
  checkminmax(Context, nargs);
}"#

    #-float
    #"
/*
  eq - implements Lisp eq
*/
boolean eq (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
  if (characterp(arg1) && characterp(arg2)) return true;  // Same character
  return false;
}"#

    #+float
    #"
/*
  eq - implements Lisp eq
*/
boolean eq (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
  if (floatp(arg1) && floatp(arg2)) return true; // Same float
  if (characterp(arg1) && characterp(arg2)) return true;  // Same character
  return false;
}"#

    #+avr-nano
    #"
/*
  equal - implements Lisp equal
*/
bool equal (object *arg1, object *arg2) {
  if (stringp(arg1) && stringp(arg2)) return stringcompare(cons(arg1, cons(arg2, nil)), false, false, true);
  if (consp(arg1) && consp(arg2)) return (equal(car(arg1), car(arg2)) && equal(cdr(arg1), cdr(arg2)));
  return eq(arg1, arg2);
}"#

    #-avr-nano
    #"
/*
  equal - implements Lisp equal
*/
bool equal (object *arg1, object *arg2) {
  if (stringp(arg1) && stringp(arg2)) return (stringcompare(cons(arg1, cons(arg2, nil)), false, false, true) != -1);
  if (consp(arg1) && consp(arg2)) return (equal(car(arg1), car(arg2)) && equal(cdr(arg1), cdr(arg2)));
  return eq(arg1, arg2);
}"#

    #"
/*
  listlength - returns the length of a list
*/
int listlength (object *list) {
  int length = 0;
  while (list != NULL) {
    if (improperp(list)) error2(notproper);
    list = cdr(list);
    length++;
  }
  return length;
}"#

    #+avr-nano
    #"
/*
  checkarguments - checks the arguments list in a special form such as with-xxx,
  dolist, or dotimes.
*/
object *checkarguments (object *args, uint8_t min, uint8_t max) {
  if (args == NULL) error2(noargument);
  args = first(args);
  if (!listp(args)) error(notalist, args);
  uint8_t length = listlength(args);
  if (length < min) error(toofewargs, args);
  if (length > max) error(toomanyargs, args);
  return args;
}"#

    #-avr-nano
    #"
/*
  checkarguments - checks the arguments list in a special form such as with-xxx,
  dolist, or dotimes.
*/
object *checkarguments (object *args, int min, int max) {
  if (args == NULL) error2(noargument);
  args = first(args);
  if (!listp(args)) error(notalist, args);
  int length = listlength(args);
  if (length < min) error(toofewargs, args);
  if (length > max) error(toomanyargs, args);
  return args;
}"#

#"
// Mathematical helper functions"#

 #+(or avr avr-nano)
 #"
/*
  pseudoRandom - returns a pseudorandom number from 0 to range-1
  For an explanation of the dummy line see: http://forum.ulisp.com/t/compiler-mystery-any-suggestions/854
*/
uint16_t pseudoRandom (int range) {
  if (RandomSeed == 0) RandomSeed++;
  uint16_t l = RandomSeed & 1;
  RandomSeed = RandomSeed >> 1;
  if (l == 1) RandomSeed = RandomSeed ^ 0xD295;
  int dummy; if (RandomSeed == 0) Serial.print((int)&dummy); // Do not remove!
  return RandomSeed % range;
}"#

 #+float
 #"
/*
  add_floats - used by fn_add
  Converts the numbers in args to floats, adds them to fresult, and returns the sum as a Lisp float.
*/
object *add_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult + checkintfloat(arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

/*
  subtract_floats - used by fn_subtract with more than one argument
  Converts the numbers in args to floats, subtracts them from fresult, and returns the result as a Lisp float.
*/
object *subtract_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult - checkintfloat(arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

/*
  negate - used by fn_subtract with one argument
  If the result is an integer, and negating it doesn't overflow, keep the result as an integer.
  Otherwise convert the result to a float, negate it, and return the result as a Lisp float.
*/
object *negate (object *arg) {
  if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(-result);
    else return number(-result);
  } else if (floatp(arg)) return makefloat(-(arg->single_float));
  else error(notanumber, arg);
  return nil;
}

/*
  multiply_floats - used by fn_multiply
  Converts the numbers in args to floats, adds them to fresult, and returns the result as a Lisp float.
*/
object *multiply_floats (object *args, float fresult) {
  while (args != NULL) {
   object *arg = car(args);
    fresult = fresult * checkintfloat(arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

/*
  divide_floats - used by fn_divide
  Converts the numbers in args to floats, divides fresult by them, and returns the result as a Lisp float.
*/
object *divide_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    float f = checkintfloat(arg);
    if (f == 0.0) error2(divisionbyzero);
    fresult = fresult / f;
    args = cdr(args);
  }
  return makefloat(fresult);
}"#

 #+(or avr avr-nano)
#"
/*
  compare - a generic compare function
  Used to implement the other comparison functions.
  If lt is true the result is true if each argument is less than the next argument.
  If gt is true the result is true if each argument is greater than the next argument.
  If eq is true the result is true if each argument is equal to the next argument.
*/
object *compare (object *args, bool lt, bool gt, bool eq) {
  int arg1 = checkinteger(first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(first(args));
    if (!lt && (arg1 < arg2)) return nil;
    if (!eq && (arg1 == arg2)) return nil;
    if (!gt && (arg1 > arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}"#

#-(or avr avr-nano)
#"
/*
  compare - a generic compare function
  Used to implement the other comparison functions.
  If lt is true the result is true if each argument is less than the next argument.
  If gt is true the result is true if each argument is greater than the next argument.
  If eq is true the result is true if each argument is equal to the next argument.
*/
object *compare (object *args, bool lt, bool gt, bool eq) {
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!lt && ((arg1->integer) < (arg2->integer))) return nil;
      if (!eq && ((arg1->integer) == (arg2->integer))) return nil;
      if (!gt && ((arg1->integer) > (arg2->integer))) return nil;
    } else {
      if (!lt && (checkintfloat(arg1) < checkintfloat(arg2))) return nil;
      if (!eq && (checkintfloat(arg1) == checkintfloat(arg2))) return nil;
      if (!gt && (checkintfloat(arg1) > checkintfloat(arg2))) return nil;
    }
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}"#

#"
/*
  intpower - calculates base to the power exp as an integer
*/
int intpower (int base, int exp) {
  int result = 1;
  while (exp) {
    if (exp & 1) result = result * base;
    exp = exp / 2;
    base = base * base;
  }
  return result;
}"#))

#+avr-nano
(defparameter *association-lists* #"
// Association lists

/*
  delassoc - deletes the pair matching key from an association list and returns the key, or nil if not found
*/
object *delassoc (object *key, object **alist) {
  object *list = *alist;
  object *prev = NULL;
  while (list != NULL) {
    object *pair = first(list);
    if (eq(key,car(pair))) {
      if (prev == NULL) *alist = cdr(list);
      else cdr(prev) = cdr(list);
      return key;
    }
    prev = list;
    list = cdr(list);
  }
  return nil;
}"#)

#-avr-nano
(defparameter *association-lists* #"
// Association lists

#-avr-nano
/*
  testargument - handles the :test argument for functions that accept it
*/
object *testargument (object *args) {
  object *test = bsymbol(EQ);
  if (args != NULL) {
    if (cdr(args) == NULL) error2(PSTR("unpaired keyword"));
    if ((isbuiltin(first(args), TEST))) test = second(args);
    else error(PSTR("unsupported keyword"), first(args));
  }
  return test;
}

/*
  delassoc - deletes the pair matching key from an association list and returns the key, or nil if not found
*/
object *delassoc (object *key, object **alist) {
  object *list = *alist;
  object *prev = NULL;
  while (list != NULL) {
    object *pair = first(list);
    if (eq(key,car(pair))) {
      if (prev == NULL) *alist = cdr(list);
      else cdr(prev) = cdr(list);
      return key;
    }
    prev = list;
    list = cdr(list);
  }
  return nil;
}"#)

(defparameter *array-utilities* '(

#+arrays
#"
// Array utilities"#

#+(and arrays avr)
#"
/*
  nextpower2 - returns the smallest power of 2 that is equal to or greater than n
*/
int nextpower2 (int n) {
  n--; n |= n >> 1; n |= n >> 2; n |= n >> 4;
  n |= n >> 8; n++;
  return n<2 ? 2 : n;
}"#

#+(and arrays (not avr))
#"
/*
  nextpower2 - returns the smallest power of 2 that is equal to or greater than n
*/
int nextpower2 (int n) {
  n--; n |= n >> 1; n |= n >> 2; n |= n >> 4;
  n |= n >> 8; n |= n >> 16; n++;
  return n<2 ? 2 : n;
}"#

#+arrays
#"
/*
  buildarray - builds an array with n elements using a tree of size s which must be a power of 2
  The elements are initialised to the default def
*/
object *buildarray (int n, int s, object *def) {
  int s2 = s>>1;
  if (s2 == 1) {
    if (n == 2) return cons(def, def);
    else if (n == 1) return cons(def, NULL);
    else return NULL;
  } else if (n >= s2) return cons(buildarray(s2, s2, def), buildarray(n - s2, s2, def));
  else return cons(buildarray(n, s2, def), nil);
}

object *makearray (object *dims, object *def, bool bitp) {
  int size = 1;
  object *dimensions = dims;
  while (dims != NULL) {
    int d = car(dims)->integer;
    if (d < 0) error2(PSTR("dimension can't be negative"));
    size = size * d;
    dims = cdr(dims);
  }
  // Bit array identified by making first dimension negative
  if (bitp) {
    size = (size + sizeof(int)*8 - 1)/(sizeof(int)*8);
    car(dimensions) = number(-(car(dimensions)->integer));
  }
  object *ptr = myalloc();
  ptr->type = ARRAY;
  object *tree = nil;
  if (size != 0) tree = buildarray(size, nextpower2(size), def);
  ptr->cdr = cons(tree, dimensions);
  return ptr;
}

/*
  arrayref - returns a pointer to the element specified by index in the array of size s
*/
object **arrayref (object *array, int index, int size) {
  int mask = nextpower2(size)>>1;
  object **p = &car(cdr(array));
  while (mask) {
    if ((index & mask) == 0) p = &(car(*p)); else p = &(cdr(*p));
    mask = mask>>1;
  }
  return p;
}

/*
  getarray - gets a pointer to an element in a multi-dimensional array, given a list of the subscripts subs
  If the first subscript is negative it's a bit array and bit is set to the bit number
*/
object **getarray (object *array, object *subs, object *env, int *bit) {
  int index = 0, size = 1, s;
  *bit = -1;
  bool bitp = false;
  object *dims = cddr(array);
  while (dims != NULL && subs != NULL) {
    int d = car(dims)->integer;
    if (d < 0) { d = -d; bitp = true; }
    if (env) s = checkinteger(eval(car(subs), env)); else s = checkinteger(car(subs));
    if (s < 0 || s >= d) error(PSTR("subscript out of range"), car(subs));
    size = size * d;
    index = index * d + s;
    dims = cdr(dims); subs = cdr(subs);
  }
  if (dims != NULL) error2(PSTR("too few subscripts"));
  if (subs != NULL) error2(PSTR("too many subscripts"));
  if (bitp) {
    size = (size + sizeof(int)*8 - 1)/(sizeof(int)*8);
    *bit = index & (sizeof(int)==4 ? 0x1F : 0x0F);
    index = index>>(sizeof(int)==4 ? 5 : 4);
  }
  return arrayref(array, index, size);
}

/*
  rslice - reads a slice of an array recursively
*/
void rslice (object *array, int size, int slice, object *dims, object *args) {
  int d = first(dims)->integer;
  for (int i = 0; i < d; i++) {
    int index = slice * d + i;
    if (!consp(args)) error2(PSTR("initial contents don't match array type"));
    if (cdr(dims) == NULL) {
      object **p = arrayref(array, index, size);
      *p = car(args);
    } else rslice(array, size, index, cdr(dims), car(args));
    args = cdr(args);
  }
}

/*
  readarray - reads a list structure from args and converts it to a d-dimensional array.
  Uses rslice for each of the slices of the array.
*/
object *readarray (int d, object *args) {
  object *list = args;
  object *dims = NULL; object *head = NULL;
  int size = 1;
  for (int i = 0; i < d; i++) {
    if (!listp(list)) error2(PSTR("initial contents don't match array type"));
    int l = listlength(list);
    if (dims == NULL) { dims = cons(number(l), NULL); head = dims; }
    else { cdr(dims) = cons(number(l), NULL); dims = cdr(dims); }
    size = size * l;
    if (list != NULL) list = car(list);
  }
  object *array = makearray(head, NULL, false);
  rslice(array, size, 0, head, args);
  return array;
}

/*
  readbitarray - reads an item in the format #*1010101000110 by reading it and returning a list of integers,
  and then converting that to a bit array
*/
object *readbitarray (gfun_t gfun) {
  char ch = gfun();
  object *head = NULL;
  object *tail = NULL;
  while (!issp(ch) && !isbr(ch)) {
    if (ch != '0' && ch != '1') error2(PSTR("illegal character in bit array"));
    object *cell = cons(number(ch - '0'), NULL);
    if (head == NULL) head = cell;
    else tail->cdr = cell;
    tail = cell;
    ch = gfun();
  }
  LastChar = ch;
  int size = listlength(head);
  object *array = makearray(cons(number(size), NULL), number(0), true);
  size = (size + sizeof(int)*8 - 1)/(sizeof(int)*8);
  int index = 0;
  while (head != NULL) {
    object **loc = arrayref(array, index>>(sizeof(int)==4 ? 5 : 4), size);
    int bit = index & (sizeof(int)==4 ? 0x1F : 0x0F);
    *loc = number((((*loc)->integer) & ~(1<<bit)) | (car(head)->integer)<<bit);
    index++;
    head = cdr(head);
  }
  return array;
}

/*
  pslice - prints a slice of an array recursively
*/
void pslice (object *array, int size, int slice, object *dims, pfun_t pfun, bool bitp) {
  bool spaces = true;
  if (slice == -1) { spaces = false; slice = 0; }
  int d = first(dims)->integer;
  if (d < 0) d = -d;
  for (int i = 0; i < d; i++) {
    if (i && spaces) pfun(' ');
    int index = slice * d + i;
    if (cdr(dims) == NULL) {
      if (bitp) pint(((*arrayref(array, index>>(sizeof(int)==4 ? 5 : 4), size))->integer)>>
        (index & (sizeof(int)==4 ? 0x1F : 0x0F)) & 1, pfun);
      else printobject(*arrayref(array, index, size), pfun);
    } else { pfun('('); pslice(array, size, index, cdr(dims), pfun, bitp); pfun(')'); }
  }
}

/*
  printarray - prints an array in the appropriate Lisp format
*/
void printarray (object *array, pfun_t pfun) {
  object *dimensions = cddr(array);
  object *dims = dimensions;
  bool bitp = false;
  int size = 1, n = 0;
  while (dims != NULL) {
    int d = car(dims)->integer;
    if (d < 0) { bitp = true; d = -d; }
    size = size * d;
    dims = cdr(dims); n++;
  }
  if (bitp) size = (size + sizeof(int)*8 - 1)/(sizeof(int)*8);
  pfun('#');
  if (n == 1 && bitp) { pfun('*'); pslice(array, size, -1, dimensions, pfun, bitp); }
  else {
    if (n > 1) { pint(n, pfun); pfun('A'); }
    pfun('('); pslice(array, size, 0, dimensions, pfun, bitp); pfun(')');
  }
}"#))

(defparameter *string-utilities*

  '(#"
// String utilities

void indent (uint8_t spaces, char ch, pfun_t pfun) {
  for (uint8_t i=0; i<spaces; i++) pfun(ch);
}

/*
  startstring - starts building a string
*/
object *startstring () {
  object *string = newstring();
  GlobalString = string;
  GlobalStringTail = string;
  return string;
}

/*
  princtostring - implements Lisp princtostring function
*/
object *princtostring (object *arg) {
  object *obj = startstring();
  prin1object(arg, pstr);
  return obj;
}"#

  #+(or avr avr-nano msp430 badge)
  #"
/*
  buildstring - adds a character on the end of a string
  Handles Lisp strings packed two characters per 16-bit word
*/
void buildstring (char ch, object **tail) {
  object *cell;
  if (cdr(*tail) == NULL) {
    cell = myalloc(); cdr(*tail) = cell;
  } else if (((*tail)->chars & 0xFF) == 0) {
    (*tail)->chars |= ch; return;
  } else {
    cell = myalloc(); car(*tail) = cell;
  }
  car(cell) = NULL; cell->chars = ch<<8; *tail = cell;
}"#


  #+(or arm esp riscv)
  #"
/*
  buildstring - adds a character on the end of a string
  Handles Lisp strings packed four characters per 32-bit word
*/
void buildstring (char ch, object** tail) {
  object* cell;
  if (cdr(*tail) == NULL) {
    cell = myalloc(); cdr(*tail) = cell;
  } else if (((*tail)->chars & 0xFFFFFF) == 0) {
    (*tail)->chars |= ch<<16; return;
  } else if (((*tail)->chars & 0xFFFF) == 0) {
    (*tail)->chars |= ch<<8; return;
  } else if (((*tail)->chars & 0xFF) == 0) {
    (*tail)->chars |= ch; return;
  } else {
    cell = myalloc(); car(*tail) = cell;
  }
  car(cell) = NULL; cell->chars = ch<<24; *tail = cell;
}"#

  #"
/*
  copystring - returns a copy of a Lisp string
*/
object *copystring (object *arg) {
  object *obj = newstring();
  object *ptr = obj;
  arg = cdr(arg);
  while (arg != NULL) {
    object *cell =  myalloc(); car(cell) = NULL;
    if (cdr(obj) == NULL) cdr(obj) = cell; else car(ptr) = cell;
    ptr = cell;
    ptr->chars = arg->chars;
    arg = car(arg);
  }
  return obj;
}

/*
  readstring - reads characters from an input stream up to delimiter delim
  and returns a Lisp string
*/
object *readstring (uint8_t delim, bool esc, gfun_t gfun) {
  object *obj = newstring();
  object *tail = obj;
  int ch = gfun();
  if (ch == -1) return nil;
  while ((ch != delim) && (ch != -1)) {
    if (esc && ch == '\\') ch = gfun();
    buildstring(ch, &tail);
    ch = gfun();
  }
  return obj;
}

/*
  stringlength - returns the length of a Lisp string
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
int stringlength (object *form) {
  int length = 0;
  form = cdr(form);
  while (form != NULL) {
    int chars = form->chars;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      if (chars>>i & 0xFF) length++;
    }
    form = car(form);
  }
  return length;
}"#

#+avr-nano
#"
/*
  nthchar - returns the nth character from a Lisp string
*/
uint8_t nthchar (object *string, int n) {
  object *arg = cdr(string);
  int top;
  if (sizeof(int) == 4) { top = n>>2; n = 3 - (n&3); }
  else { top = n>>1; n = 1 - (n&1); }
  for (int i=0; i<top; i++) {
    if (arg == NULL) return 0;
    arg = car(arg);
  }
  if (arg == NULL) return 0;
  return (arg->chars)>>(n*8) & 0xFF;
}"#

#-avr-nano
#"
/*
  getcharplace - gets character n in a Lisp string, and sets shift to (- the shift position -2)
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word.
*/
object **getcharplace (object *string, int n, int *shift) {
  object **arg = &cdr(string);
  int top;
  if (sizeof(int) == 4) { top = n>>2; *shift = 3 - (n&3); }
  else { top = n>>1; *shift = 1 - (n&1); }
  *shift = - (*shift + 2);
  for (int i=0; i<top; i++) {
    if (*arg == NULL) break;
    arg = &car(*arg);
  }
  return arg;
}

/*
  nthchar - returns the nth character from a Lisp string
*/
uint8_t nthchar (object *string, int n) {
  int shift;
  object **arg = getcharplace(string, n, &shift);
  if (*arg == NULL) return 0;
  return (((*arg)->chars)>>((-shift-2)<<3)) & 0xFF;
}"#

#"
/*
  gstr - reads a character from a string stream
*/
int gstr () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = nthchar(GlobalString, GlobalStringIndex++);
  if (c != 0) return c;
  return '\n'; // -1?
}

/*
  pstr - prints a character to a string stream
*/
void pstr (char c) {
  buildstring(c, &GlobalStringTail);
}

/*
  lispstring - converts a C string to a Lisp string
*/
object *lispstring (char *s) {
  object *obj = newstring();
  object *tail = obj;
  while(1) {
    char ch = *s++;
    if (ch == 0) break;
    if (ch == '\\') ch = *s++;
    buildstring(ch, &tail);
  }
  return obj;
}"#

#+avr-nano
#"
/*
  stringcompare - a generic string compare function
  Used to implement the other string comparison functions.
  If lt is true the result is true if each argument is less than the next argument.
  If gt is true the result is true if each argument is greater than the next argument.
  If eq is true the result is true if each argument is equal to the next argument.
*/
bool stringcompare (object *args, bool lt, bool gt, bool eq) {
  object *arg1 = checkstring(first(args));
  object *arg2 = checkstring(second(args));
  arg1 = cdr(arg1);
  arg2 = cdr(arg2);
  while ((arg1 != NULL) || (arg2 != NULL)) {
    if (arg1 == NULL) return lt;
    if (arg2 == NULL) return gt;
    if (arg1->chars < arg2->chars) return lt;
    if (arg1->chars > arg2->chars) return gt;
    arg1 = car(arg1);
    arg2 = car(arg2);
  }
  return eq;
}"#

#-avr-nano
#"
/*
  stringcompare - a generic string compare function
  Used to implement the other string comparison functions.
  Returns -1 if the comparison is false, or the index of the first mismatch if it is true.
  If lt is true the result is true if the first argument is less than the second argument.
  If gt is true the result is true if the first argument is greater than the second argument.
  If eq is true the result is true if the first argument is equal to the second argument.
*/
int stringcompare (object *args, bool lt, bool gt, bool eq) {
  object *arg1 = checkstring(first(args));
  object *arg2 = checkstring(second(args));
  arg1 = cdr(arg1); arg2 = cdr(arg2);
  int m = 0; chars_t a = 0, b = 0;
  while ((arg1 != NULL) || (arg2 != NULL)) {
    if (arg1 == NULL) return lt ? m : -1;
    if (arg2 == NULL) return gt ? m : -1;
    a = arg1->chars; b = arg2->chars;
    if (a < b) { if (lt) { m = m + sizeof(int); while (a != b) { m--; a = a >> 8; b = b >> 8; } return m; } else return -1; }
    if (a > b) { if (gt) { m = m + sizeof(int); while (a != b) { m--; a = a >> 8; b = b >> 8; } return m; } else return -1; }
    arg1 = car(arg1); arg2 = car(arg2);
    m = m + sizeof(int);
  }
  if (eq) { m = m - sizeof(int); while (a != 0) { m++; a = a << 8;} return m;} else return -1;
}"#

#+(and doc (or avr esp))
#"
/*
  documentation - returns the documentation string of a built-in or user-defined function.
*/
object *documentation (object *arg, object *env) {
  if (arg == NULL) return nil;
  if (!symbolp(arg)) error(notasymbol, arg);
  object *pair = findpair(arg, env);
  if (pair != NULL) {
    object *val = cdr(pair);
    if (listp(val) && first(val)->name == sym(LAMBDA) && cdr(val) != NULL && cddr(val) != NULL) {
      if (stringp(third(val))) return third(val);
    }
  }
  symbol_t docname = arg->name;
  if (!builtinp(docname)) return nil;
  char *docstring = lookupdoc(builtin(docname));
  if (docstring == NULL) return nil;
  object *obj = startstring();
  pfstring(docstring, pstr);
  return obj;
}"#

#+(and doc (or arm riscv))
#"
/*
  documentation - returns the documentation string of a built-in or user-defined function.
*/
object *documentation (object *arg, object *env) {
  if (arg == NULL) return nil;
  if (!symbolp(arg)) error(notasymbol, arg);
  object *pair = findpair(arg, env);
  if (pair != NULL) {
    object *val = cdr(pair);
    if (listp(val) && first(val)->name == sym(LAMBDA) && cdr(val) != NULL && cddr(val) != NULL) {
      if (stringp(third(val))) return third(val);
    }
  }
  symbol_t docname = arg->name;
  if (!builtinp(docname)) return nil;
  char *docstring = lookupdoc(builtin(docname));
  if (docstring == NULL) return nil;
  object *obj = startstring();
  pfstring(docstring, pstr);
  return obj;
}"#

#+doc
#"
/*
  apropos - finds the user-defined and built-in functions whose names contain the specified string or symbol,
  and prints them if print is true, or returns them in a list.
*/
object *apropos (object *arg, bool print) {
  char buf[17], buf2[33];
  char *part = cstring(princtostring(arg), buf, 17);
  object *result = cons(NULL, NULL);
  object *ptr = result;
  // User-defined?
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    char *full = cstring(princtostring(var), buf2, 33);
    if (strstr(full, part) != NULL) {
      if (print) {
        printsymbol(var, pserial); pserial(' '); pserial('(');
        if (consp(val) && symbolp(car(val)) && builtin(car(val)->name) == LAMBDA) pfstring(PSTR("user function"), pserial);
        else if (consp(val) && car(val)->type == CODE) pfstring(PSTR("code"), pserial);
        else pfstring(PSTR("user symbol"), pserial);
        pserial(')'); pln(pserial);
      } else {
        cdr(ptr) = cons(var, NULL); ptr = cdr(ptr);
      }
    }
    globals = cdr(globals);
    testescape();
  }
  // Built-in?
  int entries = tablesize(0) + tablesize(1);
  for (int i = 0; i < entries; i++) {
    if (findsubstring(part, (builtin_t)i)) {
      if (print) {
        uint8_t fntype = getminmax(i)>>6;
        pbuiltin((builtin_t)i, pserial); pserial(' '); pserial('(');
        if (fntype == FUNCTIONS) pfstring(PSTR("function"), pserial);
        else if (fntype == SPECIAL_FORMS) pfstring(PSTR("special form"), pserial);
        else pfstring(PSTR("symbol/keyword"), pserial);
        pserial(')'); pln(pserial);
      } else {
        cdr(ptr) = cons(bsymbol(i), NULL); ptr = cdr(ptr);
      }
    }
    testescape();
  }
  return cdr(result);
}"#

  #-avr-nano
  #"
/*
  cstring - converts a Lisp string to a C string in buffer and returns buffer
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
char *cstring (object *form, char *buffer, int buflen) {
  form = cdr(checkstring(form));
  int index = 0;
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (ch) {
        if (index >= buflen-1) error2(PSTR("no room for string"));
        buffer[index++] = ch;
      }
    }
    form = car(form);
  }
  buffer[index] = '\0';
  return buffer;
}"#

#+wifi
#"
/*
  iptostring - converts a 32-bit IP address to a lisp string
*/
object *iptostring (uint32_t ip) {
  union { uint32_t data2; uint8_t u8[4]; };
  object *obj = startstring();
  data2 = ip;
  for (int i=0; i<4; i++) {
    if (i) pstr('.');
    pintbase(u8[i], 10, pstr);
  }
  return obj;
}

/*
  ipstring - parses an IP address from a Lisp string and returns it as an IPAddress type (uint32_t)
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
uint32_t ipstring (object *form) {
  form = cdr(checkstring(form));
  int p = 0;
  union { uint32_t ipaddress; uint8_t ipbytes[4]; } ;
  ipaddress = 0;
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (ch) {
        if (ch == '.') { p++; if (p > 3) error2(PSTR("illegal IP address")); }
        else ipbytes[p] = (ipbytes[p] * 10) + ch - '0';
      }
    }
    form = car(form);
  }
  return ipaddress;
}"#))

(defparameter *closures* #"
// Lookup variable in environment

object *value (symbol_t n, object *env) {
  while (env != NULL) {
    object *pair = car(env);
    if (pair != NULL && car(pair)->name == n) return pair;
    env = cdr(env);
  }
  return nil;
}

/*
  findpair - returns the (var . value) pair bound to variable var in the local or global environment
*/
object *findpair (object *var, object *env) {
  symbol_t name = var->name;
  object *pair = value(name, env);
  if (pair == NULL) pair = value(name, GlobalEnv);
  return pair;
}

/*
  boundp - tests whether var is bound to a value
*/
bool boundp (object *var, object *env) {
  if (!symbolp(var)) error(notasymbol, var);
  return (findpair(var, env) != NULL);
}

/*
  findvalue - returns the value bound to variable var, or gives an error if unbound
*/
object *findvalue (object *var, object *env) {
  object *pair = findpair(var, env);
  if (pair == NULL) error(PSTR("unknown variable"), var);
  return pair;
}

// Handling closures

object *closure (int tc, symbol_t name, object *function, object *args, object **env) {
  object *state = car(function);
  function = cdr(function);
  int trace = 0;
  if (name) trace = tracing(name);
  if (trace) {
    indent(TraceDepth[trace-1]<<1, ' ', pserial);
    pint(TraceDepth[trace-1]++, pserial);
    pserial(':'); pserial(' '); pserial('('); printsymbol(symbol(name), pserial);
  }
  object *params = first(function);
  if (!listp(params)) errorsym(name, notalist, params);
  function = cdr(function);
  // Dropframe
  if (tc) {
    if (*env != NULL && car(*env) == NULL) {
      pop(*env);
      while (*env != NULL && car(*env) != NULL) pop(*env);
    } else push(nil, *env);
  }
  // Push state
  while (consp(state)) {
    object *pair = first(state);
    push(pair, *env);
    state = cdr(state);
  }
  // Add arguments to environment
  bool optional = false;
  while (params != NULL) {
    object *value;
    object *var = first(params);
    if (isbuiltin(var, OPTIONAL)) optional = true;
    else {
      if (consp(var)) {
        if (!optional) errorsym(name, PSTR("invalid default value"), var);
        if (args == NULL) value = eval(second(var), *env);
        else { value = first(args); args = cdr(args); }
        var = first(var);
        if (!symbolp(var)) errorsym(name, PSTR("illegal optional parameter"), var);
      } else if (!symbolp(var)) {
        errorsym(name, PSTR("illegal function parameter"), var);
      } else if (isbuiltin(var, AMPREST)) {
        params = cdr(params);
        var = first(params);
        value = args;
        args = NULL;
      } else {
        if (args == NULL) {
          if (optional) value = nil;
          else errorsym2(name, toofewargs);
        } else { value = first(args); args = cdr(args); }
      }
      push(cons(var,value), *env);
      if (trace) { pserial(' '); printobject(value, pserial); }
    }
    params = cdr(params);
  }
  if (args != NULL) errorsym2(name, toomanyargs);
  if (trace) { pserial(')'); pln(pserial); }
  // Do an implicit progn
  if (tc) push(nil, *env);
  return tf_progn(function, *env);
}

object *apply (object *function, object *args, object *env) {
  if (symbolp(function)) {
    builtin_t fname = builtin(function->name);
    if ((fname < ENDFUNCTIONS) && ((getminmax(fname)>>6) == FUNCTIONS)) {
      Context = fname;
      checkargs(args);
      return ((fn_ptr_type)lookupfn(fname))(args, env);
    } else function = eval(function, env);
  }
  if (consp(function) && isbuiltin(car(function), LAMBDA)) {
    object *result = closure(0, sym(NIL), function, args, &env);
    return eval(result, env);
  }
  if (consp(function) && isbuiltin(car(function), CLOSURE)) {
    function = cdr(function);
    object *result = closure(0, sym(NIL), function, args, &env);
    return eval(result, env);
  }
  error(PSTR("illegal function"), function);
  return NULL;
}"#)

(defparameter *in-place* '(

#"
// In-place operations"#

#+avr-nano ; no arrays or char
#"
/*
  place - returns a pointer to an object referenced in the second argument of an
  in-place operation such as setf.
*/
object **place (object *args, object *env, int *bit) {
  if (atom(args)) return &cdr(findvalue(args, env));
  object* function = first(args);
  if (symbolp(function)) {
    symbol_t sname = function->name;
    if (sname == sym(CAR) || sname == sym(FIRST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(canttakecar, value);
      return &car(value);
    }
    if (sname == sym(CDR) || sname == sym(REST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(canttakecdr, value);
      return &cdr(value);
    }
    if (sname == sym(NTH)) {
      int index = checkinteger(eval(second(args), env));
      object *list = eval(third(args), env);
      if (atom(list)) { Context = NTH; error(PSTR("second argument is not a list"), list); }
      int i = index;
      while (i > 0) {
        list = cdr(list);
        if (list == NULL) { Context = NTH; error(indexrange, number(index)); }
        i--;
      }
      return &car(list);
    }
  }
  error2(PSTR("illegal place"));
  return nil;
}"#

#-avr-nano
#"
/*
  place - returns a pointer to an object referenced in the second argument of an
  in-place operation such as setf. bit is used to indicate the bit position in a bit array
*/
object **place (object *args, object *env, int *bit) {
  *bit = -1;
  if (atom(args)) return &cdr(findvalue(args, env));
  object* function = first(args);
  if (symbolp(function)) {
    symbol_t sname = function->name;
    if (sname == sym(CAR) || sname == sym(FIRST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(canttakecar, value);
      return &car(value);
    }
    if (sname == sym(CDR) || sname == sym(REST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(canttakecdr, value);
      return &cdr(value);
    }
    if (sname == sym(NTH)) {
      int index = checkinteger(eval(second(args), env));
      object *list = eval(third(args), env);
      if (atom(list)) { Context = NTH; error(PSTR("second argument is not a list"), list); }
      int i = index;
      while (i > 0) {
        list = cdr(list);
        if (list == NULL) { Context = NTH; error(indexrange, number(index)); }
        i--;
      }
      return &car(list);
    }
    if (sname == sym(CHAR)) {
      int index = checkinteger(eval(third(args), env));
      object *string = checkstring(eval(second(args), env));
      object **loc = getcharplace(string, index, bit);
      if ((*loc) == NULL || (((((*loc)->chars)>>((-(*bit)-2)<<3)) & 0xFF) == 0)) { Context = CHAR; error(indexrange, number(index)); }
      return loc;
    }
    if (sname == sym(AREF)) {
      object *array = eval(second(args), env);
      if (!arrayp(array)) { Context = AREF; error(PSTR("first argument is not an array"), array); }
      return getarray(array, cddr(args), env, bit);
    }
  }
  error2(PSTR("illegal place"));
  return nil;
}"#

#+(and (not float) (not arrays))
#"
/*
  incfdecf() - Increments/decrements a place by 'increment', and returns the result.
  Calls place() to get a pointer to the numeric value.
*/
object *incfdecf (object *args, int increment, object *env) {
  int bit;
  object **loc = place(first(args), env, &bit);
  if (bit < -1) error2(notanumber);
  int result = checkinteger(*loc);
  args = cdr(args);
  if (args != NULL) increment = checkinteger(eval(first(args), env)) * increment;
  #if defined(checkoverflow)
  if (increment < 1) { if (INT_MIN - increment > result) error2(overflow); }
  else { if (INT_MAX - increment < result) error2(overflow); }
  #endif
  result = result + increment;
  *loc = number(result);
  return *loc;
}"#

#+(and (not float) arrays)
#"
/*
  incfdecf() - Increments/decrements a place by 'increment', and returns the result.
  Calls place() to get a pointer to the numeric value.
*/
object *incfdecf (object *args, int increment, object *env) {
  int bit;
  object **loc = place(first(args), env, &bit);
  if (bit < -1) error2(notanumber);
  int result = checkinteger(*loc);
  args = cdr(args);
  object *inc = (args != NULL) ? eval(first(args), env) : NULL;

  if (bit != -1) {
    if (inc != NULL) increment = checkbitvalue(inc);
    int newvalue = (((*loc)->integer)>>bit & 1) + increment;

    if (newvalue & ~1) error2(PSTR("result is not a bit value"));
    *loc = number((((*loc)->integer) & ~(1<<bit)) | newvalue<<bit);
    return number(newvalue);
  }

  if (inc != NULL) increment = increment * checkinteger(inc);
  #if defined(checkoverflow)
  if (increment < 1) { if (INT_MIN - increment > result) error2(overflow); }
  else { if (INT_MAX - increment < result) error2(overflow); }
  #endif
  result = result + increment;
  *loc = number(result);
  return *loc;
}"#

#"
// Checked car and cdr

/*
  carx - car with error checking
*/
object *carx (object *arg) {
  if (!listp(arg)) error(canttakecar, arg);
  if (arg == nil) return nil;
  return car(arg);
}

/*
  cdrx - cdr with error checking
*/
object *cdrx (object *arg) {
  if (!listp(arg)) error(canttakecdr, arg);
  if (arg == nil) return nil;
  return cdr(arg);
}

/*
  cxxxr - implements a general cxxxr function, 
  pattern is a sequence of bits 0b1xxx where x is 0 for a and 1 for d.
*/
object *cxxxr (object *args, uint8_t pattern) {
  object *arg = first(args);
  while (pattern != 1) {
    if ((pattern & 1) == 0) arg = carx(arg); else arg = cdrx(arg);
    pattern = pattern>>1;
  }
  return arg;
}#"

#"
// Mapping helper functions

/*
  mapcl - handles either mapc when mapl=false, or mapl when mapl=true
*/
object *mapcl (object *args, object *env, bool mapl) {
  object *function = first(args);
  args = cdr(args);
  object *result = first(args);
  protect(result);
  object *params = cons(NULL, NULL);
  protect(params);
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         unprotect(); unprotect();
         return result;
      }
      if (improperp(list)) error(notproper, list);
      object *item = mapl ? list : first(list);
      object *obj = cons(item, NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    apply(function, cdr(params), env);
  }
}"#

#"
/*
  mapcarfun - function specifying how to combine the results in mapcar
*/
void mapcarfun (object *result, object **tail) {
  object *obj = cons(result,NULL);
  cdr(*tail) = obj; *tail = obj;
}

/*
  mapcanfun - function specifying how to combine the results in mapcan
*/
void mapcanfun (object *result, object **tail) {
  if (cdr(*tail) != NULL) error(notproper, *tail);
  while (consp(result)) {
    cdr(*tail) = result; *tail = result;
    result = cdr(result);
  }
}"#

#"
/*
  mapcarcan - function used by marcar and mapcan when maplist=false, and maplist when maplist=true
  It takes the arguments, the env, a function specifying how the results are combined, and a bool.
*/
object *mapcarcan (object *args, object *env, mapfun_t fun, bool maplist) {
  object *function = first(args);
  args = cdr(args);
  object *params = cons(NULL, NULL);
  protect(params);
  object *head = cons(NULL, NULL);
  protect(head);
  object *tail = head;
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         unprotect(); unprotect();
         return cdr(head);
      }
      if (improperp(list)) error(notproper, list);
      object *item = maplist ? list : first(list);
      object *obj = cons(item, NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    object *result = apply(function, cdr(params), env);
    fun(result, &tail);
  }
}"#

#-avr-nano
#"
/*
  dobody - function used by do when star=false and do* when star=true
*/
object *dobody (object *args, object *env, bool star) {
  object *varlist = first(args), *endlist = second(args);
  object *head = cons(NULL, NULL);
  protect(head);
  object *ptr = head;
  object *newenv = env;
  while (varlist != NULL) {
    object *varform = first(varlist);
    object *var, *init = NULL, *step = NULL;
    if (atom(varform)) var = varform;
    else {
      var = first(varform);
      varform = cdr(varform);
      if (varform != NULL) {  
        init = eval(first(varform), env);
        varform = cdr(varform);
        if (varform != NULL) step = cons(first(varform), NULL);
      }
    }  
    object *pair = cons(var, init);
    push(pair, newenv);
    if (star) env = newenv;
    object *cell = cons(cons(step, pair), NULL);
    cdr(ptr) = cell; ptr = cdr(ptr);
    varlist = cdr(varlist);
  }
  env = newenv;
  head = cdr(head);
  object *endtest = first(endlist), *results = cdr(endlist);
  while (eval(endtest, env) == NULL) {
    object *forms = cddr(args);
    while (forms != NULL) {
    object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      forms = cdr(forms);
    }
    object *varlist = head;
    int count = 0;
    while (varlist != NULL) {
      object *varform = first(varlist);
      object *step = car(varform), *pair = cdr(varform);
      if (step != NULL) {
        object *val = eval(first(step), env);
        if (star) {
          cdr(pair) = val;
        } else {
          push(val, GCStack);
          push(pair, GCStack);
          count++;
        }
      } 
      varlist = cdr(varlist);
    }
    while (count > 0) {
      cdr(car(GCStack)) = car(cdr(GCStack));
      pop(GCStack); pop(GCStack);
      count--;
    }
  }
  unprotect();
  return eval(tf_progn(results, env), env);
}"#))
        