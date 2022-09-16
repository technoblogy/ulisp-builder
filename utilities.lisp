;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

(defparameter *error-handling*

  '(#"
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
}

/*
  error - prints an error message and reenters the REPL.
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
  error - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string", where fname is the name of the user Lisp function in which the error occurred.
*/
void errorsym2 (symbol_t fname, PGM_P string) {
  errorsub(fname, string);
  errorend();
}

/*
  error - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string: symbol", where fname is the name of the built-in Lisp function in which the error occurred,
  and symbol is the object generating the error.
*/
void error (builtin_t fname, PGM_P string, object *symbol) {
  errorsym(sym(fname), string, symbol);
}

/*
  error - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string", where fname is the name of the built-in Lisp function in which the error occurred.
*/
void error2 (builtin_t fname, PGM_P string) {
  errorsym2(sym(fname), string);
}

void errorend () { pln(pserial); GCStack = NULL; longjmp(exception, 1); }

/*
  formaterr - displays a format error with a ^ pointing to the error
*/
void formaterr (object *formatstr, PGM_P string, uint8_t p) {
  pln(pserial); indent(4, ' ', pserial); printstring(formatstr, pserial); pln(pserial);
  indent(p+5, ' ', pserial); pserial('^');
  error2(FORMAT, string);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

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
  if (Freespace == 0) error2(NIL, PSTR("no room"));
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

 #+avr
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

 #+(or arm esp riscv)
 #"
/*
  eqsymbols - compares the long string/symbol obj with the string in buffer.
*/
bool eqsymbols (object *obj, char *buffer) {
  object *arg = cdr(obj);
  int i = 0;
  while (!(arg == NULL && buffer[i] == 0)) {
    if (arg == NULL || buffer[i] == 0 ||
      arg->chars != (buffer[i]<<24 | buffer[i+1]<<16 | buffer[i+2]<<8 | buffer[i+3])) return false;
    arg = car(arg);
    i = i + 4;
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

#+(and avr (not arrays))
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

#-(and avr (not arrays))
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
  if (tracing(name)) error(TRACE, PSTR("already being traced"), symbol(name));
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == 0) { TraceFn[i] = name; TraceDepth[i] = 0; return; }
    i++;
  }
  error2(TRACE, PSTR("already tracing 3 functions"));
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
  error(UNTRACE, PSTR("not tracing"), symbol(name));
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
  if (n >= 1 && n <= 9) return '0'+n-1;
  if (n >= 11 && n <= 36) return 'a'+n-11;
  if (n == 37) return '-'; if (n == 38) return '*'; if (n == 39) return '$';
  return 0;
}"#

  #+(or avr msp430 badge)
  #"
/*
  pack40 - packs three radix40-encoded characters from buffer into a 16-bit number and returns it.
*/
uint16_t pack40 (char *buffer) {
  return (((toradix40(buffer[0]) * 40) + toradix40(buffer[1])) * 40 + toradix40(buffer[2]));
}

/*
  valid40 - returns true if the symbol in buffer can be encoded as three radix40-encoded characters.
*/
bool valid40 (char *buffer) {
 return (toradix40(buffer[0]) >= 11 && toradix40(buffer[1]) >= 0 && toradix40(buffer[2]) >= 0);
}"#

  #+(or arm stm32 esp riscv)
  #"
/*
  pack40 - packs six radix40-encoded characters from buffer into a 32-bit number and returns it.
*/
uint32_t pack40 (char *buffer) {
  int x = 0;
  for (int i=0; i<6; i++) x = x * 40 + toradix40(buffer[i]);
  return x;
}

/*
  valid40 - returns true if the symbol in buffer can be encoded as six radix40-encoded characters.
*/
bool valid40 (char *buffer) {
  if (toradix40(buffer[0]) < 11) return false;
  for (int i=1; i<6; i++) if (toradix40(buffer[i]) < 0) return false;
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
int checkinteger (builtin_t name, object *obj) {
  if (!integerp(obj)) error(name, notaninteger, obj);
  return obj->integer;
}"#

    #+arrays
    #"
/*
  checkbitvalue - check that obj is an integer equal to 0 or 1 and return it
*/
int checkbitvalue (builtin_t name, object *obj) {
  if (!integerp(obj)) error(name, notaninteger, obj);
  int n = obj->integer;
  if (n & ~1) error(name, PSTR("argument is not a bit value"), obj);
  return n;
}"#

    #+float
    #"
/*
  checkintfloat - check that obj is an integer or floating-point number and return the number
*/
float checkintfloat (builtin_t name, object *obj){
  if (integerp(obj)) return obj->integer;
  if (!floatp(obj)) error(name, notanumber, obj);
  return obj->single_float;
}"#

    #"
/*
  checkchar - check that obj is a character and return the character
*/
int checkchar (builtin_t name, object *obj) {
  if (!characterp(obj)) error(name, PSTR("argument is not a character"), obj);
  return obj->chars;
}

/*
  checkstring - check that obj is a string
*/
object *checkstring (builtin_t name, object *obj) {
  if (!stringp(obj)) error(name, notastring, obj);
  return obj;
}

int isstream (object *obj){
  if (!streamp(obj)) error(NIL, PSTR("not a stream"), obj);
  return obj->integer;
}

int isbuiltin (object *obj, builtin_t n) {
  return symbolp(obj) && obj->name == sym(n);
}

bool builtinp (symbol_t name) {
  return (untwist(name) > BUILTINS && untwist(name) < ENDFUNCTIONS+BUILTINS);
}

int keywordp (object *obj) {
  if (!symbolp(obj)) return false;
  builtin_t name = builtin(obj->name);
  return ((name > KEYWORDS) && (name < USERFUNCTIONS));
}

int checkkeyword (builtin_t name, object *obj) {
  if (!keywordp(obj)) error(name, PSTR("argument is not a keyword"), obj);
  builtin_t kname = builtin(obj->name);
  uint8_t context = getminmax(kname);
  if (context != 0 && context != name) error(name, invalidkey, obj);
  return ((int)lookupfn(kname));
}

/*
  checkargs - checks that the number of objects in the list args
  is within the range specified in the symbol lookup table
*/
void checkargs (builtin_t name, object *args) {
  int nargs = listlength(name, args);
  checkminmax(name, nargs);
}"#

    #-float
    #"
/*
  eq - implements Lisp eq
*/
int eq (object *arg1, object *arg2) {
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
int eq (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
  if (floatp(arg1) && floatp(arg2)) return true; // Same float
  if (characterp(arg1) && characterp(arg2)) return true;  // Same character
  return false;
}"#

    #"
int listlength (builtin_t name, object *list) {
  int length = 0;
  while (list != NULL) {
    if (improperp(list)) error2(name, notproper);
    list = cdr(list);
    length++;
  }
  return length;
}"#

#"
// Mathematical helper functions"#

 #+avr
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
    fresult = fresult + checkintfloat(ADD, arg);
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
    fresult = fresult - checkintfloat(SUBTRACT, arg);
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
  else error(SUBTRACT, notanumber, arg);
  return nil;
}

/*
  multiply_floats - used by fn_multiply
  Converts the numbers in args to floats, adds them to fresult, and returns the result as a Lisp float.
*/
object *multiply_floats (object *args, float fresult) {
  while (args != NULL) {
   object *arg = car(args);
    fresult = fresult * checkintfloat(MULTIPLY, arg);
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
    float f = checkintfloat(DIVIDE, arg);
    if (f == 0.0) error2(DIVIDE, divisionbyzero);
    fresult = fresult / f;
    args = cdr(args);
  }
  return makefloat(fresult);
}

/*
  myround - rounds
  Returns t if the argument is a floating-point number.
*/
int myround (float number) {
  return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
}"#

 #+avr
#"
/*
  compare - a generic compare function
  Used to implement the other comparison functions.
  If lt is true the result is true if each argument is less than the next argument.
  If gt is true the result is true if each argument is greater than the next argument.
  If eq is true the result is true if each argument is equal to the next argument.
*/
object *compare (builtin_t name, object *args, bool lt, bool gt, bool eq) {
  int arg1 = checkinteger(name, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(name, first(args));
    if (!lt && (arg1 < arg2)) return nil;
    if (!eq && (arg1 == arg2)) return nil;
    if (!gt && (arg1 > arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}"#

#-avr
#"
/*
  compare - a generic compare function
  Used to implement the other comparison functions.
  If lt is true the result is true if each argument is less than the next argument.
  If gt is true the result is true if each argument is greater than the next argument.
  If eq is true the result is true if each argument is equal to the next argument.
*/
object *compare (builtin_t name, object *args, bool lt, bool gt, bool eq) {
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!lt && ((arg1->integer) < (arg2->integer))) return nil;
      if (!eq && ((arg1->integer) == (arg2->integer))) return nil;
      if (!gt && ((arg1->integer) > (arg2->integer))) return nil;
    } else {
      if (!lt && (checkintfloat(name, arg1) < checkintfloat(name, arg2))) return nil;
      if (!eq && (checkintfloat(name, arg1) == checkintfloat(name, arg2))) return nil;
      if (!gt && (checkintfloat(name, arg1) > checkintfloat(name, arg2))) return nil;
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

(defparameter *association-lists* #"
// Association lists

/*
  assoc - looks for key in an association list and returns the matching pair, or nil if not found
*/
object *assoc (object *key, object *list) {
  while (list != NULL) {
    if (improperp(list)) error(ASSOC, notproper, list);
    object *pair = first(list);
    if (!listp(pair)) error(ASSOC, PSTR("element is not a list"), pair);
    if (pair != NULL && eq(key,car(pair))) return pair;
    list = cdr(list);
  }
  return nil;
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
// Array utilities

/*
  nextpower2 - returns the smallest power of 2 that is equal to or greater than n
*/
int nextpower2 (int n) {
  n--; n |= n >> 1; n |= n >> 2; n |= n >> 4;
  n |= n >> 8; n |= n >> 16; n++;
  return n<2 ? 2 : n;
}

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

object *makearray (builtin_t name, object *dims, object *def, bool bitp) {
  int size = 1;
  object *dimensions = dims;
  while (dims != NULL) {
    int d = car(dims)->integer;
    if (d < 0) error2(name, PSTR("dimension can't be negative"));
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
object **getarray (builtin_t name, object *array, object *subs, object *env, int *bit) {
  int index = 0, size = 1, s;
  *bit = -1;
  bool bitp = false;
  object *dims = cddr(array);
  while (dims != NULL && subs != NULL) {
    int d = car(dims)->integer;
    if (d < 0) { d = -d; bitp = true; }
    if (env) s = checkinteger(name, eval(car(subs), env)); else s = checkinteger(name, car(subs));
    if (s < 0 || s >= d) error(name, PSTR("subscript out of range"), car(subs));
    size = size * d;
    index = index * d + s;
    dims = cdr(dims); subs = cdr(subs);
  }
  if (dims != NULL) error2(name, PSTR("too few subscripts"));
  if (subs != NULL) error2(name, PSTR("too many subscripts"));
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
    if (!consp(args)) error2(NIL, PSTR("initial contents don't match array type"));
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
    if (!listp(list)) error2(NIL, PSTR("initial contents don't match array type"));
    int l = listlength(NIL, list);
    if (dims == NULL) { dims = cons(number(l), NULL); head = dims; }
    else { cdr(dims) = cons(number(l), NULL); dims = cdr(dims); }
    size = size * l;
    if (list != NULL) list = car(list);
  }
  object *array = makearray(NIL, head, NULL, false);
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
  while (!issp(ch) && ch != ')' && ch != '(') {
    if (ch != '0' && ch != '1') error2(NIL, PSTR("illegal character in bit array"));
    object *cell = cons(number(ch - '0'), NULL);
    if (head == NULL) head = cell;
    else tail->cdr = cell;
    tail = cell;
    ch = gfun();
  }
  LastChar = ch;
  int size = listlength(NIL, head);
  object *array = makearray(NIL, cons(number(size), NULL), 0, true);
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

object *startstring (builtin_t name) {
  (void) name;
  object *string = newstring();
  GlobalString = string;
  GlobalStringTail = string;
  return string;
}"#

  #+(or avr msp430 badge)
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
    (*tail)->chars = (*tail)->chars | ch; return;
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
void buildstring (char ch, object **tail) {
  object *cell;
  if (cdr(*tail) == NULL) {
    cell = myalloc(); cdr(*tail) = cell;
  } else if (((*tail)->chars & 0xFFFFFF) == 0) {
    (*tail)->chars = (*tail)->chars | ch<<16; return;
  } else if (((*tail)->chars & 0xFFFF) == 0) {
    (*tail)->chars = (*tail)->chars | ch<<8; return;
  } else if (((*tail)->chars & 0xFF) == 0) {
    (*tail)->chars = (*tail)->chars | ch; return;
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
object *readstring (uint8_t delim, gfun_t gfun) {
  object *obj = newstring();
  object *tail = obj;
  int ch = gfun();
  if (ch == -1) return nil;
  while ((ch != delim) && (ch != -1)) {
    if (ch == '\\') ch = gfun();
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
}

/*
  nthchar - returns the nth character from a Lisp string
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
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
}

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
}

/*
  stringcompare - a generic string compare function
  Used to implement the other string comparison functions.
  If lt is true the result is true if each argument is less than the next argument.
  If gt is true the result is true if each argument is greater than the next argument.
  If eq is true the result is true if each argument is equal to the next argument.
*/
bool stringcompare (builtin_t name, object *args, bool lt, bool gt, bool eq) {
  object *arg1 = checkstring(name, first(args));
  object *arg2 = checkstring(name, second(args));
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

#+(and doc avr)
#"
/*
  documentation - returns the documentation string of a built-in or user-defined function.
*/
object *documentation (builtin_t name, object *arg, object *env) {
  if (!symbolp(arg)) error(name, notasymbol, arg);
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
  #if defined(CPU_ATmega4809)
  return lispstring(docstring);
  #else
  object *obj = startstring(PRINCTOSTRING);
  pfstring(docstring, pstr);
  return obj;
  #endif
}"#

#+(and doc (not avr))
#"
/*
  documentation - returns the documentation string of a built-in or user-defined function.
*/
object *documentation (builtin_t name, object *arg, object *env) {
  if (!symbolp(arg)) error(name, notasymbol, arg);
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
  return lispstring(docstring);
}"#

  #+ethernet
  #"
/*
  cstring - converts a Lisp string to a C string in buffer and returns buffer
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
char *cstring (builtin_t name, object *form, char *buffer, int buflen) {
  form = cdr(checkstring(name, form));
  int index = 0;
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (ch) {
        if (index >= buflen-1) error2(NIL, PSTR("no room for string"));
        buffer[index++] = ch;
      }
    }
    form = car(form);
  }
  buffer[index] = '\0';
  return buffer;
}"#

#+ethernet
#"
/*
  ipstring - parses an IP address from a Lisp string and returns it as an IPAddress type (uint32_t)
  Handles Lisp strings packed two characters per 16-bit word, or four characters per 32-bit word
*/
IPAddress ipstring (builtin_t name, object *form) {
  form = cdr(checkstring(name, form));
  int p = 0;
  union { uint32_t ipaddress; uint8_t ipbytes[4]; } ;
  ipaddress = 0;
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (ch) {
        if (ch == '.') { p++; if (p > 3) error2(name, PSTR("illegal IP address")); }
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
  return (findpair(var, env) != NULL);
}

/*
  findvalue - returns the value bound to variable var, or givesn an error of unbound
*/
object *findvalue (builtin_t name, object *var, object *env) {
  object *pair = findpair(var, env);
  if (pair == NULL) error(name, PSTR("unknown variable"), var);
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

object *apply (builtin_t name, object *function, object *args, object *env) {
  if (symbolp(function)) {
    builtin_t fname = builtin(function->name);
    if ((fname > FUNCTIONS) && (fname < KEYWORDS)) {
      checkargs(fname, args);
      return ((fn_ptr_type)lookupfn(fname))(args, env);
    } else function = eval(function, env);
  }
  if (consp(function) && isbuiltin(car(function), LAMBDA)) {
    object *result = closure(0, sym(name), function, args, &env);
    return eval(result, env);
  }
  if (consp(function) && isbuiltin(car(function), CLOSURE)) {
    function = cdr(function);
    object *result = closure(0, sym(name), function, args, &env);
    return eval(result, env);
  }
  error(name, PSTR("illegal function"), function);
  return NULL;
}"#)

(defparameter *in-place* '(

#"
// In-place operations"#

#+(and avr (not arrays))
#"
/*
  place - returns a pointer to an object referenced in the second argument of an
  in-place operation such as setf.
*/
object **place (builtin_t name, object *args, object *env) {
  if (atom(args)) return &cdr(findvalue(name, args, env));
  object* function = first(args);
  if (symbolp(function)) {
    symbol_t sname = function->name;
    if (sname == sym(CAR) || sname == sym(FIRST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(name, canttakecar, value);
      return &car(value);
    }
    if (sname == sym(CDR) || sname == sym(REST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(name, canttakecdr, value);
      return &cdr(value);
    }
    if (sname == sym(NTH)) {
      int index = checkinteger(NTH, eval(second(args), env));
      object *list = eval(third(args), env);
      if (atom(list)) error(name, PSTR("second argument to nth is not a list"), list);
      while (index > 0) {
        list = cdr(list);
        if (list == NULL) error2(name, PSTR("index to nth is out of range"));
        index--;
      }
      return &car(list);
    }
  }
  error2(name, PSTR("illegal place"));
  return nil;
}"#

#-(and avr (not arrays))
#"
/*
  place - returns a pointer to an object referenced in the second argument of an
  in-place operation such as setf. bit is used to indicate the bit position in a bit array
*/
object **place (builtin_t name, object *args, object *env, int *bit) {
  *bit = -1;
  if (atom(args)) return &cdr(findvalue(name, args, env));
  object* function = first(args);
  if (symbolp(function)) {
    symbol_t sname = function->name;
    if (sname == sym(CAR) || sname == sym(FIRST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(name, canttakecar, value);
      return &car(value);
    }
    if (sname == sym(CDR) || sname == sym(REST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(name, canttakecdr, value);
      return &cdr(value);
    }
    if (sname == sym(NTH)) {
      int index = checkinteger(NTH, eval(second(args), env));
      object *list = eval(third(args), env);
      if (atom(list)) error(name, PSTR("second argument to nth is not a list"), list);
      while (index > 0) {
        list = cdr(list);
        if (list == NULL) error2(name, PSTR("index to nth is out of range"));
        index--;
      }
      return &car(list);
    }
    if (sname == sym(AREF)) {
      object *array = eval(second(args), env);
      if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
      return getarray(AREF, array, cddr(args), env, bit);
    }
  }
  error2(name, PSTR("illegal place"));
  return nil;
}"#

#+(and (not float) (not arrays))
#"
/*
  incfdecf() - Increments/decrements a place by 'increment', and returns the result.
  Calls place() to get a pointer to the numeric value.
*/
object *incfdecf (builtin_t name, object *args, int increment, object *env) {
  checkargs(name, args);
  object **loc = place(name, first(args), env);
  int result = checkinteger(name, *loc);
  args = cdr(args);
  if (args != NULL) increment = checkinteger(name, eval(first(args), env)) * increment;
  #if defined(checkoverflow)
  if (increment < 1) { if (INT_MIN - increment > result) error2(name, overflow); }
  else { if (INT_MAX - increment < result) error2(name, overflow); }
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
object *incfdecf (builtin_t name, object *args, int increment, object *env) {
  int bit;
  checkargs(name, args);
  object **loc = place(name, first(args), env, &bit);
  int result = checkinteger(name, *loc);
  args = cdr(args);
  object *inc = (args != NULL) ? eval(first(args), env) : NULL;

  if (bit != -1) {
    if (inc != NULL) increment = checkbitvalue(name, inc);
    int newvalue = (((*loc)->integer)>>bit & 1) + increment;

    if (newvalue & ~1) error2(name, PSTR("result is not a bit value"));
    *loc = number((((*loc)->integer) & ~(1<<bit)) | newvalue<<bit);
    return number(newvalue);
  }

  if (inc != NULL) increment = increment * checkinteger(name, inc);
  #if defined(checkoverflow)
  if (increment < 1) { if (INT_MIN - increment > result) error2(name, overflow); }
  else { if (INT_MAX - increment < result) error2(name, overflow); }
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
  if (!listp(arg)) error(NIL, canttakecar, arg);
  if (arg == nil) return nil;
  return car(arg);
}

/*
  cdrx - cdr with error checking
*/
object *cdrx (object *arg) {
  if (!listp(arg)) error(NIL, canttakecdr, arg);
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
}

// Mapping helper functions

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
  if (cdr(*tail) != NULL) error(MAPCAN, notproper, *tail);
  while (consp(result)) {
    cdr(*tail) = result; *tail = result;
    result = cdr(result);
  }
}

/*
  mapcarcan - function used by marcar and mapcan
  It takes the name of the function, the arguments, the env, and a function specifying how the results are combined.
*/
object *mapcarcan (builtin_t name, object *args, object *env, mapfun_t fun) {
  object *function = first(args);
  args = cdr(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  object *head = cons(NULL, NULL);
  push(head,GCStack);
  object *tail = head;
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         pop(GCStack);
         return cdr(head);
      }
      if (improperp(list)) error(name, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    object *result = apply(name, function, cdr(params), env);
    fun(result, &tail);
  }
}"#))

(defparameter *i2c-interface* '(

#+(and avr (not badge))
#"
// I2C interface for AVR platforms, uses much less RAM than Arduino Wire

#if defined(CPU_ATmega328P)
uint8_t const TWI_SDA_PIN = 18;
uint8_t const TWI_SCL_PIN = 19;
#elif defined(CPU_ATmega1280) || defined(CPU_ATmega2560)
uint8_t const TWI_SDA_PIN = 20;
uint8_t const TWI_SCL_PIN = 21;
#elif defined(CPU_ATmega644P) || defined(CPU_ATmega1284P)
uint8_t const TWI_SDA_PIN = 17;
uint8_t const TWI_SCL_PIN = 16;
#elif defined(CPU_ATmega32U4)
uint8_t const TWI_SDA_PIN = 6;
uint8_t const TWI_SCL_PIN = 5;
#endif

#if defined(CPU_ATmega4809) || defined(CPU_AVR128DX48)
uint32_t const FREQUENCY = 400000L;  // Hardware I2C clock in Hz
uint32_t const T_RISE = 300L;        // Rise time
#else
uint32_t const F_TWI = 400000L;  // Hardware I2C clock in Hz
uint8_t const TWSR_MTX_DATA_ACK = 0x28;
uint8_t const TWSR_MTX_ADR_ACK = 0x18;
uint8_t const TWSR_MRX_ADR_ACK = 0x40;
uint8_t const TWSR_START = 0x08;
uint8_t const TWSR_REP_START = 0x10;
uint8_t const I2C_READ = 1;
uint8_t const I2C_WRITE = 0;
#endif

void I2Cinit (bool enablePullup) {
#if defined(CPU_ATmega4809) || defined(CPU_AVR128DX48)
  #if defined(CPU_ATmega4809)
  if (enablePullup) {
    pinMode(SDA, INPUT_PULLUP);
    pinMode(SCL, INPUT_PULLUP);
  }
  #else
  (void) enablePullup;
  #endif
  uint32_t baud = ((F_CPU/FREQUENCY) - (((F_CPU*T_RISE)/1000)/1000)/1000 - 10)/2;
  TWI0.MBAUD = (uint8_t)baud;
  TWI0.MCTRLA = TWI_ENABLE_bm;                                    // Enable as master, no interrupts
  TWI0.MSTATUS = TWI_BUSSTATE_IDLE_gc;
#else
  TWSR = 0;                        // no prescaler
  TWBR = (F_CPU/F_TWI - 16)/2;     // set bit rate factor
  if (enablePullup) {
    digitalWrite(SDA, HIGH);
    digitalWrite(SCL, HIGH);
  }
#endif
}

int I2Cread () {
#if defined(CPU_ATmega4809) || defined(CPU_AVR128DX48)
  if (I2Ccount != 0) I2Ccount--;
  while (!(TWI0.MSTATUS & TWI_RIF_bm));                           // Wait for read interrupt flag
  uint8_t data = TWI0.MDATA;
  // Check slave sent ACK?
  if (I2Ccount != 0) TWI0.MCTRLB = TWI_MCMD_RECVTRANS_gc;         // ACK = more bytes to read
  else TWI0.MCTRLB = TWI_ACKACT_NACK_gc;                          // Send NAK
  return data;
#else
  if (I2Ccount != 0) I2Ccount--;
  TWCR = 1<<TWINT | 1<<TWEN | ((I2Ccount == 0) ? 0 : (1<<TWEA));
  while (!(TWCR & 1<<TWINT));
  return TWDR;
#endif
}

bool I2Cwrite (uint8_t data) {
#if defined(CPU_ATmega4809) || defined(CPU_AVR128DX48)
  TWI0.MCTRLB = TWI_MCMD_RECVTRANS_gc;                            // Prime transaction
  TWI0.MDATA = data;                                              // Send data
  while (!(TWI0.MSTATUS & TWI_WIF_bm));                           // Wait for write to complete

  if (TWI0.MSTATUS & (TWI_ARBLOST_bm | TWI_BUSERR_bm)) return false; // Fails if bus error or arblost
  return !(TWI0.MSTATUS & TWI_RXACK_bm);                          // Returns true if slave gave an ACK
#else
  TWDR = data;
  TWCR = 1<<TWINT | 1 << TWEN;
  while (!(TWCR & 1<<TWINT));
  return (TWSR & 0xF8) == TWSR_MTX_DATA_ACK;
#endif
}

bool I2Cstart (uint8_t address, uint8_t read) {
#if defined(CPU_ATmega4809) || defined(CPU_AVR128DX48)
  TWI0.MADDR = address<<1 | read;                                 // Send START condition
  while (!(TWI0.MSTATUS & (TWI_WIF_bm | TWI_RIF_bm)));            // Wait for write or read interrupt flag
  if (TWI0.MSTATUS & TWI_ARBLOST_bm) {                            // Arbitration lost or bus error
    while (!((TWI0.MSTATUS & TWI_BUSSTATE_gm) == TWI_BUSSTATE_IDLE_gc)); // Wait for bus to return to idle state
    return false;
  } else if (TWI0.MSTATUS & TWI_RXACK_bm) {                       // Address not acknowledged by client
    TWI0.MCTRLB |= TWI_MCMD_STOP_gc;                              // Send stop condition
    while (!((TWI0.MSTATUS & TWI_BUSSTATE_gm) == TWI_BUSSTATE_IDLE_gc)); // Wait for bus to return to idle state
    return false;
  }
  return true;                                                    // Return true if slave gave an ACK
#else
  uint8_t addressRW = address<<1 | read;
  TWCR = 1<<TWINT | 1<<TWSTA | 1<<TWEN;    // Send START condition
  while (!(TWCR & 1<<TWINT));
  if ((TWSR & 0xF8) != TWSR_START && (TWSR & 0xF8) != TWSR_REP_START) return false;
  TWDR = addressRW;  // send device address and direction
  TWCR = 1<<TWINT | 1<<TWEN;
  while (!(TWCR & 1<<TWINT));
  if (addressRW & I2C_READ) return (TWSR & 0xF8) == TWSR_MRX_ADR_ACK;
  else return (TWSR & 0xF8) == TWSR_MTX_ADR_ACK;
#endif
}

bool I2Crestart (uint8_t address, uint8_t read) {
  return I2Cstart(address, read);
}

void I2Cstop (uint8_t read) {
#if defined(CPU_ATmega4809) || defined(CPU_AVR128DX48)
  (void) read;
  TWI0.MCTRLB |= TWI_MCMD_STOP_gc;                                // Send STOP
  while (!((TWI0.MSTATUS & TWI_BUSSTATE_gm) == TWI_BUSSTATE_IDLE_gc)); // Wait for bus to return to idle state
#else
  (void) read;
  TWCR = 1<<TWINT | 1<<TWEN | 1<<TWSTO;
  while (TWCR & 1<<TWSTO); // wait until stop and bus released
#endif
}"#

#+arm
#"
// I2C interface for up to two ports, using Arduino Wire

void I2Cinit (TwoWire *port, bool enablePullup) {
  (void) enablePullup;
  port->begin();
}

int I2Cread (TwoWire *port) {
  return port->read();
}

void I2Cwrite (TwoWire *port, uint8_t data) {
  port->write(data);
}

bool I2Cstart (TwoWire *port, uint8_t address, uint8_t read) {
 int ok = true;
 if (read == 0) {
   port->beginTransmission(address);
   ok = (port->endTransmission(true) == 0);
   port->beginTransmission(address);
 }
 else port->requestFrom(address, I2Ccount);
 return ok;
}

bool I2Crestart (TwoWire *port, uint8_t address, uint8_t read) {
  int error = (port->endTransmission(false) != 0);
  if (read == 0) port->beginTransmission(address);
  else port->requestFrom(address, I2Ccount);
  return error ? false : true;
}

void I2Cstop (TwoWire *port, uint8_t read) {
  if (read == 0) port->endTransmission(); // Check for error?
}"#

#-(or avr badge arm)
#"
// I2C interface for one port, using Arduino Wire

void I2Cinit (bool enablePullup) {
  (void) enablePullup;
  Wire.begin();
}

int I2Cread () {
  return Wire.read();
}

void I2Cwrite (uint8_t data) {
  Wire.write(data);
}

bool I2Cstart (uint8_t address, uint8_t read) {
 int ok = true;
 if (read == 0) {
   Wire.beginTransmission(address);
   ok = (Wire.endTransmission(true) == 0);
   Wire.beginTransmission(address);
 }
 else Wire.requestFrom(address, I2Ccount);
 return ok;
}

bool I2Crestart (uint8_t address, uint8_t read) {
  int error = (Wire.endTransmission(false) != 0);
  if (read == 0) Wire.beginTransmission(address);
  else Wire.requestFrom(address, I2Ccount);
  return error ? false : true;
}

void I2Cstop (uint8_t read) {
  if (read == 0) Wire.endTransmission(); // Check for error?
}"#

#+badge
#"
// I2C interface for AVR platforms, uses much less RAM than Arduino Wire

uint8_t const TWI_SDA_PIN = 17;
uint8_t const TWI_SCL_PIN = 16;

uint32_t const F_TWI = 400000L;  // Hardware I2C clock in Hz
uint8_t const TWSR_MTX_DATA_ACK = 0x28;
uint8_t const TWSR_MTX_ADR_ACK = 0x18;
uint8_t const TWSR_MRX_ADR_ACK = 0x40;
uint8_t const TWSR_START = 0x08;
uint8_t const TWSR_REP_START = 0x10;
uint8_t const I2C_READ = 1;
uint8_t const I2C_WRITE = 0;

void I2Cinit (bool enablePullup) {
  TWSR = 0;                        // no prescaler
  TWBR = (F_CPU/F_TWI - 16)/2;     // set bit rate factor
  if (enablePullup) {
    digitalWrite(TWI_SDA_PIN, HIGH);
    digitalWrite(TWI_SCL_PIN, HIGH);
  }
}

int I2Cread () {
  if (I2Ccount != 0) I2Ccount--;
  TWCR = 1<<TWINT | 1<<TWEN | ((I2Ccount == 0) ? 0 : (1<<TWEA));
  while (!(TWCR & 1<<TWINT));
  return TWDR;
}

bool I2Cwrite (uint8_t data) {
  TWDR = data;
  TWCR = 1<<TWINT | 1 << TWEN;
  while (!(TWCR & 1<<TWINT));
  return (TWSR & 0xF8) == TWSR_MTX_DATA_ACK;
}

bool I2Cstart (uint8_t address, uint8_t read) {
  uint8_t addressRW = address<<1 | read;
  TWCR = 1<<TWINT | 1<<TWSTA | 1<<TWEN;    // Send START condition
  while (!(TWCR & 1<<TWINT));
  if ((TWSR & 0xF8) != TWSR_START && (TWSR & 0xF8) != TWSR_REP_START) return false;
  TWDR = addressRW;  // send device address and direction
  TWCR = 1<<TWINT | 1<<TWEN;
  while (!(TWCR & 1<<TWINT));
  if (addressRW & I2C_READ) return (TWSR & 0xF8) == TWSR_MRX_ADR_ACK;
  else return (TWSR & 0xF8) == TWSR_MTX_ADR_ACK;
}

bool I2Crestart (uint8_t address, uint8_t read) {
  return I2Cstart(address, read);
}

void I2Cstop (uint8_t read) {
  (void) read;
  TWCR = 1<<TWINT | 1<<TWEN | 1<<TWSTO;
  while (TWCR & 1<<TWSTO); // wait until stop and bus released
}"#))
        