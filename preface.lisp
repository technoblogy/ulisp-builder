;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

(defparameter *macros* '(

#+avr-nano
#"
// C Macros

#define nil                NULL
#define car(x)             (((object *) (x))->car)
#define cdr(x)             (((object *) (x))->cdr)

#define first(x)           car(x)
#define rest(x)            cdr(x)
#define second(x)          first(rest(x))
#define cddr(x)            cdr(cdr(x))
#define third(x)           first(cddr(x))

#define push(x, y)         ((y) = cons((x),(y)))
#define pop(y)             ((y) = cdr(y))

#define protect(y)         push((y), GCStack)
#define unprotect()        pop(GCStack)

#define integerp(x)        ((x) != NULL && (x)->type == NUMBER)
#define symbolp(x)         ((x) != NULL && (x)->type == SYMBOL)
#define stringp(x)         ((x) != NULL && (x)->type == STRING)
#define characterp(x)      ((x) != NULL && (x)->type == CHARACTER)
#define streamp(x)         ((x) != NULL && (x)->type == STREAM)

#define mark(x)            (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x)          (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x)          ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT            1

#define setflag(x)         (Flags = Flags | 1<<(x))
#define clrflag(x)         (Flags = Flags & ~(1<<(x)))
#define tstflag(x)         (Flags & 1<<(x))

#define issp(x)            (x == ' ' || x == '\n' || x == '\r' || x == '\t')
#define isbr(x)            (x == ')' || x == '(' || x == '"' || x == '#' || x == '\'')
#define fntype(x)          (getminmax((uint16_t)(x))>>6)
#define longsymbolp(x)     (((x)->name & 0x03) == 0)
#define longnamep(x)       (((x) & 0x03) == 0)
#define twist(x)           ((uint16_t)((x)<<2) | (((x) & 0xC000)>>14))
#define untwist(x)         (((x)>>2 & 0x3FFF) | ((x) & 0x03)<<14)
#define arraysize(x)       (sizeof(x) / sizeof(x[0]))
#define stringifyX(x)      #x
#define stringify(x)       stringifyX(x)
#define PACKEDS            17600
#define BUILTINS           64000
#define ENDFUNCTIONS       1536

#define SDCARD_SS_PIN 10

#if defined(CPU_ATmega4809) || defined(CPU_ATtiny3227)
#define PROGMEM
#define PSTR(s) (s)
#endif"#

#+avr
#"
// C Macros

#define nil                NULL
#define car(x)             (((object *) (x))->car)
#define cdr(x)             (((object *) (x))->cdr)

#define first(x)           car(x)
#define rest(x)            cdr(x)
#define second(x)          first(rest(x))
#define cddr(x)            cdr(cdr(x))
#define third(x)           first(cddr(x))

#define push(x, y)         ((y) = cons((x),(y)))
#define pop(y)             ((y) = cdr(y))

#define protect(y)         push((y), GCStack)
#define unprotect()        pop(GCStack)

#define integerp(x)        ((x) != NULL && (x)->type == NUMBER)
#define symbolp(x)         ((x) != NULL && (x)->type == SYMBOL)
#define stringp(x)         ((x) != NULL && (x)->type == STRING)
#define characterp(x)      ((x) != NULL && (x)->type == CHARACTER)
#define arrayp(x)          ((x) != NULL && (x)->type == ARRAY)
#define streamp(x)         ((x) != NULL && (x)->type == STREAM)

#define mark(x)            (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x)          (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x)          ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT            1

#define setflag(x)         (Flags = Flags | 1<<(x))
#define clrflag(x)         (Flags = Flags & ~(1<<(x)))
#define tstflag(x)         (Flags & 1<<(x))

#define issp(x)            (x == ' ' || x == '\n' || x == '\r' || x == '\t')
#define isbr(x)            (x == ')' || x == '(' || x == '"' || x == '#' || x == '\'')
#define fntype(x)          (getminmax((uint16_t)(x))>>6)
#define longsymbolp(x)     (((x)->name & 0x03) == 0)
#define longnamep(x)       (((x) & 0x03) == 0)
#define twist(x)           ((uint16_t)((x)<<2) | (((x) & 0xC000)>>14))
#define untwist(x)         (((x)>>2 & 0x3FFF) | ((x) & 0x03)<<14)
#define arraysize(x)       (sizeof(x) / sizeof(x[0]))
#define stringifyX(x)      #x
#define stringify(x)       stringifyX(x)
#define PACKEDS            17600
#define BUILTINS           64000
#define ENDFUNCTIONS       1536

// Code marker stores start and end of code block (max 256 bytes)
#define startblock(x)      ((x->integer) & 0xFF)
#define endblock(x)        ((x->integer) >> 8 & 0xFF)

#define SDCARD_SS_PIN 10"#

#+(or arm riscv esp)
#"
// C Macros

#define nil                NULL
#define car(x)             (((object *) (x))->car)
#define cdr(x)             (((object *) (x))->cdr)

#define first(x)           car(x)
#define rest(x)            cdr(x)
#define second(x)          first(rest(x))
#define cddr(x)            cdr(cdr(x))
#define third(x)           first(cddr(x))

#define push(x, y)         ((y) = cons((x),(y)))
#define pop(y)             ((y) = cdr(y))

#define protect(y)         push((y), GCStack)
#define unprotect()        pop(GCStack)

#define integerp(x)        ((x) != NULL && (x)->type == NUMBER)
#define floatp(x)          ((x) != NULL && (x)->type == FLOAT)
#define symbolp(x)         ((x) != NULL && (x)->type == SYMBOL)
#define stringp(x)         ((x) != NULL && (x)->type == STRING)
#define characterp(x)      ((x) != NULL && (x)->type == CHARACTER)
#define arrayp(x)          ((x) != NULL && (x)->type == ARRAY)
#define streamp(x)         ((x) != NULL && (x)->type == STREAM)

#define mark(x)            (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x)          (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x)          ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT            1

#define setflag(x)         (Flags = Flags | 1<<(x))
#define clrflag(x)         (Flags = Flags & ~(1<<(x)))
#define tstflag(x)         (Flags & 1<<(x))

#define issp(x)            (x == ' ' || x == '\n' || x == '\r' || x == '\t')
#define isbr(x)            (x == ')' || x == '(' || x == '"' || x == '#' || x == '\'')
#define fntype(x)          (getminmax((uint16_t)(x))>>6)
#define longsymbolp(x)     (((x)->name & 0x03) == 0)
#define longnamep(x)       (((x) & 0x03) == 0)
#define twist(x)           ((uint32_t)((x)<<2) | (((x) & 0xC0000000)>>30))
#define untwist(x)         (((x)>>2 & 0x3FFFFFFF) | ((x) & 0x03)<<30)
#define arraysize(x)       (sizeof(x) / sizeof(x[0]))
#define stringifyX(x)      #x
#define stringify(x)       stringifyX(x)
#define PACKEDS            0x43238000
#define BUILTINS           0xF4240000
#define ENDFUNCTIONS       0x0BDC0000"#

#+(or arm riscv)
#"
// Code marker stores start and end of code block
#define startblock(x)      ((x->integer) & 0xFFFF)
#define endblock(x)        ((x->integer) >> 16 & 0xFFFF)"#))

(defparameter *constants* 
'(
#+avr-nano
#"
// Constants

const int TRACEMAX = 3; // Number of traced functions
enum type { ZZERO=0, SYMBOL=2, CODE=4, NUMBER=6, STREAM=8, CHARACTER=10, STRING=12, PAIR=14 };  // STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, STRINGSTREAM };
enum fntypes_t { OTHER_FORMS, TAIL_FORMS, FUNCTIONS, SPECIAL_FORMS };

// Stream names used by printobject
const char serialstream[] PROGMEM = "serial";
const char i2cstream[] PROGMEM = "i2c";
const char spistream[] PROGMEM = "spi";
const char sdstream[] PROGMEM = "sd";
const char stringstream[] PROGMEM = "string";
PGM_P const streamname[] PROGMEM = {serialstream, i2cstream, spistream, sdstream, stringstream};"#

#+avr
#"
// Constants

#define TRACEMAX 3  // Maximum number of traced functions
enum type { ZZERO=0, SYMBOL=2, CODE=4, NUMBER=6, STREAM=8, CHARACTER=10, ARRAY=12, STRING=14, PAIR=16 };  // ARRAY STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, STRINGSTREAM };
enum fntypes_t { OTHER_FORMS, TAIL_FORMS, FUNCTIONS, SPECIAL_FORMS };

// Stream names used by printobject
const char serialstream[] PROGMEM = "serial";
const char i2cstream[] PROGMEM = "i2c";
const char spistream[] PROGMEM = "spi";
const char sdstream[] PROGMEM = "sd";
const char stringstream[] PROGMEM = "string";
PGM_P const streamname[] PROGMEM = {serialstream, i2cstream, spistream, sdstream, stringstream};"#

#+arm
#"
// Constants

#define TRACEMAX 3  // Maximum number of traced functions
enum type { ZZERO=0, SYMBOL=2, CODE=4, NUMBER=6, STREAM=8, CHARACTER=10, FLOAT=12, ARRAY=14, STRING=16, PAIR=18 };  // ARRAY STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, WIFISTREAM, STRINGSTREAM, GFXSTREAM };
enum fntypes_t { OTHER_FORMS, TAIL_FORMS, FUNCTIONS, SPECIAL_FORMS };

// Stream names used by printobject
const char serialstream[] PROGMEM = "serial";
const char i2cstream[] PROGMEM = "i2c";
const char spistream[] PROGMEM = "spi";
const char sdstream[] PROGMEM = "sd";
const char wifistream[] PROGMEM = "wifi";
const char stringstream[] PROGMEM = "string";
const char gfxstream[] PROGMEM = "gfx";
const char *const streamname[] PROGMEM = {serialstream, i2cstream, spistream, sdstream, wifistream, stringstream, gfxstream};"#

#+esp
#"
// Constants

#define TRACEMAX 3  // Maximum number of traced functions
enum type { ZZERO=0, SYMBOL=2, CODE=4, NUMBER=6, STREAM=8, CHARACTER=10, FLOAT=12, ARRAY=14, STRING=16, PAIR=18 };  // ARRAY STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, WIFISTREAM, STRINGSTREAM, GFXSTREAM };
enum fntypes_t { OTHER_FORMS, TAIL_FORMS, FUNCTIONS, SPECIAL_FORMS };

// Stream names used by printobject
const char serialstream[] PROGMEM = "serial";
const char i2cstream[] PROGMEM = "i2c";
const char spistream[] PROGMEM = "spi";
const char sdstream[] PROGMEM = "sd";
const char wifistream[] PROGMEM = "wifi";
const char stringstream[] PROGMEM = "string";
const char gfxstream[] PROGMEM = "gfx";
PGM_P const streamname[] PROGMEM = {serialstream, i2cstream, spistream, sdstream, wifistream, stringstream, gfxstream};"#

#+riscv
#"
// Constants

#define TRACEMAX 3  // Maximum number of traced functions
enum type { ZZERO=0, SYMBOL=2, CODE=4, NUMBER=6, STREAM=8, CHARACTER=10, FLOAT=12, ARRAY=14, STRING=16, PAIR=18 };  // STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, STRINGSTREAM, GFXSTREAM };
enum fntypes_t { OTHER_FORMS, TAIL_FORMS, FUNCTIONS, SPECIAL_FORMS };

// Stream names used by printobject
const char serialstream[] PROGMEM = "serial";
const char i2cstream[] PROGMEM = "i2c";
const char spistream[] PROGMEM = "spi";
const char sdstream[] PROGMEM = "sd";
const char stringstream[] PROGMEM = "string";
const char gfxstream[] PROGMEM = "gfx";
const char *const streamname[] PROGMEM = {serialstream, i2cstream, spistream, sdstream, stringstream, gfxstream};"#))

#+avr-nano
(defparameter *typedefs* #"
// Typedefs

typedef uint16_t symbol_t;
typedef uint16_t builtin_t;
typedef uint16_t chars_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        chars_t chars; // For strings
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);

typedef const struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);"#)

#+avr
(defparameter *typedefs* #"
// Typedefs

typedef uint16_t symbol_t;
typedef uint16_t builtin_t;
typedef uint16_t chars_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        chars_t chars; // For strings
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);

typedef const struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
  const char *doc;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);"#)

#+msp430
(defparameter *typedefs* #"
// Typedefs

typedef unsigned int symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        int chars; // For strings
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);

typedef const struct {
  PGM_P string;
  fn_ptr_type fptr;
  uint8_t funtype: 2;
  uint8_t minargs: 3;
  uint8_t maxargs: 3;
  const char *doc;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);

typedef uint16_t builtin_t;"#)

#+arm
(defparameter *typedefs* #"
// Typedefs

typedef uint32_t symbol_t;
typedef uint32_t builtin_t;
typedef uint32_t chars_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        chars_t chars; // For strings
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);

typedef const struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
  const char *doc;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);"#)

#+riscv
(defparameter *typedefs* #"
// Typedefs

typedef uint32_t symbol_t;
typedef uint32_t builtin_t;
typedef uint32_t chars_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      uintptr_t type;
      union {
        symbol_t name;
        int integer;
        chars_t chars; // For strings
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);

typedef const struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
  const char *doc;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);
typedef int PinMode;"#)

#+esp
(defparameter *typedefs* #"
// Typedefs

typedef uint32_t symbol_t;
typedef uint32_t builtin_t;
typedef uint32_t chars_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        chars_t chars; // For strings
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);

typedef const struct {
  PGM_P string;
  fn_ptr_type fptr;
  uint8_t minmax;
  const char *doc;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);"#)

(defparameter *global-variables* 
  '(

#+avr-nano
#"
// Global variables

uint8_t FLAG __attribute__ ((section (".noinit")));

object Workspace[WORKSPACESIZE] OBJECTALIGNED;"#

#+avr
#"
// Global variables

uint8_t FLAG __attribute__ ((section (".noinit")));

object Workspace[WORKSPACESIZE] OBJECTALIGNED;
#if defined(CODESIZE)
uint8_t MyCode[CODESIZE] WORDALIGNED; // Must be even
#endif"#


#+arm
#"
// Global variables

object Workspace[WORKSPACESIZE] WORDALIGNED MEMBANK;
#if defined(CODESIZE)
RAMFUNC uint8_t MyCode[CODESIZE] WORDALIGNED;
#endif"#

#+esp
#"
// Global variables

#if defined(BOARD_HAS_PSRAM)
object *Workspace WORDALIGNED;
#else
object Workspace[WORKSPACESIZE] WORDALIGNED;
#endif"#

#+riscv
#"
// Global variables

#if (WORKSPACESIZE > 80000)
object *Workspace WORDALIGNED;
#else
object Workspace[WORKSPACESIZE] WORDALIGNED;
#endif
uint8_t MyCode[CODESIZE] WORDALIGNED;"#

#+avr-nano
#"
jmp_buf exception;
unsigned int Freespace = 0;
object *Freelist;
unsigned int I2Ccount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];
builtin_t Context;

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
object *GlobalStringTail;
int GlobalStringIndex = 0;
uint8_t PrintCount = 0;
uint8_t BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;
uint16_t RandomSeed;"#

#+avr
#"
jmp_buf toplevel_handler;
jmp_buf *handler = &toplevel_handler;
unsigned int Freespace = 0;
object *Freelist;
unsigned int I2Ccount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];
builtin_t Context;
#define BACKTRACESIZE 8
uint8_t TraceStart = 0, TraceTop = 0;
symbol_t Backtrace[BACKTRACESIZE];

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
object *GlobalStringTail;
int GlobalStringIndex = 0;
uint8_t PrintCount = 0;
uint8_t BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;
uint16_t RandomSeed;"#

#+(or arm riscv)
#"
jmp_buf toplevel_handler;
jmp_buf *handler = &toplevel_handler;
unsigned int Freespace = 0;
object *Freelist;
unsigned int I2Ccount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];
builtin_t Context;
#define BACKTRACESIZE 8
uint8_t TraceStart = 0, TraceTop = 0;
symbol_t Backtrace[BACKTRACESIZE];

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
object *GlobalStringTail;
int GlobalStringIndex = 0;
uint8_t PrintCount = 0;
uint8_t BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;"#

#+esp
#"
jmp_buf toplevel_handler;
jmp_buf *handler = &toplevel_handler;
unsigned int Freespace = 0;
object *Freelist;
unsigned int I2Ccount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];
builtin_t Context;
#define BACKTRACESIZE 8
uint8_t TraceStart = 0, TraceTop = 0;
symbol_t Backtrace[BACKTRACESIZE];

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
object *GlobalStringTail;
int GlobalStringIndex = 0;
uint8_t PrintCount = 0;
uint8_t BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;
void* StackBottom;"#


#-errors
#"
// Flags
enum flag { PRINTREADABLY, RETURNFLAG, ESCAPE, EXITEDITOR, LIBRARYLOADED, NOESC, NOECHO, BACKTRACE };
typedef uint8_t flags_t;
volatile flags_t Flags = 1<<PRINTREADABLY; // Set by default"#

#+errors
#"
// Flags
enum flag { PRINTREADABLY, RETURNFLAG, ESCAPE, EXITEDITOR, LIBRARYLOADED, NOESC, NOECHO, MUFFLEERRORS, BACKTRACE };
typedef uint16_t flags_t;
volatile flags_t Flags = 1<<PRINTREADABLY; // Set by default"#

#"
// Forward references
object *tee;
void pfstring (PGM_P s, pfun_t pfun);"#))

