;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; Function definitions

(defparameter *definitions*

  '((nil ;; Symbols
     ((NIL "nil" 0 0 #"
/*
  nil
  A symbol equivalent to the empty list (). Also represents false.
*/"#)

      (TEE "t" 0 0 #"
/*
  t
  A symbol representing true.
*/"#)

      (NOTHING nil 0 0 #"
/*
  nothing
  A symbol with no value.
  It is useful if you want to suppress printing the result of evaluating a function.
*/"#)

      (OPTIONAL "&optional" 0 0 #"
/*
  &optional
  Can be followed by one or more optional parameters in a lambda or defun parameter list.
*/"#)

      #-avr-nano
      (FEATURES "*features*" 0 0 #"
/*
  *features*
  Returns a list of keywords representing features supported by this platform.
*/"#)

      #+arrays
      (INITIALELEMENT ":initial-element" 0 0 nil)

      #+arrays
      (ELEMENTTYPE ":element-type" 0 0 nil)

      #-avr-nano
      (TEST ":test" 0 0 nil)

      #-avr-nano
      (COLONA ":a" 0 0 nil)

      #-avr-nano
      (COLONB ":b" 0 0 nil)

      #-avr-nano
      (COLONC ":c" 0 0 nil)

      #+arrays
      (BIT nil 0 0 nil)

      (AMPREST "&rest" 0 0 #"
/*
  &rest
  Can be followed by a parameter in a lambda or defun parameter list,
  and is assigned a list of the corresponding arguments.
*/"#)

      (LAMBDA nil 1 127 #"
/*
  (lambda (parameter*) form*)
  Creates an unnamed function with parameters. The body is evaluated with the parameters as local variables
  whose initial values are defined by the values of the forms after the lambda form.
*/"#)

      (LET "let" 1 127 #"
/*
  (let ((var value) ... ) forms*)
  Declares local variables with values, and evaluates the forms with those local variables.
*/"#)

      (LETSTAR "let*" 1 127 #"
/*
  (let* ((var value) ... ) forms*)
  Declares local variables with values, and evaluates the forms with those local variables.
  Each declaration can refer to local variables that have been defined earlier in the let*.
*/"#)

      (CLOSURE nil 1 127 nil)

      #+avr
      (PSTAR "*p*" 0 127 nil)

      #-(or avr avr-nano)
      (PSTAR "*pc*" 0 127 nil))

     "sy")

    ("Special forms"
     ((QUOTE nil 1 1 "
object *sp_quote (object *args, object *env) {
  (void) env;
  return first(args);
}")

      (OR nil 0 127 "
/*
  (or item*)
  Evaluates its arguments until one returns non-nil, and returns its value.
*/
object *sp_or (object *args, object *env) {
  while (args != NULL) {
    object *val = eval(car(args), env);
    if (val != NULL) return val;
    args = cdr(args);
  }
  return nil;
}")

      #+ignore
      (LAMBDA nil 0 127 "
object *sp_lambda (object *args, object *env) {
  return cons(symbol(CLOSURE), (cons(env,args)));
}")

      (DEFUN nil 2 127 #"
/*
  (defun name (parameters) form*)
  Defines a function.
*/
object *sp_defun (object *args, object *env) {
  (void) env;
  object *var = first(args);
  if (!symbolp(var)) error(notasymbol, var);
  object *val = cons(bsymbol(LAMBDA), cdr(args));
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  return var;
}"#)

      (DEFVAR nil 1 3 #"
/*
  (defvar variable form)
  Defines a global variable.
*/
object *sp_defvar (object *args, object *env) {
  object *var = first(args);
  if (!symbolp(var)) error(notasymbol, var);
  object *val = NULL;
  args = cdr(args);
  if (args != NULL) { setflag(NOESC); val = eval(first(args), env); clrflag(NOESC); }
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  return var;
}"#)

     (SETQ nil 2 126 #"
/*
  (setq symbol value [symbol value]*)
  For each pair of arguments assigns the value of the second argument
  to the variable specified in the first argument.
*/
object *sp_setq (object *args, object *env) {
  object *arg = nil; builtin_t setq = Context;
  while (args != NULL) {
    if (cdr(args) == NULL) { Context = setq; error2(oddargs); }
    object *pair = findvalue(first(args), env);
    arg = eval(second(args), env);
    cdr(pair) = arg;
    args = cddr(args);
  }
  return arg;
}"#)

     #-esp
     (LOOP nil 0 127 "
/*
  (loop forms*)
  Executes its arguments repeatedly until one of the arguments calls (return),
  which then causes an exit from the loop.
*/
object *sp_loop (object *args, object *env) {
  object *start = args;
  for (;;) {
    args = start;
    while (args != NULL) {
      object *result = eval(car(args),env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      args = cdr(args);
    }
    testescape();
  }
}")

     #+esp
     (LOOP nil 0 127 "
/*
  (loop forms*)
  Executes its arguments repeatedly until one of the arguments calls (return),
  which then causes an exit from the loop.
*/
object *sp_loop (object *args, object *env) {
  object *start = args;
  for (;;) {
    yield();
    args = start;
    while (args != NULL) {
      object *result = eval(car(args),env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      args = cdr(args);
    }
    testescape();
  }
}")

     #+avr-nano
     (PUSH nil 2 2 "
/*
  (push item place)
  Modifies the value of place, which should be a list, to add item onto the front of the list,
  and returns the new list.
*/
object *sp_push (object *args, object *env) {
  object *item = eval(first(args), env);
  object **loc = place(second(args), env);
  push(item, *loc);
  return *loc;
}")

     #-avr-nano
     (PUSH nil 2 2 "
/*
  (push item place)
  Modifies the value of place, which should be a list, to add item onto the front of the list,
  and returns the new list.
*/
object *sp_push (object *args, object *env) {
  int bit;
  object *item = eval(first(args), env);
  object **loc = place(second(args), env, &bit);
  if (bit != -1) error2(invalidarg);
  push(item, *loc);
  return *loc;
}")

     #+avr-nano
     (POP nil 1 1 "
/*
  (pop place)
  Modifies the value of place, which should be a non-nil list, to remove its first item,
  and returns that item.
*/
object *sp_pop (object *args, object *env) {
  object *arg = first(args);
  if (arg == NULL) error2(invalidarg);
  object **loc = place(arg, env);
  if (!consp(*loc)) error(notalist, *loc);
  object *result = car(*loc);
  pop(*loc);
  return result;
}")

     #-avr-nano
     (POP nil 1 1 "
/*
  (pop place)
  Modifies the value of place, which should be a non-nil list, to remove its first item,
  and returns that item.
*/
object *sp_pop (object *args, object *env) {
  int bit;
  object *arg = first(args);
  if (arg == NULL) error2(invalidarg);
  object **loc = place(arg, env, &bit);
  if (bit < -1) error(invalidarg, arg);
  if (!consp(*loc)) error(notalist, *loc);
  object *result = car(*loc);
  pop(*loc);
  return result;
}")) "sp")


     ("Accessors"
      (
       #-float
       (INCF nil 1 2 #"
/*
  (incf place [number])
  Increments a place, which should have an numeric value, and returns the result.
  The third argument is an optional increment which defaults to 1.
*/
object *sp_incf (object *args, object *env) {
  return incfdecf(args, 1, env);
}"#)

       #+float
       (INCF nil 1 2 #"
/*
  (incf place [number])
  Increments a place, which should have an numeric value, and returns the result.
  The third argument is an optional increment which defaults to 1.
*/
object *sp_incf (object *args, object *env) {
  int bit;
  object **loc = place(first(args), env, &bit);
  if (bit < -1) error2(notanumber);
  args = cdr(args);

  object *x = *loc;
  object *inc = (args != NULL) ? eval(first(args), env) : NULL;

  if (bit != -1) {
    int increment;
    if (inc == NULL) increment = 1; else increment = checkbitvalue(inc);
    int newvalue = (((*loc)->integer)>>bit & 1) + increment;

    if (newvalue & ~1) error2(PSTR("result is not a bit value"));
    *loc = number((((*loc)->integer) & ~(1<<bit)) | newvalue<<bit);
    return number(newvalue);
  }

  if (floatp(x) || floatp(inc)) {
    float increment;
    float value = checkintfloat(x);

    if (inc == NULL) increment = 1.0; else increment = checkintfloat(inc);

    *loc = makefloat(value + increment);
  } else if (integerp(x) && (integerp(inc) || inc == NULL)) {
    int increment;
    int value = x->integer;

    if (inc == NULL) increment = 1; else increment = inc->integer;

    if (increment < 1) {
      if (INT_MIN - increment > value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    } else {
      if (INT_MAX - increment < value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    }
  } else error2(notanumber);
  return *loc;
}"#)

    #-float
    (DECF nil 1 2 #"
/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
*/
object *sp_decf (object *args, object *env) {
  return incfdecf(args, -1, env);
}"#)

    #+float
    (DECF nil 1 2 #"
/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
*/
object *sp_decf (object *args, object *env) {
  int bit;
  object **loc = place(first(args), env, &bit);
  if (bit < -1) error2(notanumber);
  args = cdr(args);

  object *x = *loc;
  object *dec = (args != NULL) ? eval(first(args), env) : NULL;

  if (bit != -1) {
    int decrement;
    if (dec == NULL) decrement = 1; else decrement = checkbitvalue(dec);
    int newvalue = (((*loc)->integer)>>bit & 1) - decrement;

    if (newvalue & ~1) error2(PSTR("result is not a bit value"));
    *loc = number((((*loc)->integer) & ~(1<<bit)) | newvalue<<bit);
    return number(newvalue);
  }

  if (floatp(x) || floatp(dec)) {
    float decrement;
    float value = checkintfloat(x);

    if (dec == NULL) decrement = 1.0; else decrement = checkintfloat(dec);

    *loc = makefloat(value - decrement);
  } else if (integerp(x) && (integerp(dec) || dec == NULL)) {
    int decrement;
    int value = x->integer;

    if (dec == NULL) decrement = 1; else decrement = dec->integer;

    if (decrement < 1) {
      if (INT_MAX + decrement < value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    } else {
      if (INT_MIN + decrement > value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    }
  } else error2(notanumber);
  return *loc;
}"#)

     #+avr-nano
     (SETF nil 2 126 #"
/*
  (setf place value [place value]*)
  For each pair of arguments modifies a place to the result of evaluating value.
*/
object *sp_setf (object *args, object *env) {
  builtin_t setf = Context;
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) { Context = setf; error2(oddargs); }
    object **loc = place(first(args), env);
    arg = eval(second(args), env);
    *loc = arg;
    args = cddr(args);
  }
  return arg;
}"#)

     #-avr-nano
     (SETF nil 2 126 #"
/*
  (setf place value [place value]*)
  For each pair of arguments modifies a place to the result of evaluating value.
*/
object *sp_setf (object *args, object *env) {
  int bit; builtin_t setf = Context;
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) { Context = setf; error2(oddargs); }
    object **loc = place(first(args), env, &bit);
    arg = eval(second(args), env);
    if (bit == -1) *loc = arg;
    else if (bit < -1) (*loc)->chars = ((*loc)->chars & ~(0xff<<((-bit-2)<<3))) | checkchar(arg)<<((-bit-2)<<3);
    else *loc = number((checkinteger(*loc) & ~(1<<bit)) | checkbitvalue(arg)<<bit);
    args = cddr(args);
  }
  return arg;
}"#)) "sp")

     ("Other special forms"
      (
       (DOLIST nil 1 127 #"
/*
  (dolist (var list [result]) form*)
  Sets the local variable var to each element of list in turn, and executes the forms.
  It then returns result, or nil if result is omitted.
*/
object *sp_dolist (object *args, object *env) {
  object *params = checkarguments(args, 2, 3);
  object *var = first(params);
  object *list = eval(second(params), env);
  protect(list); // Don't GC the list
  object *pair = cons(var,nil);
  push(pair,env);
  params = cddr(params);
  args = cdr(args);
  while (list != NULL) {
    if (improperp(list)) error(notproper, list);
    cdr(pair) = first(list);
    object *forms = args;
    while (forms != NULL) {
      object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        unprotect();
        return result;
      }
      forms = cdr(forms);
    }
    list = cdr(list);
  }
  cdr(pair) = nil;
  unprotect();
  if (params == NULL) return nil;
  return eval(car(params), env);
}"#)

      (DOTIMES nil 1 127 #"
/*
  (dotimes (var number [result]) form*)
  Executes the forms number times, with the local variable var set to each integer from 0 to number-1 in turn.
  It then returns result, or nil if result is omitted.
*/
object *sp_dotimes (object *args, object *env) {
  object *params = checkarguments(args, 2, 3);
  object *var = first(params);
  int count = checkinteger(eval(second(params), env));
  int index = 0;
  params = cddr(params);
  object *pair = cons(var,number(0));
  push(pair,env);
  args = cdr(args);
  while (index < count) {
    cdr(pair) = number(index);
    object *forms = args;
    while (forms != NULL) {
      object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      forms = cdr(forms);
    }
    index++;
  }
  cdr(pair) = number(index);
  if (params == NULL) return nil;
  return eval(car(params), env);
}"#)

      #-avr-nano
      (DO nil 2 127 #"
/*
  (do ((var [init [step]])*) (end-test result*) form*)
  Accepts an arbitrary number of iteration vars, which are initialised to init and stepped by step sequentially.
  The forms are executed until end-test is true. It returns result.
*/
object *sp_do (object *args, object *env) {
  return dobody(args, env, false);
}"#)

      #-avr-nano
      (DOSTAR "do*" 1 127 #"
/*
  (do* ((var [init [step]])*) (end-test result*) form*)
  Accepts an arbitrary number of iteration vars, which are initialised to init and stepped by step in parallel.
  The forms are executed until end-test is true. It returns result.
*/
object *sp_dostar (object *args, object *env) {
  return dobody(args, env, true);
}"#)

      (TRACE nil 0 1 #"
/*
  (trace [function]*)
  Turns on tracing of up to TRACEMAX user-defined functions,
  and returns a list of the functions currently being traced.
*/
object *sp_trace (object *args, object *env) {
  (void) env;
  while (args != NULL) {
    object *var = first(args);
    if (!symbolp(var)) error(notasymbol, var);
    trace(var->name);
    args = cdr(args);
  }
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
    i++;
  }
  return args;
}"#)

      (UNTRACE nil 0 1 #"
/*
  (untrace [function]*)
  Turns off tracing of up to TRACEMAX user-defined functions, and returns a list of the functions untraced.
  If no functions are specified it untraces all functions.
*/
object *sp_untrace (object *args, object *env) {
  (void) env;
  if (args == NULL) {
    int i = 0;
    while (i < TRACEMAX) {
      if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
      TraceFn[i] = 0;
      i++;
    }
  } else {
    while (args != NULL) {
      object *var = first(args);
      if (!symbolp(var)) error(notasymbol, var);
      untrace(var->name);
      args = cdr(args);
    }
  }
  return args;
}"#)

      (FORMILLIS "for-millis" 1 127 "
/*
  (for-millis ([number]) form*)
  Executes the forms and then waits until a total of number milliseconds have elapsed.
  Returns the total number of milliseconds taken.
*/
object *sp_formillis (object *args, object *env) {
  object *param = checkarguments(args, 0, 1);
  unsigned long start = millis();
  unsigned long now, total = 0;
  if (param != NULL) total = checkinteger(eval(first(param), env));
  eval(tf_progn(cdr(args),env), env);
  do {
    now = millis() - start;
    testescape();
  } while (now < total);
  if (now <= INT_MAX) return number(now);
  return nil;
}")

      (TIME nil 1 1 #"
/*
  (time form)
  Prints the value returned by the form, and the time taken to evaluate the form
  in milliseconds or seconds.
*/
object *sp_time (object *args, object *env) {
  unsigned long start = millis();
  object *result = eval(first(args), env);
  unsigned long elapsed = millis() - start;
  printobject(result, pserial);
  pfstring(PSTR("\nTime: "), pserial);
  if (elapsed < 1000) {
    pint(elapsed, pserial);
    pfstring(PSTR(" ms\n"), pserial);
  } else {
    elapsed = elapsed+50;
    pint(elapsed/1000, pserial);
    pserial('.'); pint((elapsed/100)%10, pserial);
    pfstring(PSTR(" s\n"), pserial);
  }
  return bsymbol(NOTHING);
}"#)

      (WITHOUTPUTTOSTRING "with-output-to-string" 1 127 "
/*
  (with-output-to-string (str) form*)
  Returns a string containing the output to the stream variable str.
*/
object *sp_withoutputtostring (object *args, object *env) {
  object *params = checkarguments(args, 1, 1);
  object *var = first(params);
  object *pair = cons(var, stream(STRINGSTREAM, 0));
  push(pair,env);
  object *string = startstring();
  protect(string);
  object *forms = cdr(args);
  eval(tf_progn(forms,env), env);
  unprotect();
  return string;
}")

      (WITHSERIAL "with-serial" 1 127 "
/*
  (with-serial (str port [baud]) form*)
  Evaluates the forms with str bound to a serial-stream using port.
  The optional baud gives the baud rate divided by 100, default 96.
*/
object *sp_withserial (object *args, object *env) {
  object *params = checkarguments(args, 2, 3);
  object *var = first(params);
  int address = checkinteger(eval(second(params), env));
  params = cddr(params);
  int baud = 96;
  if (params != NULL) baud = checkinteger(eval(first(params), env));
  object *pair = cons(var, stream(SERIALSTREAM, address));
  push(pair,env);
  serialbegin(address, baud);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  serialend(address);
  return result;
}")

      #-(or arm esp)
     (WITHI2C "with-i2c" 1 127 "
/*
  (with-i2c (str [port] address [read-p]) form*)
  Evaluates the forms with str bound to an i2c-stream defined by address.
  If read-p is nil or omitted the stream is written to, otherwise it specifies the number of bytes
  to be read from the stream. The port if specified is ignored.
*/
object *sp_withi2c (object *args, object *env) {
  object *params = checkarguments(args, 2, 4);
  object *var = first(params);
  int address = checkinteger(eval(second(params), env));
  params = cddr(params);
  if (address == 0 && params != NULL) params = cdr(params); // Ignore port
  int read = 0; // Write
  I2Ccount = 0;
  if (params != NULL) {
    object *rw = eval(first(params), env);
    if (integerp(rw)) I2Ccount = rw->integer;
    read = (rw != NULL);
  }
  I2Cinit(1); // Pullups
  object *pair = cons(var, (I2Cstart(address, read)) ? stream(I2CSTREAM, address) : nil);
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  I2Cstop(read);
  return result;
}")

     #+(or arm esp)
     (WITHI2C "with-i2c" 1 127 "
/*
  (with-i2c (str [port] address [read-p]) form*)
  Evaluates the forms with str bound to an i2c-stream defined by address.
  If read-p is nil or omitted the stream is written to, otherwise it specifies the number of bytes
  to be read from the stream. If port is omitted it defaults to 0, otherwise it specifies the port, 0 or 1.
*/
object *sp_withi2c (object *args, object *env) {
  object *params = checkarguments(args, 2, 4);
  object *var = first(params);
  int address = checkinteger(eval(second(params), env));
  params = cddr(params);
  if ((address == 0 || address == 1) && params != NULL) {
    address = address * 128 + checkinteger(eval(first(params), env));
    params = cdr(params);
  }
  int read = 0; // Write
  I2Ccount = 0;
  if (params != NULL) {
    object *rw = eval(first(params), env);
    if (integerp(rw)) I2Ccount = rw->integer;
    read = (rw != NULL);
  }
  // Top bit of address is I2C port
  TwoWire *port = &Wire;
  #if defined(ULISP_I2C1)
  if (address > 127) port = &Wire1;
  #endif
  I2Cinit(port, 1); // Pullups
  object *pair = cons(var, (I2Cstart(port, address & 0x7F, read)) ? stream(I2CSTREAM, address) : nil);
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  I2Cstop(port, read);
  return result;
}")

      #+(or avr avr-nano esp)
      (WITHSPI "with-spi" 1 127 #"
/*
  (with-spi (str pin [clock] [bitorder] [mode]) form*)
  Evaluates the forms with str bound to an spi-stream.
  The parameters specify the enable pin, clock in kHz (default 4000),
  bitorder 0 for LSBFIRST and 1 for MSBFIRST (default 1), and SPI mode (default 0).
*/
object *sp_withspi (object *args, object *env) {
  object *params = checkarguments(args, 2, 6);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(nostream);
  int pin = checkinteger(eval(car(params), env));
  pinMode(pin, OUTPUT);
  digitalWrite(pin, HIGH);
  params = cdr(params);
  int clock = 4000, mode = SPI_MODE0; // Defaults
  int bitorder = MSBFIRST;
  if (params != NULL) {
    clock = checkinteger(eval(car(params), env));
    params = cdr(params);
    if (params != NULL) {
      bitorder = (checkinteger(eval(car(params), env)) == 0) ? LSBFIRST : MSBFIRST;
      params = cdr(params);
      if (params != NULL) {
        int modeval = checkinteger(eval(car(params), env));
        mode = (modeval == 3) ? SPI_MODE3 : (modeval == 2) ? SPI_MODE2 : (modeval == 1) ? SPI_MODE1 : SPI_MODE0;
      }
    }
  }
  object *pair = cons(var, stream(SPISTREAM, pin));
  push(pair,env);
  SPI.begin();
  SPI.beginTransaction(SPISettings(((unsigned long)clock * 1000), bitorder, mode));
  digitalWrite(pin, LOW);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  digitalWrite(pin, HIGH);
  SPI.endTransaction();
  return result;
}"#)

      #+arm
      (WITHSPI "with-spi" 1 127 #"
/*
  (with-spi (str pin [clock] [bitorder] [mode] [port]) form*)
  Evaluates the forms with str bound to an spi-stream.
  The parameters specify the enable pin, clock in kHz (default 4000),
  bitorder 0 for LSBFIRST and 1 for MSBFIRST (default 1), SPI mode (default 0), and port 0 or 1 (default 0).
*/
object *sp_withspi (object *args, object *env) {
  object *params = checkarguments(args, 2, 6);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(nostream);
  int pin = checkinteger(eval(car(params), env));
  pinMode(pin, OUTPUT);
  digitalWrite(pin, HIGH);
  params = cdr(params);
  int clock = 4000, mode = SPI_MODE0, address = 0; // Defaults
  BitOrder bitorder = MSBFIRST;
  if (params != NULL) {
    clock = checkinteger(eval(car(params), env));
    params = cdr(params);
    if (params != NULL) {
      bitorder = (checkinteger(eval(car(params), env)) == 0) ? LSBFIRST : MSBFIRST;
      params = cdr(params);
      if (params != NULL) {
        int modeval = checkinteger(eval(car(params), env));
        mode = (modeval == 3) ? SPI_MODE3 : (modeval == 2) ? SPI_MODE2 : (modeval == 1) ? SPI_MODE1 : SPI_MODE0;
        params = cdr(params);
        if (params != NULL) {
          address = checkinteger(eval(car(params), env));
        }
      }
    }
  }
  object *pair = cons(var, stream(SPISTREAM, pin + 128*address));
  push(pair,env);
  SPIClass *spiClass = &SPI;
  #if defined(ULISP_SPI1)
  if (address == 1) spiClass = &SPI1;
  #endif
  spiClass->begin();
  spiClass->beginTransaction(SPISettings(((unsigned long)clock * 1000), bitorder, mode));
  digitalWrite(pin, LOW);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  digitalWrite(pin, HIGH);
  spiClass->endTransaction();
  return result;
}"#)

      #+riscv
      (WITHSPI "with-spi" 1 127 #"
/*
  (with-spi (str pin [clock] [bitorder] [mode] [port]) form*)
  Evaluates the forms with str bound to an spi-stream.
  The parameters specify the enable pin, clock in kHz (default 4000),
  bitorder 0 for LSBFIRST and 1 for MSBFIRST (default 1), SPI mode (default 0), and port 0 or 1 (default 0).
*/
object *sp_withspi (object *args, object *env) {
  object *params = checkarguments(args, 2, 6);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(nostream);
  int pin = checkinteger(eval(car(params), env));
  pinMode(pin, OUTPUT);
  digitalWrite(pin, HIGH);
  params = cdr(params);
  int clock = 4000, mode = SPI_MODE0, address = 0; // Defaults
  BitOrder bitorder = MSBFIRST;
  if (params != NULL) {
    clock = checkinteger(eval(car(params), env));
    params = cdr(params);
    if (params != NULL) {
      bitorder = (checkinteger(eval(car(params), env)) == 0) ? LSBFIRST : MSBFIRST;
      params = cdr(params);
      if (params != NULL) {
        int modeval = checkinteger(eval(car(params), env));
        mode = (modeval == 3) ? SPI_MODE3 : (modeval == 2) ? SPI_MODE2 : (modeval == 1) ? SPI_MODE1 : SPI_MODE0;
        params = cdr(params);
        if (params != NULL) {
          address = checkinteger(eval(car(params), env));
        }
      }
    }
  }
  object *pair = cons(var, stream(SPISTREAM, pin + 128*address));
  push(pair,env);
  SPIClass *spiClass = &SPI;
  (*spiClass).begin();
  (*spiClass).beginTransaction(SPISettings(((unsigned long)clock * 1000), bitorder, mode));
  digitalWrite(pin, LOW);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  digitalWrite(pin, HIGH);
  (*spiClass).endTransaction();
  return result;
}"#)

      #+arm
      (WITHSDCARD "with-sd-card" 2 127 #"
/*
  (with-sd-card (str filename [mode]) form*)
  Evaluates the forms with str bound to an sd-stream reading from or writing to the file filename.
  If mode is omitted the file is read, otherwise 0 means read, 1 write-append, or 2 write-overwrite.
*/
object *sp_withsdcard (object *args, object *env) {
  #if defined(sdcardsupport)
  object *params = checkarguments(args, 2, 3);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(PSTR("no filename specified"));
  builtin_t temp = Context;
  object *filename = eval(first(params), env);
  Context = temp;
  if (!stringp(filename)) error(PSTR("filename is not a string"), filename);
  params = cdr(params);
  SDBegin();
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(first(params));
  int oflag = O_READ;
  if (mode == 1) oflag = O_RDWR | O_CREAT | O_APPEND; else if (mode == 2) oflag = O_RDWR | O_CREAT | O_TRUNC;
  if (mode >= 1) {
    char buffer[BUFFERSIZE];
    SDpfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDpfile) error2(PSTR("problem writing to SD card or invalid filename"));
  } else {
    char buffer[BUFFERSIZE];
    SDgfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDgfile) error2(PSTR("problem reading from SD card or invalid filename"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  return result;
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

      #+esp
      (WITHSDCARD "with-sd-card" 2 127 #"
/*
  (with-sd-card (str filename [mode]) form*)
  Evaluates the forms with str bound to an sd-stream reading from or writing to the file filename.
  If mode is omitted the file is read, otherwise 0 means read, 1 write-append, or 2 write-overwrite.
*/
object *sp_withsdcard (object *args, object *env) {
  #if defined(sdcardsupport)
  object *params = checkarguments(args, 2, 3);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(PSTR("no filename specified"));
  builtin_t temp = Context;
  object *filename = eval(first(params), env);
  Context = temp;
  if (!stringp(filename)) error(PSTR("filename is not a string"), filename);
  params = cdr(params);
  SDBegin();
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(first(params));
  const char *oflag = FILE_READ;
  if (mode == 1) oflag = FILE_APPEND; else if (mode == 2) oflag = FILE_WRITE;
  if (mode >= 1) {
    char buffer[BUFFERSIZE];
    SDpfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDpfile) error2(PSTR("problem writing to SD card or invalid filename"));
  } else {
    char buffer[BUFFERSIZE];
    SDgfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDgfile) error2(PSTR("problem reading from SD card or invalid filename"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  return result;
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

      #+(or avr avr-nano)
      (WITHSDCARD "with-sd-card" 2 127 #"
/*
  (with-sd-card (str filename [mode]) form*)
  Evaluates the forms with str bound to an sd-stream reading from or writing to the file filename.
  If mode is omitted the file is read, otherwise 0 means read, 1 write-append, or 2 write-overwrite.
*/
object *sp_withsdcard (object *args, object *env) {
  #if defined(sdcardsupport)
  object *params = checkarguments(args, 2, 3);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(PSTR("no filename specified"));
  builtin_t temp = Context;
  object *filename = eval(first(params), env);
  Context = temp;
  if (!stringp(filename)) error(PSTR("filename is not a string"), filename);
  params = cdr(params);
  SD.begin(SDCARD_SS_PIN);
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(first(params));
  int oflag = O_READ;
  if (mode == 1) oflag = O_RDWR | O_CREAT | O_APPEND; else if (mode == 2) oflag = O_RDWR | O_CREAT | O_TRUNC;
  if (mode >= 1) {
    char buffer[BUFFERSIZE];
    SDpfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDpfile) error2(PSTR("problem writing to SD card or invalid filename"));
  } else {
    char buffer[BUFFERSIZE];
    SDgfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDgfile) error2(PSTR("problem reading from SD card or invalid filename"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  return result;
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

      #+riscv
      (WITHSDCARD "with-sd-card" 2 127 #"
/*
  (with-sd-card (str filename [mode]) form*)
  Evaluates the forms with str bound to an sd-stream reading from or writing to the file filename.
  If mode is omitted the file is read, otherwise 0 means read, 1 write-append, or 2 write-overwrite.
*/
object *sp_withsdcard (object *args, object *env) {
  #if defined(sdcardsupport)
  object *params = checkarguments(args, 2, 3);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(PSTR("no filename specified"));
  builtin_t temp = Context;
  object *filename = eval(first(params), env);
  Context = temp;
  if (!stringp(filename)) error(PSTR("filename is not a string"), filename);
  params = cdr(params);
  if (!SD.begin(SS)) error2("problem initialising SD card");
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(first(params));
  int oflag = O_READ;
  if (mode == 1) oflag = O_RDWR | O_CREAT | O_APPEND; else if (mode == 2) oflag = O_RDWR | O_CREAT | O_TRUNC;
  if (mode >= 1) {
    char buffer[BUFFERSIZE];
    SDpfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDpfile) error2(PSTR("problem writing to SD card or invalid filename"));
  } else {
    char buffer[BUFFERSIZE];
    SDgfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDgfile) error2(PSTR("problem reading from SD card or invalid filename"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  return result;
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

    #+msp430
    (WITHLCD "with-lcd" 1 127 #"
object *sp_withlcd (object *args, object *env) {
  #if defined(__MSP430FR6989__)
  myLCD.init();
  object *params = first(args);
  object *var = first(params);
  object *pair = cons(var, stream(LCDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  return result;
  #else
  (void) args, (void) env;
  error(PSTR("with-lcd not supported"));
  return nil;
#endif
}"#)) "sp")

    #+(or avr arm stm32 riscv)
    ("Assembler"
     
     (
      #+avr
      (DEFCODE nil 0 127 #"
/*
  (defcode name (parameters) form*)
  Creates a machine-code function called name from a series of 16-bit integers given in the body of the form.
  These are written into RAM, and can be executed by calling the function in the same way as a normal Lisp function.
*/
object *sp_defcode (object *args, object *env) {
#if defined(CODESIZE)
  setflag(NOESC);
  object *var = first(args);
  if (!symbolp(var)) error(PSTR("not a symbol"), var);

  // Make *p* a local variable for program counter
  object *pcpair = cons(bsymbol(PSTAR), number(0));
  push(pcpair,env);
  args = cdr(args);

  // Make labels into local variables
  object *entries = cdr(args);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      object *pair = cons(arg,number(0));
      push(pair,env);
    }
    entries = cdr(entries);
  }

  // First pass
  int origin = 0;
  int codesize = assemble(1, origin, cdr(args), env, pcpair);

  // See if it will fit
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = car(globals);
    if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
      object *codeid = second(pair);
      if (codeid->type == CODE) {
        codesize = codesize + endblock(codeid) - startblock(codeid);
      }
    }
    globals = cdr(globals);
  }
  if (codesize > CODESIZE) error(PSTR("not enough room for code"), var);

  // Compact the code block, removing gaps
  origin = 0;
  object *block;
  int smallest;

  do {
    smallest = CODESIZE;
    globals = GlobalEnv;
    while (globals != NULL) {
      object *pair = car(globals);
      if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
        object *codeid = second(pair);
        if (codeid->type == CODE) {
          if (startblock(codeid) < smallest && startblock(codeid) >= origin) {
            smallest = startblock(codeid);
            block = codeid;
          }
        }
      }
      globals = cdr(globals);
    }

    // Compact fragmentation if necessary
    if (smallest == origin) origin = endblock(block); // No gap
    else if (smallest < CODESIZE) { // Slide block down
      int target = origin;
      for (int i=startblock(block); i<endblock(block); i++) {
        MyCode[target] = MyCode[i];
        target++;
      }
      block->integer = target<<8 | origin;
      origin = target;
    }

  } while (smallest < CODESIZE);

  // Second pass - origin is first free location
  codesize = assemble(2, origin, cdr(args), env, pcpair);

  object *val = cons(codehead((origin+codesize)<<8 | origin), args);
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);

  #if defined(CPU_ATmega1284P)
  // Use Optiboot Flasher in MightyCore with 256 byte page from CODE_ADDRESS 0x1bb00 to 0x1bbff
  optiboot_page_erase(CODE_ADDRESS);
  for (unsigned int i=0; i<CODESIZE/2; i++) optiboot_page_fill(CODE_ADDRESS + i*2, MyCode[i*2] | MyCode[i*2+1]<<8);
  optiboot_page_write(CODE_ADDRESS);
  #elif defined (CPU_AVR128DX48)
  // Use Flash Writer in DxCore with 512 byte page from CODE_ADDRESS 0x1be00 to 0x1c000
  if (Flash.checkWritable()) error2(PSTR("flash write not supported"));
  if (Flash.erasePage(CODE_ADDRESS, 1)) error2(PSTR("problem erasing flash"));
  Flash.writeBytes(CODE_ADDRESS, MyCode, CODESIZE);
  #endif
  
  clrflag(NOESC);
  return var;
#else
  (void) args, (void) env;
  return nil;
#endif
}"#)

      #+arm
      (DEFCODE nil 0 127 #"
/*
  (defcode name (parameters) form*)
  Creates a machine-code function called name from a series of 16-bit integers given in the body of the form.
  These are written into RAM, and can be executed by calling the function in the same way as a normal Lisp function.
*/
object *sp_defcode (object *args, object *env) {
#if defined(CODESIZE)
  setflag(NOESC);
  object *var = first(args);
  object *params = second(args);
  if (!symbolp(var)) error(PSTR("not a symbol"), var);

  // Make parameters into synonyms for registers r0, r1, etc
  int regn = 0;
  while (params != NULL) {
    if (regn > 3) error(PSTR("more than 4 parameters"), var);
    object *regpair = cons(car(params), bsymbol((builtin_t)((toradix40('r')*40+toradix40('0')+regn)*2560000))); // Symbol for r0 etc
    push(regpair,env);
    regn++;
    params = cdr(params);
  }

  // Make *pc* a local variable for program counter
  object *pcpair = cons(bsymbol(PSTAR), number(0));
  push(pcpair,env);

  args = cdr(args);

  // Make labels into local variables
  object *entries = cdr(args);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      object *pair = cons(arg,number(0));
      push(pair,env);
    }
    entries = cdr(entries);
  } 

  // First pass
  int origin = 0;
  int codesize = assemble(1, origin, cdr(args), env, pcpair);

  // See if it will fit
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = car(globals);
    if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
      object *codeid = second(pair);
      if (codeid->type == CODE) {
        codesize = codesize + endblock(codeid) - startblock(codeid);
      }
    }
    globals = cdr(globals);
  }
  if (codesize > CODESIZE) error(PSTR("not enough room for code"), var);
  
  // Compact the code block, removing gaps
  origin = 0;
  object *block;
  int smallest;

  do {
    smallest = CODESIZE;
    globals = GlobalEnv;
    while (globals != NULL) {
      object *pair = car(globals);
      if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
        object *codeid = second(pair);
        if (codeid->type == CODE) {
          if (startblock(codeid) < smallest && startblock(codeid) >= origin) {
            smallest = startblock(codeid);
            block = codeid;
          }        
        }
      }
      globals = cdr(globals);
    }

    // Compact fragmentation if necessary
    if (smallest == origin) origin = endblock(block); // No gap
    else if (smallest < CODESIZE) { // Slide block down
      int target = origin;
      for (int i=startblock(block); i<endblock(block); i++) {
        MyCode[target] = MyCode[i];
        target++;
      }
      block->integer = target<<16 | origin;
      origin = target;
    }

  } while (smallest < CODESIZE);

  // Second pass - origin is first free location
  codesize = assemble(2, origin, cdr(args), env, pcpair);

  object *val = cons(codehead((origin+codesize)<<16 | origin), args);
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  clrflag(NOESC);
  return var;
#else
  error2(PSTR("not available"));
  return nil;
#endif
}"#)

      #+riscv
      (DEFCODE nil 0 127 #"
/*
  (defcode name (parameters) form*)
  Creates a machine-code function called name from a series of 16-bit integers given in the body of the form.
  These are written into RAM, and can be executed by calling the function in the same way as a normal Lisp function.
*/
object *sp_defcode (object *args, object *env) {
  setflag(NOESC);
  object *var = first(args);
  object *params = second(args);
  if (!symbolp(var)) error(PSTR("not a symbol"), var);

  // Make parameters into synonyms for registers a0, a1, etc
  int regn = 0;
  while (params != NULL) {
    if (regn > 3) error(PSTR("more than 4 parameters"), var);
    object *regpair = cons(car(params), bsymbol((builtin_t)((toradix40('a')*40+toradix40('0')+regn)*2560000))); // Symbol for a0 etc
    push(regpair,env);
    regn++;
    params = cdr(params);
  }
  
  // Make *pc* a local variable
  object *pcpair = cons(bsymbol(PSTAR), number(0));
  push(pcpair,env);
  args = cdr(args);
  
  // Make labels into local variables
  object *entries = cdr(args);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      object *pair = cons(arg,number(0));
      push(pair,env);
    }
    entries = cdr(entries);
  } 

  // First pass
  int origin = 0;
  int codesize = assemble(1, origin, cdr(args), env, pcpair);

  // See if it will fit
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = car(globals);
    if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
      object *codeid = second(pair);
      if (codeid->type == CODE) {
        codesize = codesize + endblock(codeid) - startblock(codeid);
      }
    }
    globals = cdr(globals);
  }
  if (codesize > CODESIZE) error(PSTR("not enough room for code"), var);
  
  // Compact the code block, removing gaps
  origin = 0;
  object *block;
  int smallest;

  do {
    smallest = CODESIZE;
    globals = GlobalEnv;
    while (globals != NULL) {
      object *pair = car(globals);
      if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
        object *codeid = second(pair);
        if (codeid->type == CODE) {
          if (startblock(codeid) < smallest && startblock(codeid) >= origin) {
            smallest = startblock(codeid);
            block = codeid;
          }
        }
      }
      globals = cdr(globals);
    }

    // Compact fragmentation if necessary
    if (smallest == origin) origin = endblock(block); // No gap
    else if (smallest < CODESIZE) { // Slide block down
      int target = origin;
      for (int i=startblock(block); i<endblock(block); i++) {
        MyCode[target] = MyCode[i];
        target++;
      }
      block->integer = target<<16 | origin;
      origin = target;
    }

  } while (smallest < CODESIZE);

  // Second pass - origin is first free location
  codesize = assemble(2, origin, cdr(args), env, pcpair);

  object *val = cons(codehead((origin+codesize)<<16 | origin), args);
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  clrflag(NOESC);
  return var;
}"#)) "sp")

    ("Tail-recursive forms"
     ((PROGN nil 0 127 "
/*
  (progn form*)
  Evaluates several forms grouped together into a block, and returns the result of evaluating the last form.
*/
object *tf_progn (object *args, object *env) {
  if (args == NULL) return nil;
  object *more = cdr(args);
  while (more != NULL) {
    object *result = eval(car(args),env);
    if (tstflag(RETURNFLAG)) return quote(result);
    args = more;
    more = cdr(args);
  }
  return car(args);
}")

      (IF nil 2 3 #"
/*
  (if test then [else])
  Evaluates test. If it's non-nil the form then is evaluated and returned;
  otherwise the form else is evaluated and returned.
*/
object *tf_if (object *args, object *env) {
  if (args == NULL || cdr(args) == NULL) error2(toofewargs);
  if (eval(first(args), env) != nil) return second(args);
  args = cddr(args);
  return (args != NULL) ? first(args) : nil;
}"#)

      (COND nil 0 127 #"
/*
  (cond ((test form*) (test form*) ... ))
  Each argument is a list consisting of a test optionally followed by one or more forms.
  If the test evaluates to non-nil the forms are evaluated, and the last value is returned as the result of the cond.
  If the test evaluates to nil, none of the forms are evaluated, and the next argument is processed in the same way.
*/
object *tf_cond (object *args, object *env) {
  while (args != NULL) {
    object *clause = first(args);
    if (!consp(clause)) error(illegalclause, clause);
    object *test = eval(first(clause), env);
    object *forms = cdr(clause);
    if (test != nil) {
      if (forms == NULL) return quote(test); else return tf_progn(forms, env);
    }
    args = cdr(args);
  }
  return nil;
}"#)
      
      (WHEN nil 1 127 #"
/*
  (when test form*)
  Evaluates the test. If it's non-nil the forms are evaluated and the last value is returned.
*/
object *tf_when (object *args, object *env) {
  if (args == NULL) error2(noargument);
  if (eval(first(args), env) != nil) return tf_progn(cdr(args),env);
  else return nil;
}"#)

      (UNLESS nil 1 127 #"
/*
  (unless test form*)
  Evaluates the test. If it's nil the forms are evaluated and the last value is returned.
*/
object *tf_unless (object *args, object *env) {
  if (args == NULL) error2(noargument);
  if (eval(first(args), env) != nil) return nil;
  else return tf_progn(cdr(args),env);
}"#)

      (CASE nil 1 127 #"
/*
  (case keyform ((key form*) (key form*) ... ))
  Evaluates a keyform to produce a test key, and then tests this against a series of arguments,
  each of which is a list containing a key optionally followed by one or more forms.
*/
object *tf_case (object *args, object *env) {
  object *test = eval(first(args), env);
  args = cdr(args);
  while (args != NULL) {
    object *clause = first(args);
    if (!consp(clause)) error(illegalclause, clause);
    object *key = car(clause);
    object *forms = cdr(clause);
    if (consp(key)) {
      while (key != NULL) {
        if (eq(test,car(key))) return tf_progn(forms, env);
        key = cdr(key);
      }
    } else if (eq(test,key) || eq(key,tee)) return tf_progn(forms, env);
    args = cdr(args);
  }
  return nil;
}"#)

      (AND nil 0 127 "
/*
  (and item*)
  Evaluates its arguments until one returns nil, and returns the last value.
*/
object *tf_and (object *args, object *env) {
  if (args == NULL) return tee;
  object *more = cdr(args);
  while (more != NULL) {
    if (eval(car(args), env) == NULL) return nil;
    args = more;
    more = cdr(args);
  }
  return car(args);
}")) "tf")

    ("Core functions"
     ((NOT nil 1 1 "
/*
  (not item)
  Returns t if its argument is nil, or nil otherwise. Equivalent to null.
*/
object *fn_not (object *args, object *env) {
  (void) env;
  return (first(args) == nil) ? tee : nil;
}")

      (NULLFN "null" 1 1 (not))

      (CONS nil 2 2 "
/*
  (cons item item)
  If the second argument is a list, cons returns a new list with item added to the front of the list.
  If the second argument isn't a list cons returns a dotted pair.
*/
object *fn_cons (object *args, object *env) {
  (void) env;
  return cons(first(args), second(args));
}")

      (ATOM nil 1 1 "
/*
  (atom item)
  Returns t if its argument is a single number, symbol, or nil.
*/
object *fn_atom (object *args, object *env) {
  (void) env;
  return atom(first(args)) ? tee : nil;
}")

      (LISTP nil 1 1 "
/*
  (listp item)
  Returns t if its argument is a list.
*/
object *fn_listp (object *args, object *env) {
  (void) env;
  return listp(first(args)) ? tee : nil;
}")

      (CONSP nil 1 1 "
/*
  (consp item)
  Returns t if its argument is a non-null list.
*/
object *fn_consp (object *args, object *env) {
  (void) env;
  return consp(first(args)) ? tee : nil;
}")

      (SYMBOLP nil 1 1 #"
/*
  (symbolp item)
  Returns t if its argument is a symbol.
*/
object *fn_symbolp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (arg == NULL || symbolp(arg)) ? tee : nil;
}"#)

      #-avr-nano
      (ARRAYP nil 1 1 #"
/*
  (arrayp item)
  Returns t if its argument is an array.
*/
object *fn_arrayp (object *args, object *env) {
  (void) env;
  return arrayp(first(args)) ? tee : nil;
}"#)

      (BOUNDP nil 1 1 #"
/*
  (boundp item)
  Returns t if its argument is a symbol with a value.
*/
object *fn_boundp (object *args, object *env) {
  return boundp(first(args), env) ? tee : nil;
}"#)

      #+avr-nano
      (KEYWORDP nil 1 1 #"
/*
  (keywordp item)
  Returns t if its argument is a keyword.
*/
object *fn_keywordp (object *args, object *env) {
  (void) env;
  return keywordp(first(args)) ? tee : nil;
}"#)

      #-avr-nano
      (KEYWORDP nil 1 1 #"
/*
  (keywordp item)
  Returns t if its argument is a built-in or user-defined keyword.
*/
object *fn_keywordp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!symbolp(arg)) return nil;
  return (keywordp(arg) || colonp(arg->name)) ? tee : nil;
}"#)

      #-avr-nano
      (SETFN "set" 2 126 #"
/*
  (set symbol value [symbol value]*)
  For each pair of arguments, assigns the value of the second argument to the value of the first argument.
*/
object *fn_setfn (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(oddargs);
    object *pair = findvalue(first(args), env);
    arg = second(args);
    cdr(pair) = arg;
    args = cddr(args);
  }
  return arg;
}"#)

      (STREAMP nil 1 1 #"
/*
  (streamp item)
  Returns t if its argument is a stream.
*/
object *fn_streamp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return streamp(arg) ? tee : nil;
}"#)

      (EQ nil 2 2 "
/*
  (eq item item)
  Tests whether the two arguments are the same symbol, same character, equal numbers,
  or point to the same cons, and returns t or nil as appropriate.
*/
object *fn_eq (object *args, object *env) {
  (void) env;
  return eq(first(args), second(args)) ? tee : nil;
}")

      (EQUAL nil 2 2 "
/*
  (equal item item)
  Tests whether the two arguments are the same symbol, same character, equal numbers,
  or point to the same cons, and returns t or nil as appropriate.
*/
object *fn_equal (object *args, object *env) {
  (void) env;
  return equal(first(args), second(args)) ? tee : nil;
}")))

    ("List functions"

     ((CAR nil 1 1 #"
/*
  (car list)
  Returns the first item in a list. 
*/
object *fn_car (object *args, object *env) {
  (void) env;
  return carx(first(args));
}"#)

      (FIRST nil 1 1 (car))

      (CDR nil 1 1 #"
/*
  (cdr list)
  Returns a list with the first item removed.
*/
object *fn_cdr (object *args, object *env) {
  (void) env;
  return cdrx(first(args));
}"#)

      (REST nil 1 1 (cdr))

      (CAAR nil 1 1 #"
/*
  (caar list)
*/
object *fn_caar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b100);
}"#)

      (CADR nil 1 1 #"
/*
  (cadr list)
*/
object *fn_cadr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b101);
}"#)

      (SECOND nil 1 1 (cadr))

      (CDAR nil 1 1 #"
/*
  (cdar list)
  Equivalent to (cdr (car list)).
*/
object *fn_cdar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b110);
}"#)

      (CDDR nil 1 1 #"
/*
  (cddr list)
  Equivalent to (cdr (cdr list)).
*/
object *fn_cddr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b111);
}"#)

      (CAAAR nil 1 1 #"
/*
  (caaar list)
  Equivalent to (car (car (car list))). 
*/
object *fn_caaar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1000);
}"#)

      (CAADR nil 1 1 #"
/*
  (caadr list)
  Equivalent to (car (car (cdar list))).
*/
object *fn_caadr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1001);;
}"#)

      (CADAR nil 1 1 #"
/*
  (cadar list)
  Equivalent to (car (cdr (car list))).
*/
object *fn_cadar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1010);
}"#)

      (CADDR nil 1 1 #"
/*
  (caddr list)
  Equivalent to (car (cdr (cdr list))).
*/
object *fn_caddr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1011);
}"#)

      (THIRD nil 1 1 (caddr))

      (CDAAR nil 1 1 #"
/*
  (cdaar list)
  Equivalent to (cdar (car (car list))).
*/
object *fn_cdaar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1100);
}"#)

      (CDADR nil 1 1 #"
/*
  (cdadr list)
  Equivalent to (cdr (car (cdr list))).
*/
object *fn_cdadr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1101);
}"#)

      (CDDAR nil 1 1 #"
/*
  (cddar list)
  Equivalent to (cdr (cdr (car list))).
*/
object *fn_cddar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1110);
}"#)

      (CDDDR nil 1 1 #"
/*
  (cdddr list)
  Equivalent to (cdr (cdr (cdr list))).
*/
object *fn_cdddr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1111);
}"#)

      #-arrays
      (LENGTH nil 1 1 #"
/*
  (length item)
  Returns the number of items in a list, or the length of a string.
*/
object *fn_length (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (listp(arg)) return number(listlength(arg));
  if (!stringp(arg)) error(invalidarg, arg);
  return number(stringlength(arg));
}"#)
      
      #+arrays
      (LENGTH nil 1 1 #"
/*
  (length item)
  Returns the number of items in a list, the length of a string, or the length of a one-dimensional array.
*/
object *fn_length (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (listp(arg)) return number(listlength(arg));
  if (stringp(arg)) return number(stringlength(arg));
  if (!(arrayp(arg) && cdr(cddr(arg)) == NULL)) error(PSTR("argument is not a list, 1d array, or string"), arg);
  return number(abs(first(cddr(arg))->integer));
}"#)

      #+arrays
      (ARRAYDIMENSIONS "array-dimensions" 1 1 #"
/*
  (array-dimensions item)
  Returns a list of the dimensions of an array.
*/
object *fn_arraydimensions (object *args, object *env) {
  (void) env;
  object *array = first(args);
  if (!arrayp(array)) error(PSTR("argument is not an array"), array);
  object *dimensions = cddr(array);
  return (first(dimensions)->integer < 0) ? cons(number(-(first(dimensions)->integer)), cdr(dimensions)) : dimensions;
}"#)

      (LIST nil 0 127 "
/*
  (list item*)
  Returns a list of the values of its arguments.
*/
object *fn_list (object *args, object *env) {
  (void) env;
  return args;
}")

      #-avr-nano
      (COPYLIST "copy-list" 1 1 "
/*
  (copy-list list)
  Returns a copy of a list.
*/
object *fn_copylist (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!listp(arg)) error(notalist, arg);
  object *result = cons(NULL, NULL);
  object *ptr = result;
  while (arg != NULL) {
    cdr(ptr) = cons(car(arg), NULL); 
    ptr = cdr(ptr); arg = cdr(arg);
  }
  return cdr(result);
}")

      #+arrays
      (MAKEARRAY "make-array" 1 5 #"
/*
  (make-array size [:initial-element element] [:element-type 'bit])
  If size is an integer it creates a one-dimensional array with elements from 0 to size-1.
  If size is a list of n integers it creates an n-dimensional array with those dimensions.
  If :element-type 'bit is specified the array is a bit array.
*/
object *fn_makearray (object *args, object *env) {
  (void) env;
  object *def = nil;
  bool bitp = false;
  object *dims = first(args);
  if (dims == NULL) error2(PSTR("dimensions can't be nil"));
  else if (atom(dims)) dims = cons(dims, NULL);
  args = cdr(args);
  while (args != NULL && cdr(args) != NULL) {
    object *var = first(args);
    if (isbuiltin(first(args), INITIALELEMENT)) def = second(args);
    else if (isbuiltin(first(args), ELEMENTTYPE) && isbuiltin(second(args), BIT)) bitp = true;
    else error(PSTR("argument not recognised"), var);
    args = cddr(args);
  }
  if (bitp) {
    if (def == nil) def = number(0);
    else def = number(-checkbitvalue(def)); // 1 becomes all ones
  }
  return makearray(dims, def, bitp);
}"#)

      (REVERSE nil 1 1 #"
/*
  (reverse list)
  Returns a list with the elements of list in reverse order.
*/
object *fn_reverse (object *args, object *env) {
  (void) env;
  object *list = first(args);
  object *result = NULL;
  while (list != NULL) {
    if (improperp(list)) error(notproper, list);
    push(first(list),result);
    list = cdr(list);
  }
  return result;
}"#)

      (NTH nil 2 2 #"
/*
  (nth number list)
  Returns the nth item in list, counting from zero.
*/
object *fn_nth (object *args, object *env) {
  (void) env;
  int n = checkinteger(first(args));
  if (n < 0) error(indexnegative, first(args));
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(notproper, list);
    if (n == 0) return car(list);
    list = cdr(list);
    n--;
  }
  return nil;
}"#)

      #+arrays
      (AREF nil 2 127 #"
/*
  (aref array index [index*])
  Returns an element from the specified array.
*/
object *fn_aref (object *args, object *env) {
  (void) env;
  int bit;
  object *array = first(args);
  if (!arrayp(array)) error(PSTR("first argument is not an array"), array);
  object *loc = *getarray(array, cdr(args), 0, &bit);
  if (bit == -1) return loc;
  else return number((loc->integer)>>bit & 1);
}"#)

      #+avr-nano
      (ASSOC nil 2 4 #"
/*
  (assoc key list)
  Looks up a key in an association list of (key . value) pairs,
  and returns the matching pair, or nil if no pair is found.
*/
object *fn_assoc (object *args, object *env) {
  (void) env;
  object *key = first(args);
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(notproper, list);
    object *pair = first(list);
    if (!listp(pair)) error(PSTR("element is not a list"), pair);
    if (pair != NULL && eq(key,car(pair))) return pair;
    list = cdr(list);
  }
  return nil;
}"#)

      #-avr-nano
      (ASSOC nil 2 4 #"
/*
  (assoc key list [:test function])
  Looks up a key in an association list of (key . value) pairs, using eq or the specified test function,
  and returns the matching pair, or nil if no pair is found.
*/
object *fn_assoc (object *args, object *env) {
  (void) env;
  object *key = first(args);
  object *list = second(args);
  object *test = testargument(cddr(args));
  while (list != NULL) {
    if (improperp(list)) error(notproper, list);
    object *pair = first(list);
    if (!listp(pair)) error(PSTR("element is not a list"), pair);
    if (pair != NULL && apply(test, cons(key, cons(car(pair), NULL)), env) != NULL) return pair;
    list = cdr(list);
  }
  return nil;
}"#)

      #+avr-nano
      (MEMBER nil 2 4 #"
/*
  (member item list)
  Searches for an item in a list, using eq, and returns the list starting from the first occurrence of the item,
  or nil if it is not found.
*/
object *fn_member (object *args, object *env) {
  (void) env;
  object *item = first(args);
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(notproper, list);
    if (eq(item,car(list))) return list;
    list = cdr(list);
  }
  return nil;
}"#)

      #-avr-nano
      (MEMBER nil 2 4 #"
/*
  (member item list [:test function])
  Searches for an item in a list, using eq or the specified test function, and returns the list starting
  from the first occurrence of the item, or nil if it is not found.
*/
object *fn_member (object *args, object *env) {
  (void) env;
  object *item = first(args);
  object *list = second(args);
  object *test = testargument(cddr(args));
  while (list != NULL) {
    if (improperp(list)) error(notproper, list);
    if (apply(test, cons(item, cons(car(list), NULL)), env) != NULL) return list;
    list = cdr(list);
  }
  return nil;
}"#)

      (APPLY nil 2 127 #"
/*
  (apply function list)
  Returns the result of evaluating function, with the list of arguments specified by the second parameter.
*/
object *fn_apply (object *args, object *env) {
  object *previous = NULL;
  object *last = args;
  while (cdr(last) != NULL) {
    previous = last;
    last = cdr(last);
  }
  object *arg = car(last);
  if (!listp(arg)) error(notalist, arg);
  cdr(previous) = arg;
  return apply(first(args), cdr(args), env);
}"#)

      (FUNCALL nil 1 127 "
/*
  (funcall function argument*)
  Evaluates function with the specified arguments.
*/
object *fn_funcall (object *args, object *env) {
  return apply(first(args), cdr(args), env);
}")

      (APPEND nil 0 127 #"
/*
  (append list*)
  Joins its arguments, which should be lists, into a single list.
*/
object *fn_append (object *args, object *env) {
  (void) env;
  object *head = NULL;
  object *tail;
  while (args != NULL) {
    object *list = first(args);
    if (!listp(list)) error(notalist, list);
    while (consp(list)) {
      object *obj = cons(car(list), cdr(list));
      if (head == NULL) head = obj;
      else cdr(tail) = obj;
      tail = obj;
      list = cdr(list);
      if (cdr(args) != NULL && improperp(list)) error(notproper, first(args));
    }
    args = cdr(args);
  }
  return head;
}"#)

      #+avr-nano
      (MAPC nil 2 127 #"
/*
  (mapc function list1 [list]*)
  Applies the function to each element in one or more lists, ignoring the results.
  It returns the first list argument.
*/
object *fn_mapc (object *args, object *env) {
  object *function = first(args);
  args = cdr(args);
  object *result = first(args);
  push(result,GCStack);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack); pop(GCStack);
         return result;
      }
      if (improperp(list)) error(notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    apply(function, cdr(params), env);
  }
}"#)

      #-avr-nano
      (MAPC nil 2 127 #"
/*
  (mapc function list1 [list]*)
  Applies the function to each element in one or more lists, ignoring the results.
  It returns the first list argument.
*/
object *fn_mapc (object *args, object *env) {
  return mapcl(args, env, false);
}"#)

      #-avr-nano
      (MAPL nil 2 127 #"
/*
  (mapl function list1 [list]*)
  Applies the function to one or more lists and then successive cdrs of those lists,
  ignoring the results. It returns the first list argument.
*/
object *fn_mapl (object *args, object *env) {
  return mapcl(args, env, true);
}"#)

      #+avr-nano
      (MAPCAR nil 2 127 #"
/*
  (mapcar function list1 [list]*)
  Applies the function to each element in one or more lists, and returns the resulting list.
*/
object *fn_mapcar (object *args, object *env) {
  return mapcarcan(args, env, mapcarfun);
}"#)

      #-avr-nano
      (MAPCAR nil 2 127 #"
/*
  (mapcar function list1 [list]*)
  Applies the function to each element in one or more lists, and returns the resulting list.
*/
object *fn_mapcar (object *args, object *env) {
  return mapcarcan(args, env, mapcarfun, false);
}"#)

      #+avr-nano
      (MAPCAN nil 2 127 #"
/*
  (mapcan function list1 [list]*)
  Applies the function to each element in one or more lists. The results should be lists,
  and these are destructively concatenated together to give the value returned.
*/
object *fn_mapcan (object *args, object *env) {
  return mapcarcan(args, env, mapcanfun);
}"#)

      #-avr-nano
      (MAPCAN nil 2 127 #"
/*
  (mapcan function list1 [list]*)
  Applies the function to each element in one or more lists. The results should be lists,
  and these are destructively concatenated together to give the value returned.
*/
object *fn_mapcan (object *args, object *env) {
  return mapcarcan(args, env, mapcanfun, false);
}"#)

      #-avr-nano
      (MAPLIST nil 2 127 #"
/*
  (maplist function list1 [list]*)
  Applies the function to one or more lists and then successive cdrs of those lists,
  and returns the resulting list.
*/
object *fn_maplist (object *args, object *env) {
  return mapcarcan(args, env, mapcarfun, true);
}"#)

      #-avr-nano
      (MAPCON nil 2 127 #"
/*
  (mapcon function list1 [list]*)
  Applies the function to one or more lists and then successive cdrs of those lists,
  and these are destructively concatenated together to give the value returned.
*/
object *fn_mapcon (object *args, object *env) {
  return mapcarcan(args, env, mapcanfun, true);
}"#)))

    ("Arithmetic functions"
     (

      #-float
      (ADD "+" 0 127 #"
/*
  (+ number*)
  Adds its arguments together.
*/
object *fn_add (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    int temp = checkinteger(car(args));
    #if defined(checkoverflow)
    if (temp < 1) { if (INT_MIN - temp > result) error2(overflow); }
    else { if (INT_MAX - temp < result) error2(overflow); }
    #endif
    result = result + temp;
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (ADD "+" 0 127 #"
/*
  (+ number*)
  Adds its arguments together.
  If each argument is an integer, and the running total doesn't overflow, the result is an integer,
  otherwise a floating-point number.
*/
object *fn_add (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    object *arg = car(args);
    if (floatp(arg)) return add_floats(args, (float)result);
    else if (integerp(arg)) {
      int val = arg->integer;
      if (val < 1) { if (INT_MIN - val > result) return add_floats(args, (float)result); }
      else { if (INT_MAX - val < result) return add_floats(args, (float)result); }
      result = result + val;
    } else error(notanumber, arg);
    args = cdr(args);
  }
  return number(result);
}"#)

      #-float
      (SUBTRACT "-" 1 127 #"
/*
  (- number*)
  If there is one argument, negates the argument.
  If there are two or more arguments, subtracts the second and subsequent arguments from the first argument.
*/
object *fn_subtract (object *args, object *env) {
  (void) env;
  int result = checkinteger(car(args));
  args = cdr(args);
  if (args == NULL) {
    #if defined(checkoverflow)
    if (result == INT_MIN) error2(overflow);
    #endif
    return number(-result);
  }
  while (args != NULL) {
    int temp = checkinteger(car(args));
    #if defined(checkoverflow)
    if (temp < 1) { if (INT_MAX + temp < result) error2(overflow); }
    else { if (INT_MIN + temp > result) error2(overflow); }
    #endif
    result = result - temp;
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (SUBTRACT "-" 1 127 #"
/*
  (- number*)
  If there is one argument, negates the argument.
  If there are two or more arguments, subtracts the second and subsequent arguments from the first argument.
  If each argument is an integer, and the running total doesn't overflow, returns the result as an integer,
  otherwise a floating-point number.
*/
object *fn_subtract (object *args, object *env) {
  (void) env;
  object *arg = car(args);
  args = cdr(args);
  if (args == NULL) return negate(arg);
  else if (floatp(arg)) return subtract_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) return subtract_floats(args, result);
      else if (integerp(arg)) {
        int val = (car(args))->integer;
        if (val < 1) { if (INT_MAX + val < result) return subtract_floats(args, result); }
        else { if (INT_MIN + val > result) return subtract_floats(args, result); }
        result = result - val;
      } else error(notanumber, arg);
      args = cdr(args);
    }
    return number(result);
  } else error(notanumber, arg);
  return nil;
}"#)

      #-float
      (MULTIPLY "*" 0 127 #"
/*
  (* number*)
  Multiplies its arguments together.
*/
object *fn_multiply (object *args, object *env) {
  (void) env;
  int result = 1;
  while (args != NULL){
    #if defined(checkoverflow)
    signed long temp = (signed long) result * checkinteger(car(args));
    if ((temp > INT_MAX) || (temp < INT_MIN)) error2(overflow);
    result = temp;
    #else
    result = result * checkinteger(car(args));
    #endif
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (MULTIPLY "*" 0 127 #"
/*
  (* number*)
  Multiplies its arguments together.
  If each argument is an integer, and the running total doesn't overflow, the result is an integer,
  otherwise it's a floating-point number.
*/
object *fn_multiply (object *args, object *env) {
  (void) env;
  int result = 1;
  while (args != NULL){
    object *arg = car(args);
    if (floatp(arg)) return multiply_floats(args, result);
    else if (integerp(arg)) {
      int64_t val = result * (int64_t)(arg->integer);
      if ((val > INT_MAX) || (val < INT_MIN)) return multiply_floats(args, result);
      result = val;
    } else error(notanumber, arg);
    args = cdr(args);
  }
  return number(result);
}"#)

      #-float
      (DIVIDE "/" 2 127 #"
/*
  (/ number*)
  Divides the first argument by the second and subsequent arguments.
*/
object *fn_divide (object *args, object *env) {
  (void) env;
  int result = checkinteger(first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg = checkinteger(car(args));
    if (arg == 0) error2(divisionbyzero);
    #if defined(checkoverflow)
    if ((result == INT_MIN) && (arg == -1)) error2(overflow);
    #endif
    result = result / arg;
    args = cdr(args);
  }
  return number(result);
}"#)

     #-float
     (TRUNCATE nil 1 2 (divide))

     #+float
     (DIVIDE "/" 1 127 #"
/*
  (/ number*)
  Divides the first argument by the second and subsequent arguments.
  If each argument is an integer, and each division produces an exact result, the result is an integer;
  otherwise it's a floating-point number.
*/
object *fn_divide (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  args = cdr(args);
  // One argument
  if (args == NULL) {
    if (floatp(arg)) {
      float f = arg->single_float;
      if (f == 0.0) error2(divisionbyzero);
      return makefloat(1.0 / f);
    } else if (integerp(arg)) {
      int i = arg->integer;
      if (i == 0) error2(divisionbyzero);
      else if (i == 1) return number(1);
      else return makefloat(1.0 / i);
    } else error(notanumber, arg);
  }
  // Multiple arguments
  if (floatp(arg)) return divide_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) {
        return divide_floats(args, result);
      } else if (integerp(arg)) {
        int i = arg->integer;
        if (i == 0) error2(divisionbyzero);
        if ((result % i) != 0) return divide_floats(args, result);
        if ((result == INT_MIN) && (i == -1)) return divide_floats(args, result);
        result = result / i;
        args = cdr(args);
      } else error(notanumber, arg);
    }
    return number(result);
  } else error(notanumber, arg);
  return nil;
}"#)

     #+avr-nano
     (MOD nil 2 2 #"
/*
  (mod number number)
  Returns its first argument modulo the second argument.
  If both arguments are integers the result is an integer; otherwise it's a floating-point number.
*/
object *fn_mod (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(first(args));
  int arg2 = checkinteger(second(args));
  if (arg2 == 0) error2(divisionbyzero);
  int r = arg1 % arg2;
  if ((arg1<0) != (arg2<0)) r = r + arg2;
  return number(r);
}"#)

     #-avr-nano
     (MOD nil 2 2 #"
/*
  (mod number number)
  Returns its first argument modulo the second argument.
  If both arguments are integers the result is an integer; otherwise it's a floating-point number.
*/
object *fn_mod (object *args, object *env) {
  (void) env;
  return remmod(args, true);
}"#)

     #-avr-nano
     (REM nil 2 2 #"
/*
  (rem number number)
  Returns the remainder from dividing the first argument by the second argument.
  If both arguments are integers the result is an integer; otherwise it's a floating-point number.
*/
object *fn_rem (object *args, object *env) {
  (void) env;
  return remmod(args, false);
}"#)

      #-float
      (ONEPLUS "1+" 1 1 #"
/*
  (1+ number)
  Adds one to its argument and returns it.
*/
object *fn_oneplus (object *args, object *env) {
  (void) env;
  int result = checkinteger(first(args));
  #if defined(checkoverflow)
  if (result == INT_MAX) error2(overflow);
  #endif
  return number(result + 1);
}"#)

      #+float
      (ONEPLUS "1+" 1 1 #"
/*
  (1+ number)
  Adds one to its argument and returns it.
  If the argument is an integer the result is an integer if possible;
  otherwise it's a floating-point number.
*/
object *fn_oneplus (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  if (floatp(arg)) return makefloat((arg->single_float) + 1.0);
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MAX) return makefloat((arg->integer) + 1.0);
    else return number(result + 1);
  } else error(notanumber, arg);
  return nil;
}"#)

      #-float
      (ONEMINUS "1-" 1 1 #"
/*
  (1- number)
  Subtracts one from its argument and returns it.
*/
object *fn_oneminus (object *args, object *env) {
  (void) env;
  int result = checkinteger(first(args));
  #if defined(checkoverflow)
  if (result == INT_MIN) error2(overflow);
  #endif
  return number(result - 1);
}"#)

      #+float
      (ONEMINUS "1-" 1 1 #"
/*
  (1- number)
  Subtracts one from its argument and returns it.
  If the argument is an integer the result is an integer if possible;
  otherwise it's a floating-point number.
*/
object *fn_oneminus (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  if (floatp(arg)) return makefloat((arg->single_float) - 1.0);
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat((arg->integer) - 1.0);
    else return number(result - 1);
  } else error(notanumber, arg);
  return nil;
}"#)

      #-float
      (ABS nil 1 1 #"
/*
  (abs number)
  Returns the absolute, positive value of its argument.
*/
object *fn_abs (object *args, object *env) {
  (void) env;
  int result = checkinteger(first(args));
  #if defined(checkoverflow)
  if (result == INT_MIN) error2(overflow);
  #endif
  return number(abs(result));
}"#)

      #+float
      (ABS nil 1 1 #"
/*
  (abs number)
  Returns the absolute, positive value of its argument.
  If the argument is an integer the result will be returned as an integer if possible,
  otherwise a floating-point number.
*/
object *fn_abs (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return makefloat(abs(arg->single_float));
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(abs((float)result));
    else return number(abs(result));
  } else error(notanumber, arg);
  return nil;
}"#)

      #-float
      (RANDOM nil 1 1 #"
/*
  (random number)
  Returns a random number between 0 and one less than its argument.
*/
object *fn_random (object *args, object *env) {
  (void) env;
  int arg = checkinteger(first(args));
  return number(pseudoRandom(arg));
}"#)

      #+float
      (RANDOM nil 1 1 #"
/*
  (random number)
  If number is an integer returns a random number between 0 and one less than its argument.
  Otherwise returns a floating-point number between zero and number.
*/
object *fn_random (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (integerp(arg)) return number(random(arg->integer));
  else if (floatp(arg)) return makefloat((float)rand()/(float)(RAND_MAX/(arg->single_float)));
  else error(notanumber, arg);
  return nil;
}"#)

      #-float
      (MAXFN "max" 1 127 #"
/*
  (max number*)
  Returns the maximum of one or more arguments.
*/
object *fn_maxfn (object *args, object *env) {
  (void) env;
  int result = checkinteger(first(args));
  args = cdr(args);
  while (args != NULL) {
    int next = checkinteger(car(args));
    if (next > result) result = next;
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (MAXFN "max" 1 127 #"
/*
  (max number*)
  Returns the maximum of one or more arguments.
*/
object *fn_maxfn (object *args, object *env) {
  (void) env;
  object* result = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg = car(args);
    if (integerp(result) && integerp(arg)) {
      if ((arg->integer) > (result->integer)) result = arg;
    } else if ((checkintfloat(arg) > checkintfloat(result))) result = arg;
    args = cdr(args);
  }
  return result;
}"#)

      #-float
      (MINFN "min" 1 127 #"
/*
  (min number*)
  Returns the minimum of one or more arguments.
*/
object *fn_minfn (object *args, object *env) {
  (void) env;
  int result = checkinteger(first(args));
  args = cdr(args);
  while (args != NULL) {
    int next = checkinteger(car(args));
    if (next < result) result = next;
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (MINFN "min" 1 127 #"
/*
  (min number*)
  Returns the minimum of one or more arguments.
*/
object *fn_minfn (object *args, object *env) {
  (void) env;
  object* result = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg = car(args);
    if (integerp(result) && integerp(arg)) {
      if ((arg->integer) < (result->integer)) result = arg;
    } else if ((checkintfloat(arg) < checkintfloat(result))) result = arg;
    args = cdr(args);
  }
  return result;
}"#)))

    ("Arithmetic comparisons"

     (

      #-float
      (NOTEQ "/=" 1 127 #"
/*
  (/= number*)
  Returns t if none of the arguments are equal, or nil if two or more arguments are equal.
*/
object *fn_noteq (object *args, object *env) {
  (void) env;
  while (args != NULL) {   
    object *nargs = args;
    int arg1 = checkinteger(first(nargs));
    nargs = cdr(nargs);
    while (nargs != NULL) {
       int arg2 = checkinteger(first(nargs));
       if (arg1 == arg2) return nil;
       nargs = cdr(nargs);
    }
    args = cdr(args);
  }
  return tee;
}"#)

      #+float
      (NOTEQ "/=" 1 127 #"
/*
  (/= number*)
  Returns t if none of the arguments are equal, or nil if two or more arguments are equal.
*/
object *fn_noteq (object *args, object *env) {
  (void) env;
  while (args != NULL) {
    object *nargs = args;
    object *arg1 = first(nargs);
    nargs = cdr(nargs);
    while (nargs != NULL) {
      object *arg2 = first(nargs);
      if (integerp(arg1) && integerp(arg2)) {
        if ((arg1->integer) == (arg2->integer)) return nil;
      } else if ((checkintfloat(arg1) == checkintfloat(arg2))) return nil;
      nargs = cdr(nargs);
    }
    args = cdr(args);
  }
  return tee;
}"#)

      (NUMEQ "=" 1 127 #"
/*
  (= number*)
  Returns t if all the arguments, which must be numbers, are numerically equal, and nil otherwise.
*/
object *fn_numeq (object *args, object *env) {
  (void) env;
  return compare(args, false, false, true);
}"#)

      (LESS "<" 1 127 #"
/*
  (< number*)
  Returns t if each argument is less than the next argument, and nil otherwise.
*/
object *fn_less (object *args, object *env) {
  (void) env;
  return compare(args, true, false, false);
}"#)

      (LESSEQ "<=" 1 127 #"
/*
  (<= number*)
  Returns t if each argument is less than or equal to the next argument, and nil otherwise.
*/
object *fn_lesseq (object *args, object *env) {
  (void) env;
  return compare(args, true, false, true);
}"#)

      (GREATER ">" 1 127 #"
/*
  (> number*)
  Returns t if each argument is greater than the next argument, and nil otherwise.
*/
object *fn_greater (object *args, object *env) {
  (void) env;
  return compare(args, false, true, false);
}"#)

      (GREATEREQ ">=" 1 127 #"
/*
  (>= number*)
  Returns t if each argument is greater than or equal to the next argument, and nil otherwise.
*/
object *fn_greatereq (object *args, object *env) {
  (void) env;
  return compare(args, false, true, true);
}"#)

      #-float
      (PLUSP nil 1 1 "
/*
  (plusp number)
  Returns t if the argument is greater than zero, or nil otherwise.
*/
object *fn_plusp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(first(args));
  if (arg > 0) return tee;
  else return nil;
}")

      #+float
      (PLUSP nil 1 1 "
/*
  (plusp number)
  Returns t if the argument is greater than zero, or nil otherwise.
*/
object *fn_plusp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) > 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) > 0) ? tee : nil;
  else error(notanumber, arg);
  return nil;
}")

      #-float
      (MINUSP nil 1 1 "
object *fn_minusp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(first(args));
  if (arg < 0) return tee;
  else return nil;
}")

      #+float
      (MINUSP nil 1 1 "
/*
  (minusp number)
  Returns t if the argument is less than zero, or nil otherwise.
*/
object *fn_minusp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) < 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) < 0) ? tee : nil;
  else error(notanumber, arg);
  return nil;
}")

      #-float
      (ZEROP nil 1 1 "
/*
  (zerop number)
  Returns t if the argument is zero.
*/
object *fn_zerop (object *args, object *env) {
  (void) env;
  int arg = checkinteger(first(args));
  return (arg == 0) ? tee : nil;
}")

      #+float
      (ZEROP nil 1 1 "
/*
  (zerop number)
  Returns t if the argument is zero.
*/
object *fn_zerop (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) == 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) == 0) ? tee : nil;
  else error(notanumber, arg);
  return nil;
}")

      (ODDP nil 1 1 "
/*
  (oddp number)
  Returns t if the integer argument is odd.
*/
object *fn_oddp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(first(args));
  return ((arg & 1) == 1) ? tee : nil;
}")

      (EVENP nil 1 1 "
/*
  (evenp number)
  Returns t if the integer argument is even.
*/
object *fn_evenp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(first(args));
  return ((arg & 1) == 0) ? tee : nil;
}")))

    ("Number functions"

     ((INTEGERP nil 1 1 #"
/*
  (integerp number)
  Returns t if the argument is an integer.
*/
object *fn_integerp (object *args, object *env) {
  (void) env;
  return integerp(first(args)) ? tee : nil;
}"#)

     #-float
     (NUMBERP nil 1 1 (integerp))

     #+float
     (NUMBERP nil 1 1 #"
/*
  (numberp number)
  Returns t if the argument is a number.
*/
object *fn_numberp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (integerp(arg) || floatp(arg)) ? tee : nil;
}"#)))

      #+float
      ("Floating-point functions"
       ((FLOATFN "float" 1 1 #"
/*
  (float number)
  Returns its argument converted to a floating-point number.
*/
object *fn_floatfn (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (floatp(arg)) ? arg : makefloat((float)(arg->integer));
}"#)

    (FLOATP nil 1 1 #"
/*
  (floatp number)
  Returns t if the argument is a floating-point number.
*/
object *fn_floatp (object *args, object *env) {
  (void) env;
  return floatp(first(args)) ? tee : nil;
}"#)

    (SIN nil 1 1 float-function)
    (COS nil 1 1 float-function)
    (TAN nil 1 1 float-function)
    (ASIN nil 1 1 float-function)
    (ACOS nil 1 1 float-function)
    (ATAN nil 1 2 #"
/*
  (atan number1 [number2])
  Returns the arc tangent of number1/number2, in radians. If number2 is omitted it defaults to 1.
*/
object *fn_atan (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  float div = 1.0;
  args = cdr(args);
  if (args != NULL) div = checkintfloat(first(args));
  return makefloat(atan2(checkintfloat(arg), div));
}"#)
      
    (SINH nil 1 1 float-function)
    (COSH nil 1 1 float-function)
    (TANH nil 1 1 float-function)
    (EXP nil 1 1 float-function)
    (SQRT nil 1 1 float-function)

    (LOG nil 1 2 #"
/*
  (log number [base])
  Returns the logarithm of number to the specified base. If base is omitted it defaults to e.
*/
object *fn_log (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  float fresult = log(checkintfloat(arg));
  args = cdr(args);
  if (args == NULL) return makefloat(fresult);
  else return makefloat(fresult / log(checkintfloat(first(args))));
}"#)
    
    (EXPT nil 2 2 #"
/*
  (expt number power)
  Returns number raised to the specified power.
  Returns the result as an integer if the arguments are integers and the result will be within range,
  otherwise a floating-point number.
*/
object *fn_expt (object *args, object *env) {
  (void) env;
  object *arg1 = first(args); object *arg2 = second(args);
  float float1 = checkintfloat(arg1);
  float value = log(abs(float1)) * checkintfloat(arg2);
  if (integerp(arg1) && integerp(arg2) && ((arg2->integer) >= 0) && (abs(value) < 21.4875))
    return number(intpower(arg1->integer, arg2->integer));
  if (float1 < 0) {
    if (integerp(arg2)) return makefloat((arg2->integer & 1) ? -exp(value) : exp(value));
    else error2(PSTR("invalid result"));
  }
  return makefloat(exp(value));
}"#)
      
    (CEILING nil 1 2 truncate-function)
    (FLOOR nil 1 2 truncate-function)
      
    (TRUNCATE nil 1 2 #"
/*
  (truncate number [divisor])
  Returns the integer part of number/divisor. If divisor is omitted it defaults to 1.
*/
object *fn_truncate (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number((int)(checkintfloat(arg) / checkintfloat(first(args))));
  else return number((int)(checkintfloat(arg)));
}"#)

    (ROUND nil 1 2 #"
/*
  (round number [divisor])
  Returns the integer closest to number/divisor. If divisor is omitted it defaults to 1.
*/
object *fn_round (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(round(checkintfloat(arg) / checkintfloat(first(args))));
  else return number(round(checkintfloat(arg)));
}"#)))

    ("Characters"

     ((CHAR "char" 2 2 #"
/*
  (char string n)
  Returns the nth character in a string, counting from zero.
*/
object *fn_char (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!stringp(arg)) error(notastring, arg);
  object *n = second(args);
  char c = nthchar(arg, checkinteger(n));
  if (c == 0) error(indexrange, n);
  return character(c);
}"#)

      (CHARCODE "char-code" 1 1 #"
/*
  (char-code character)
  Returns the ASCII code for a character, as an integer.
*/
object *fn_charcode (object *args, object *env) {
  (void) env;
  return number(checkchar(first(args)));
}"#)

      (CODECHAR "code-char" 1 1 #"
/*
  (code-char integer)
  Returns the character for the specified ASCII code.
*/
object *fn_codechar (object *args, object *env) {
  (void) env;
  return character(checkinteger(first(args)));
}"#)

      (CHARACTERP nil 1 1 #"
/*
  (characterp item)
  Returns t if the argument is a character and nil otherwise.
*/
object *fn_characterp (object *args, object *env) {
  (void) env;
  return characterp(first(args)) ? tee : nil;
}"#)))

      ("Strings"

     ((STRINGP nil 1 1 "
/*
  (stringp item)
  Returns t if the argument is a string and nil otherwise.
*/
object *fn_stringp (object *args, object *env) {
  (void) env;
  return stringp(first(args)) ? tee : nil;
}")

      #+avr-nano
      (STRINGEQ "string=" 2 2 #"
/*
  (string= string string)
  Returns t if the two strings are the same, or nil otherwise.
*/
object *fn_stringeq (object *args, object *env) {
  (void) env;
  return stringcompare(args, false, false, true) ? tee : nil;
}"#)

      #-avr-nano
      (STRINGEQ "string=" 2 2 #"
/*
  (string= string string)
  Returns t if the two strings are the same, or nil otherwise.
*/
object *fn_stringeq (object *args, object *env) {
  (void) env;
  int m = stringcompare(args, false, false, true);
  return m == -1 ? nil : tee;
}"#)

      #+avr-nano
      (STRINGLESS "string<" 2 2 #"
/*
  (string< string string)
  Returns t if the first string is alphabetically less than the second string,
  or nil otherwise.
*/
object *fn_stringless (object *args, object *env) {
  (void) env;
  return stringcompare(args, true, false, false) ? tee : nil;
}"#)

      #-avr-nano
      (STRINGLESS "string<" 2 2 #"
/*
  (string< string string)
  Returns the index to the first mismatch if the first string is alphabetically less than the second string,
  or nil otherwise.
*/
object *fn_stringless (object *args, object *env) {
  (void) env;
  int m = stringcompare(args, true, false, false);
  return m == -1 ? nil : number(m);
}"#)

      #+avr-nano
      (STRINGGREATER "string>" 2 2 #"
/*
  (string> string string)
  Returns t if the first string is alphabetically greater than the second string,
  or nil otherwise. 
*/
object *fn_stringgreater (object *args, object *env) {
  (void) env;
  return stringcompare(args, false, true, false) ? tee : nil;
}"#)

      #-avr-nano
      (STRINGGREATER "string>" 2 2 #"
/*
  (string> string string)
  Returns the index to the first mismatch if the first string is alphabetically greater than the second string,
  or nil otherwise. 
*/
object *fn_stringgreater (object *args, object *env) {
  (void) env;
  int m = stringcompare(args, false, true, false);
  return m == -1 ? nil : number(m);
}"#)

      #-avr-nano
      (STRINGNOTEQ "string/=" 2 2 #"
/*
  (string/= string string)
  Returns the index to the first mismatch if the two strings are not the same, or nil otherwise.
*/
object *fn_stringnoteq (object *args, object *env) {
  (void) env;
  int m = stringcompare(args, true, true, false);
  return m == -1 ? nil : number(m);
}"#)

      #-avr-nano
      (STRINGLESSEQ "string<=" 2 2 #"
/*
  (string<= string string)
  Returns the index to the first mismatch if the first string is alphabetically less than or equal to
  the second string, or nil otherwise. 
*/
object *fn_stringlesseq (object *args, object *env) {
  (void) env;
  int m = stringcompare(args, true, false, true);
  return m == -1 ? nil : number(m);
}"#)

      #-avr-nano
      (STRINGGREATEREQ "string>=" 2 2 #"
/*
  (string>= string string)
  Returns the index to the first mismatch if the first string is alphabetically greater than or equal to
  the second string, or nil otherwise.
*/
object *fn_stringgreatereq (object *args, object *env) {
  (void) env;
  int m = stringcompare(args, false, true, true);
  return m == -1 ? nil : number(m);
}"#)

      (SORT "sort" 2 2 #"
/*
  (sort list test)
  Destructively sorts list according to the test function, using an insertion sort, and returns the sorted list.
*/
object *fn_sort (object *args, object *env) {
  if (first(args) == NULL) return nil;
  object *list = cons(nil,first(args));
  protect(list);
  object *predicate = second(args);
  object *compare = cons(NULL, cons(NULL, NULL));
  protect(compare);
  object *ptr = cdr(list);
  while (cdr(ptr) != NULL) {
    object *go = list;
    while (go != ptr) {
      car(compare) = car(cdr(ptr));
      car(cdr(compare)) = car(cdr(go));
      if (apply(predicate, compare, env)) break;
      go = cdr(go);
    }
    if (go != ptr) {
      object *obj = cdr(ptr);
      cdr(ptr) = cdr(obj);
      cdr(obj) = cdr(go);
      cdr(go) = obj;
    } else ptr = cdr(ptr);
  }
  unprotect(); unprotect();
  return cdr(list);
}"#)

      (STRINGFN "string" 1 1 #"
/*
  (string item)
  Converts its argument to a string.
*/
object *fn_stringfn (object *args, object *env) {
  return fn_princtostring(args, env);
}"#)

    (CONCATENATE nil 1 127 #"
/*
  (concatenate 'string string*)
  Joins together the strings given in the second and subsequent arguments, and returns a single string.
*/
object *fn_concatenate (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (builtin(arg->name) != STRINGFN) error2(PSTR("only supports strings"));
  args = cdr(args);
  object *result = newstring();
  object *tail = result;
  while (args != NULL) {
    object *obj = checkstring(first(args));
    obj = cdr(obj);
    while (obj != NULL) {
      int quad = obj->chars;
      while (quad != 0) {
         char ch = quad>>((sizeof(int)-1)*8) & 0xFF;
         buildstring(ch, &tail);
         quad = quad<<8;
      }
      obj = car(obj);
    }
    args = cdr(args);
  }
  return result;
}"#)

    (SUBSEQ nil 2 3 #"
/*
  (subseq seq start [end])
  Returns a subsequence of a list or string from item start to item end-1.
*/
object *fn_subseq (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  int start = checkinteger(second(args)), end;
  if (start < 0) error(indexnegative, second(args));
  args = cddr(args);
  if (listp(arg)) {
    int length = listlength(arg);
    if (args != NULL) end = checkinteger(car(args)); else end = length;
    if (start > end || end > length) error2(indexrange);
    object *result = cons(NULL, NULL);
    object *ptr = result;
    for (int x = 0; x < end; x++) {
      if (x >= start) { cdr(ptr) = cons(car(arg), NULL); ptr = cdr(ptr); }
      arg = cdr(arg);
    }
    return cdr(result);
  } else if (stringp(arg)) {
    int length = stringlength(arg);
    if (args != NULL) end = checkinteger(car(args)); else end = length;
    if (start > end || end > length) error2(indexrange);
    object *result = newstring();
    object *tail = result;
    for (int i=start; i<end; i++) {
      char ch = nthchar(arg, i);
      buildstring(ch, &tail);
    }
    return result;
  } else error2(PSTR("argument is not a list or string"));
  return nil;
}"#)

    #-avr-nano
    (SEARCH nil 2 4 #"
/*
  (search pattern target [:test function])
  Returns the index of the first occurrence of pattern in target, or nil if it's not found.
  The target can be a list or string. If it's a list a test function can be specified; default eq.
*/
object *fn_search (object *args, object *env) {
  (void) env;
  object *pattern = first(args);
  object *target = second(args);
  if (pattern == NULL) return number(0);
  else if (target == NULL) return nil;
  
  else if (listp(pattern) && listp(target)) {
    object *test = testargument(cddr(args));
    int l = listlength(target);
    int m = listlength(pattern);
    for (int i = 0; i <= l-m; i++) {
      object *target1 = target;
      while (pattern != NULL && apply(test, cons(car(target1), cons(car(pattern), NULL)), env) != NULL) {
        pattern = cdr(pattern);
        target1 = cdr(target1);
      }
      if (pattern == NULL) return number(i);
      pattern = first(args); target = cdr(target);
    }
    return nil;

  } else if (stringp(pattern) && stringp(target)) {
    if (cddr(args) != NULL) error2(PSTR("keyword argument not supported for strings"));
    int l = stringlength(target);
    int m = stringlength(pattern);
    for (int i = 0; i <= l-m; i++) {
      int j = 0;
      while (j < m && nthchar(target, i+j) == nthchar(pattern, j)) j++;
      if (j == m) return number(i);
    }
    return nil;
  } else error2(PSTR("arguments are not both lists or strings"));
  return nil;
}"#)

    (READFROMSTRING "read-from-string" 1 1 #"
/*
  (read-from-string string)
  Reads an atom or list from the specified string and returns it.
*/
object *fn_readfromstring (object *args, object *env) {
  (void) env;
  object *arg = checkstring(first(args));
  GlobalString = arg;
  GlobalStringIndex = 0;
  object *val = read(gstr);
  LastChar = 0;
  return val;
}"#)

    (PRINCTOSTRING "princ-to-string" 1 1 #"
/*
  (princ-to-string item)
  Prints its argument to a string, and returns the string.
  Characters and strings are printed without quotation marks or escape characters.
*/
object *fn_princtostring (object *args, object *env) {
  (void) env;
  return princtostring(first(args));
}"#)

    (PRIN1TOSTRING "prin1-to-string" 1 1 #"
/*
  (prin1-to-string item [stream])
  Prints its argument to a string, and returns the string.
  Characters and strings are printed with quotation marks and escape characters,
  in a format that will be suitable for read-from-string.
*/
object *fn_prin1tostring (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  object *obj = startstring();
  printobject(arg, pstr);
  return obj;
}"#)))

    ("Bitwise operators"

     ((LOGAND nil 0 127 bitwise)
      (LOGIOR nil 0 127 bitwise)
      (LOGXOR nil 0 127 bitwise)
      
      (LOGNOT nil 1 1 "
/*
  (lognot value)
  Returns the bitwise logical NOT of the value.
*/
object *fn_lognot (object *args, object *env) {
  (void) env;
  int result = checkinteger(car(args));
  return number(~result);
}")

      (ASH nil 2 2 "
/*
  (ash value shift)
  Returns the result of bitwise shifting value by shift bits. If shift is positive, value is shifted to the left.
*/
object *fn_ash (object *args, object *env) {
  (void) env;
  int value = checkinteger(first(args));
  int count = checkinteger(second(args));
  if (count >= 0) return number(value << count);
  else return number(value >> abs(count));
}")

      (LOGBITP nil 2 2 "
/*
  (logbitp bit value)
  Returns t if bit number bit in value is a '1', and nil if it is a '0'.
*/
object *fn_logbitp (object *args, object *env) {
  (void) env;
  int index = checkinteger(first(args));
  int value = checkinteger(second(args));
  return (bitRead(value, index) == 1) ? tee : nil;
}")))

    ("System functions"
     ((EVAL nil 1 1 "
/*
  (eval form*)
  Evaluates its argument an extra time.
*/
object *fn_eval (object *args, object *env) {
  return eval(first(args), env);
}")

      (RETURN nil 0 1 "
/*
  (return [value])
  Exits from a (dotimes ...), (dolist ...), or (loop ...) loop construct and returns value.
*/
object *fn_return (object *args, object *env) {
  (void) env;
  setflag(RETURNFLAG);
  if (args == NULL) return nil; else return first(args);
}")

      (GLOBALS nil 0 0 "
/*
  (globals)
  Returns a list of global variables.
*/
object *fn_globals (object *args, object *env) {
  (void) args, (void) env;
  object *result = cons(NULL, NULL);
  object *ptr = result;
  object *arg = GlobalEnv;
  while (arg != NULL) {
    cdr(ptr) = cons(car(car(arg)), NULL); ptr = cdr(ptr);
    arg = cdr(arg);
  }
  return cdr(result);
}")

      (LOCALS nil 0 0 "
/*
  (locals)
  Returns an association list of local variables and their values.
*/
object *fn_locals (object *args, object *env) {
  (void) args;
  return env;
}")

      (MAKUNBOUND nil 1 1 #"
/*
  (makunbound symbol)
  Removes the value of the symbol from GlobalEnv and returns the symbol.
*/
object *fn_makunbound (object *args, object *env) {
  (void) env;
  object *var = first(args);
  if (!symbolp(var)) error(notasymbol, var);
  delassoc(var, &GlobalEnv);
  return var;
}"#)

      (BREAK nil 0 0 #"
/*
  (break)
  Inserts a breakpoint in the program. When evaluated prints Break! and reenters the REPL.
*/
object *fn_break (object *args, object *env) {
  (void) args;
  pfstring(PSTR("\nBreak!\n"), pserial);
  BreakLevel++;
  repl(env);
  BreakLevel--;
  return nil;
}"#)

      (READ nil 0 1 "
/*
  (read [stream])
  Reads an atom or list from the serial input and returns it.
  If stream is specified the item is read from the specified stream.
*/
object *fn_read (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  return read(gfun);
}")

      (PRIN1 nil 1 2 "
/*
  (prin1 item [stream]) 
  Prints its argument, and returns its value.
  Strings are printed with quotation marks and escape characters.
*/
object *fn_prin1 (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  printobject(obj, pfun);
  return obj;
}")

      (PRINT nil 1 2 "
/*
  (print item [stream])
  Prints its argument with quotation marks and escape characters, on a new line, and followed by a space.
  If stream is specified the argument is printed to the specified stream.
*/
object *fn_print (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  pln(pfun);
  printobject(obj, pfun);
  pfun(' ');
  return obj;
}")

      (PRINC nil 1 2 "
/*
  (princ item [stream]) 
  Prints its argument, and returns its value.
  Characters and strings are printed without quotation marks or escape characters.
*/
object *fn_princ (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  prin1object(obj, pfun);
  return obj;
}")

      (TERPRI nil 0 1 "
/*
  (terpri [stream])
  Prints a new line, and returns nil.
  If stream is specified the new line is written to the specified stream. 
*/
object *fn_terpri (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  pln(pfun);
  return nil;
}")

    (READBYTE "read-byte" 0 2 #"
/*
  (read-byte stream)
  Reads a byte from a stream and returns it.
*/
object *fn_readbyte (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  int c = gfun();
  return (c == -1) ? nil : number(c);
}"#)

    (READLINE "read-line" 0 1 #"
/*
  (read-line [stream])
  Reads characters from the serial input up to a newline character, and returns them as a string, excluding the newline.
  If stream is specified the line is read from the specified stream.
*/
object *fn_readline (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  return readstring('\n', false, gfun);
}"#)

    (WRITEBYTE "write-byte" 1 2 #"
/*
  (write-byte number [stream])
  Writes a byte to a stream.
*/
object *fn_writebyte (object *args, object *env) {
  (void) env;
  int value = checkinteger(first(args));
  pfun_t pfun = pstreamfun(cdr(args));
  (pfun)(value);
  return nil;
}"#)

    (WRITESTRING "write-string" 1 2 #"
/*
  (write-string string [stream])
  Writes a string. If stream is specified the string is written to the stream.
*/
object *fn_writestring (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  flags_t temp = Flags;
  clrflag(PRINTREADABLY);
  printstring(obj, pfun);
  Flags = temp;
  return nil;
}"#)

    (WRITELINE "write-line" 1 2 #"
/*
  (write-line string [stream])
  Writes a string terminated by a newline character. If stream is specified the string is written to the stream.
*/
object *fn_writeline (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  flags_t temp = Flags;
  clrflag(PRINTREADABLY);
  printstring(obj, pfun);
  pln(pfun);
  Flags = temp;
  return nil;
}"#)

      #+(or arm esp)
      (RESTARTI2C "restart-i2c" 1 2 #"
/*
  (restart-i2c stream [read-p])
  Restarts an i2c-stream.
  If read-p is nil or omitted the stream is written to.
  If read-p is an integer it specifies the number of bytes to be read from the stream.
*/
object *fn_restarti2c (object *args, object *env) {
  (void) env;
  int stream = isstream(first(args));
  args = cdr(args);
  int read = 0; // Write
  I2Ccount = 0;
  if (args != NULL) {
    object *rw = first(args);
    if (integerp(rw)) I2Ccount = rw->integer;
    read = (rw != NULL);
  }
  int address = stream & 0xFF;
  if (stream>>8 != I2CSTREAM) error2(PSTR("not an i2c stream"));
  TwoWire *port;
  if (address < 128) port = &Wire;
  #if defined(ULISP_I2C1)
  else port = &Wire1;
  #endif
  return I2Crestart(port, address & 0x7F, read) ? tee : nil;
}"#)

      #-(or arm esp)
      (RESTARTI2C "restart-i2c" 1 2 #"
/*
  (restart-i2c stream [read-p])
  Restarts an i2c-stream.
  If read-p is nil or omitted the stream is written to.
  If read-p is an integer it specifies the number of bytes to be read from the stream.
*/
object *fn_restarti2c (object *args, object *env) {
  (void) env;
  int stream = isstream(first(args));
  args = cdr(args);
  int read = 0; // Write
  I2Ccount = 0;
  if (args != NULL) {
    object *rw = first(args);
    if (integerp(rw)) I2Ccount = rw->integer;
    read = (rw != NULL);
  }
  int address = stream & 0xFF;
  if (stream>>8 != I2CSTREAM) error2(PSTR("not an i2c stream"));
  return I2Crestart(address, read) ? tee : nil;
}"#)

      #+(or avr avr-nano)
      (GC nil 0 1 #"
/*
  (gc [print time])
  Forces a garbage collection and prints the number of objects collected, and the time taken.
*/
object *fn_gc (object *args, object *env) {
  if (args == NULL || first(args) != NULL) {
    int initial = Freespace;
    unsigned long start = micros();
    gc(args, env);
    unsigned long elapsed = micros() - start;
    pfstring(PSTR("Space: "), pserial);
    pint(Freespace - initial, pserial);
    pfstring(PSTR(" bytes, Time: "), pserial);
    pint(elapsed, pserial);
    pfstring(PSTR(" us\n"), pserial);
  } else gc(args, env);
  return nil;
}"#)

      #-(or avr avr-nano)
      (GC nil 0 1 #"
/*
  (gc [print time])
  Forces a garbage collection and prints the number of objects collected, and the time taken.
*/
object *fn_gc (object *args, object *env) {
  if (args == NULL || first(args) != NULL) {
    int initial = Freespace;
    unsigned long start = micros();
    gc(args, env);
    unsigned long elapsed = micros() - start;
    pfstring("Space: ", pserial);
    pint(Freespace - initial, pserial);
    pfstring(" bytes, Time: ", pserial);
    pint(elapsed, pserial);
    pfstring(" us\n", pserial);
  } else gc(args, env);
  return nil;
}"#)

      (ROOM nil 0 0 #"
/*
  (room)
  Returns the number of free Lisp cells remaining.
*/
object *fn_room (object *args, object *env) {
  (void) args, (void) env;
  return number(Freespace);
}"#)

      #-avr-nano
      (BACKTRACE nil 0 1 #"
/*
  (backtrace [on])
  Sets the state of backtrace according to the boolean flag 'on',
  or with no argument displays the current state of backtrace.
*/
object *fn_backtrace (object *args, object *env) {
  (void) env;
  if (args == NULL) return (tstflag(BACKTRACE)) ? tee : nil;
  if (first(args) == NULL) clrflag(BACKTRACE); else setflag(BACKTRACE);
  return first(args);
}"#)

      (SAVEIMAGE "save-image" 0 1 "
/*
  (save-image [symbol])
  Saves the current uLisp image to non-volatile memory or SD card so it can be loaded using load-image.
*/
object *fn_saveimage (object *args, object *env) {
  if (args != NULL) args = eval(first(args), env);
  return number(saveimage(args));
}")

      (LOADIMAGE "load-image" 0 1 "
/*
  (load-image [filename])
  Loads a saved uLisp image from non-volatile memory or SD card.
*/
object *fn_loadimage (object *args, object *env) {
  (void) env;
  if (args != NULL) args = first(args);
  return number(loadimage(args));
}")

      #+ignore
      (DUMPIMAGE "dump-image" 0 0 #"
object *fn_dumpimage(object *args, object *env) {
  (void) args, (void) env;
  int imagesize = workspacesize; // compactimage(NULL);
  char tmp[16];
  Serial.println(); 
  sprintf(tmp, "freelist: %04x, ", (int)freelist);
  Serial.print(tmp);
  sprintf(tmp, "GlobalEnv: %04x, ", (int)GlobalEnv);
  Serial.print(tmp);
  sprintf(tmp, "GCStack: %04x, ", (int)GCStack);
  Serial.print(tmp);
      
  for (int i=0; i<imagesize; i++) {
    if (i%16 == 0) {
      Serial.println(); 
      sprintf(tmp, "%04x: ", (int)&workspace[i]);
      Serial.print(tmp);
    }
    sprintf(tmp, "%04x.%04x ", (unsigned int)car(&workspace[i]) , (unsigned int)cdr(&workspace[i]));
    Serial.print(tmp);
  }
  Serial.println();
  return nil;
}"#)

      (CLS "cls" 0 0 "
/*
  (cls)
  Prints a clear-screen character.
*/
object *fn_cls (object *args, object *env) {
  (void) args, (void) env;
  pserial(12);
  return nil;
}")))

    ("Arduino procedures"
     (

      #+ignore
      (WATCHDOG nil 0 1 "
object *fn_watchdog (object *args, object *env) {
  (void) env;
  if (args == NULL) watchdogreset();
  else watchdogenable(integer(first(args)));
  return nil;
}")

      #+(or avr avr-nano arm esp riscv)
      (PINMODE nil 2 2 "
/*
  (pinmode pin mode)
  Sets the input/output mode of an Arduino pin number, and returns nil.
  The mode parameter can be an integer, a keyword, or t or nil.
*/
object *fn_pinmode (object *args, object *env) {
  (void) env; int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(arg);
  else pin = checkinteger(first(args));
  int pm = INPUT;
  arg = second(args);
  if (keywordp(arg)) pm = checkkeyword(arg);
  else if (integerp(arg)) {
    int mode = arg->integer;
    if (mode == 1) pm = OUTPUT; else if (mode == 2) pm = INPUT_PULLUP;
    #if defined(INPUT_PULLDOWN)
    else if (mode == 4) pm = INPUT_PULLDOWN;
    #endif
  } else if (arg != nil) pm = OUTPUT;
  pinMode(pin, pm);
  return nil;
}")

      #+stm32
      (PINMODE nil 2 2 "
/*
  (pinmode pin mode)
  Sets the input/output mode of an Arduino pin number, and returns nil.
  The mode parameter can be an integer, a keyword, or t or nil.
*/
object *fn_pinmode (object *args, object *env) {
  (void) env;
  int pin = checkinteger(first(args));
  int pm = INPUT;
  object *mode = second(args);
  if (integerp(mode)) {
    int nmode = checkinteger(mode);
    if (nmode == 1) pm = OUTPUT; else if (nmode == 2) pm = INPUT_PULLUP;
    #if defined(INPUT_PULLDOWN)
    else if (nmode == 4) pm = INPUT_PULLDOWN;
    #endif
  } else if (mode != nil) pm = OUTPUT;
  pinMode(pin, (WiringPinMode)pm);
  return nil;
}")

      (DIGITALREAD nil 1 1 "
/*
  (digitalread pin)
  Reads the state of the specified Arduino pin number and returns t (high) or nil (low).
*/
object *fn_digitalread (object *args, object *env) {
  (void) env;
  int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(arg);
  else pin = checkinteger(arg);
  if (digitalRead(pin) != 0) return tee; else return nil;
}")

      (DIGITALWRITE nil 2 2 "
/*
  (digitalwrite pin state)
  Sets the state of the specified Arduino pin number.
*/
object *fn_digitalwrite (object *args, object *env) {
  (void) env;
  int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(arg);
  else pin = checkinteger(arg);
  arg = second(args);
  int mode;
  if (keywordp(arg)) mode = checkkeyword(arg);
  else if (integerp(arg)) mode = arg->integer ? HIGH : LOW;
  else mode = (arg != nil) ? HIGH : LOW;
  digitalWrite(pin, mode);
  return arg;
}")

      (ANALOGREAD nil 1 1 #"
/*
  (analogread pin)
  Reads the specified Arduino analogue pin number and returns the value.
*/
object *fn_analogread (object *args, object *env) {
  (void) env;
  int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(arg);
  else {
    pin = checkinteger(arg);
    checkanalogread(pin);
  }
  return number(analogRead(pin));
}"#)

      #+(or avr avr-nano)
      (ANALOGREFERENCE nil 1 1 #"
/*
  (analogreference keyword)
  Specifies a keyword to set the analogue reference voltage used for analogue input. 
*/
object *fn_analogreference (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  analogReference(checkkeyword(arg));
  return arg;
}"#)

      #+arm
      (ANALOGREFERENCE nil 1 1 #"
/*
  (analogreference keyword)
  Specifies a keyword to set the analogue reference voltage used for analogue input. 
*/
object *fn_analogreference (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41) || defined(MAX32620) \
   || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) \
   || defined(ARDUINO_RASPBERRY_PI_PICO_2) || defined(ARDUINO_PIMORONI_PICO_PLUS_2) \
   || defined(ARDUINO_PIMORONI_TINY2350) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) \
   || defined(ARDUINO_ADAFRUIT_QTPY_RP2040) || defined(ARDUINO_NANO_MATTER) \
   || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040_ADALOGGER)
  error2("not supported");
  #else
  analogReference((eAnalogReference)checkkeyword(arg));
  #endif
  return arg;
}"#)

      #+(or avr avr-nano)
      (ANALOGREADRESOLUTION nil 1 1 #"
/*
  (analogreadresolution bits)
  Specifies the resolution for the analogue inputs on platforms that support it.
  The default resolution on all platforms is 10 bits.
*/
object *fn_analogreadresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(CPU_AVR128DX48)
  uint8_t res = checkinteger(arg);
  if (res == 10) analogReadResolution(10);
  else if (res == 12) analogReadResolution(12);
  else error(PSTR("invalid resolution"), arg);
  #else
  error2(PSTR("not supported"));
  #endif
  return arg;
}"#)

      #+arm
      (ANALOGREADRESOLUTION nil 1 1 #"
/*
  (analogreadresolution bits)
  Specifies the resolution for the analogue inputs on platforms that support it.
  The default resolution on all platforms is 10 bits.
*/
object *fn_analogreadresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_2) \
  || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040) \
  || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040_ADALOGGER)
  error2("not supported");
  #else
  analogReadResolution(checkinteger(arg));
  #endif
  return arg;
}"#)

      #+esp
      (ANALOGREADRESOLUTION nil 1 1 #"
/*
  (analogreadresolution bits)
  Specifies the resolution for the analogue inputs on platforms that support it.
  The default resolution on all platforms is 10 bits.
*/
object *fn_analogreadresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(ESP32)
  analogReadResolution(checkinteger(arg));
  #else
  error2(PSTR("not supported"));
  #endif
  return arg;
}"#)

      #+riscv
      (ANALOGREADRESOLUTION nil 1 1 #"
/*
  (analogreadresolution bits)
  Specifies the resolution for the analogue inputs on platforms that support it.
  The default resolution on all platforms is 10 bits.
*/
object *fn_analogreadresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  analogReadResolution(checkinteger(arg));
  return arg;
}"#)

      (ANALOGWRITE nil 2 2 #"
/*
  (analogwrite pin value)
  Writes the value to the specified Arduino pin number.
*/
object *fn_analogwrite (object *args, object *env) {
  (void) env;
  int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(arg);
  else pin = checkinteger(arg);
  checkanalogwrite(pin);
  object *value = second(args);
  analogWrite(pin, checkinteger(value));
  return value;
}"#)

      #+(or arm riscv)
      (ANALOGWRITERESOLUTION nil 1 1 #"
/*
  (analogwrite pin value)
  Sets the analogue write resolution.
*/
object *fn_analogwriteresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  analogWriteResolution(checkinteger(arg));
  return arg;
}"#)

      #+(or avr avr-nano)
      (DACREFERENCE nil 1 1 #"
/*
  (dacreference value)
  Sets the DAC voltage reference. AVR128DX48 only.
*/
object *fn_dacreference (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(CPU_AVR128DX48)
  int ref = checkinteger(arg);
  DACReference(ref);
  #endif
  return arg;
}"#)

      (DELAY nil 1 1 "
/*
  (delay number)
  Delays for a specified number of milliseconds.
*/
object *fn_delay (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  unsigned long start = millis();
  unsigned long total = checkinteger(arg1);
  do testescape();
  while (millis() - start < total);
  return arg1;
}")

      (MILLIS nil 0 0 #"
/*
  (millis)
  Returns the time in milliseconds that uLisp has been running.
*/
object *fn_millis (object *args, object *env) {
  (void) args, (void) env;
  return number(millis());
}"#)

      #+(or avr avr-nano)
      (SLEEP nil 0 1 #"
/*
  (sleep secs)
  Puts the processor into a low-power sleep mode for secs.
  Only supported on some platforms. On other platforms it does delay(1000*secs).
*/
object *fn_sleep (object *args, object *env) {
  (void) env;
  if (args == NULL || first(args) == NULL) { sleep(); return nil; }
  object *arg1 = first(args);
  doze(checkinteger(arg1));
  return arg1;
}"#)

     #-(or avr avr-nano)
     (SLEEP nil 0 1 #"
/*
  (sleep secs)
  Puts the processor into a low-power sleep mode for secs.
  Only supported on some platforms. On other platforms it does delay(1000*secs).
*/
object *fn_sleep (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  doze(checkinteger(arg1));
  return arg1;
}"#)

     #+ignore
      (SHIFTOUT nil 4 4 "
object *fn_shiftout (object *args, object *env) {
  (void) env;
  int datapin = integer(first(args));
  int clockpin = integer(second(args));
  int order = (third(args) != nil);
  object *value = fourth(args);
  shiftOut(datapin, clockpin, order, integer(value));
  return value;
}")

      #+ignore
      (SHIFTIN nil 3 3 "
object *fn_shiftin (object *args, object *env) {
  (void) env;
  int datapin = integer(first(args));
  int clockpin = integer(second(args));
  int order = (third(args) != nil);
  int value = shiftIn(datapin, clockpin, order);
  return number(value);
}")

      (NOTE nil 0 3 #"
/*
  (note [pin] [note] [octave])
  Generates a square wave on pin.
  note represents the note in the well-tempered scale.
  The argument octave can specify an octave; default 0.
*/
object *fn_note (object *args, object *env) {
  (void) env;
  static int pin = 255;
  if (args != NULL) {
    pin = checkinteger(first(args));
    int note = 48, octave = 0;
    if (cdr(args) != NULL) {
      note = checkinteger(second(args));
      if (cddr(args) != NULL) octave = checkinteger(third(args));
    }
    playnote(pin, note, octave);
  } else nonote(pin);
  return nil;
}"#)
      
      #+(or avr avr-nano)
      (REGISTER nil 1 2 #"
/*
  (register address [value])
  Reads or writes the value of a peripheral register.
  If value is not specified the function returns the value of the register at address.
  If value is specified the value is written to the register at address and the function returns value.
*/
object *fn_register (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  int addr;
  if (keywordp(arg)) addr = checkkeyword(arg);
  else addr = checkinteger(first(args));
  if (cdr(args) == NULL) return number(*(volatile uint8_t *)addr);
  (*(volatile uint8_t *)addr) = checkinteger(second(args));
  return second(args);
}"#)

      #+(or arm esp riscv)
      (REGISTER nil 1 2 #"
/*
  (register address [value])
  Reads or writes the value of a peripheral register.
  If value is not specified the function returns the value of the register at address.
  If value is specified the value is written to the register at address and the function returns value.
*/
object *fn_register (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  int addr;
  if (keywordp(arg)) addr = checkkeyword(arg);
  else addr = checkinteger(first(args));
  if (cdr(args) == NULL) return number(*(uint32_t *)addr);
  (*(uint32_t *)addr) = checkinteger(second(args));
  return second(args);
}"#)

      #+interrupts
      (ATTACHINTERRUPT "attach-interrupt" 1 3 #"
object *fn_attachinterrupt (object *args, object *env) {
  (void) env;
  object *number = first(args);
  if (number == NULL) {
    int n = NINTERRUPTS;
    args = cdr(args);
    delassoc(number,&Events);
    push(cons(number,first(args)),Events);
    InterruptCount[n] = 0;
    TCCR1A = 0;                    // CTC mode
    TCCR1B = 1<<WGM12 | 5<<CS10;   // Prescaler 1024
    OCR1A = 15624;                 // 1 sec
    TIMSK1 = 1<<TOIE1;             // OVF interrupt
  } else {
    int n = integer(number);
    if (n<0 || n>=NINTERRUPTS-1) error3(ATTACHINTERRUPT, PSTR("invalid interrupt"));
    args = cdr(args);
    delassoc(number,&Events);
    if (args == NULL || first(args) == NULL) {
      EIMSK &= ~(1<<n);
      return nil;
    }
    push(cons(number,first(args)),Events);
    InterruptCount[n] = 0;
    int mode = 3;
    args = cdr(args);
    if (args != NULL) mode = integer(first(args));
    if (mode<0 || mode>3) error3(ATTACHINTERRUPT, PSTR("invalid mode"));
    EIMSK |= 1<<n;
    n = n<<1;
    if (n <= 6) EICRA = (EICRA & ~(3<<n)) | mode<<n;
    #if NINTERRUPTS > 4
    else { n = n & 0x03; EICRB = (EICRB & ~(3<<n)) | mode<<n; }
    #endif
  }
  return nil;
}"#)))


    ("Tree Editor"

     ((EDIT nil 1 1 #"
/*
  (edit 'function)
  Calls the Lisp tree editor to allow you to edit a function definition.
*/
object *fn_edit (object *args, object *env) {
  object *fun = first(args);
  object *pair = findvalue(fun, env);
  clrflag(EXITEDITOR);
  object *arg = edit(eval(fun, env));
  cdr(pair) = arg;
  return arg;
}"#)))

    ("Pretty printer"
     
     (

      #-gfx
      (PPRINT nil 1 2 #"
/*
  (pprint item [str])
  Prints its argument, using the pretty printer, to display it formatted in a structured way.
  If str is specified it prints to the specified stream. It returns no value.
*/
object *fn_pprint (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  pln(pfun);
  superprint(obj, 0, pfun);
  return bsymbol(NOTHING);
}"#)

      #+gfx
      (PPRINT nil 1 2 #"
/*
  (pprint item [str])
  Prints its argument, using the pretty printer, to display it formatted in a structured way.
  If str is specified it prints to the specified stream. It returns no value.
*/
object *fn_pprint (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  #if defined(gfxsupport)
  if (pfun == gfxwrite) ppwidth = GFXPPWIDTH;
  #endif
  pln(pfun);
  superprint(obj, 0, pfun);
  ppwidth = PPWIDTH;
  return bsymbol(NOTHING);
}"#)

      #+(or avr avr-nano)
    (PPRINTALL nil 0 1 #"
/*
  (pprintall [str])
  Pretty-prints the definition of every function and variable defined in the uLisp workspace.
  If str is specified it prints to the specified stream. It returns no value.
*/
object *fn_pprintall (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pfun);
    if (consp(val) && symbolp(car(val)) && builtin(car(val)->name) == LAMBDA) {
      superprint(cons(bsymbol(DEFUN), cons(var, cdr(val))), 0, pfun);
    #if defined(CODESIZE)
    } else if (consp(val) && car(val)->type == CODE) {
      superprint(cons(bsymbol(DEFCODE), cons(var, cdr(val))), 0, pfun);
    #endif
    } else {
      superprint(cons(bsymbol(DEFVAR), cons(var, cons(quote(val), NULL))), 0, pfun);
    }
    pln(pfun);
    testescape();
    globals = cdr(globals);
  }
  return bsymbol(NOTHING);
}"#)

    #+ignore
    (PPRINTALL nil 0 1 #"
/*
  (pprintall [str])
  Pretty-prints the definition of every function and variable defined in the uLisp workspace.
  If str is specified it prints to the specified stream. It returns no value.
*/
object *fn_pprintall (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pfun);
    if (consp(val) && symbolp(car(val)) && car(val)->name == LAMBDA) {
      superprint(cons(symbol(DEFUN), cons(var, cdr(val))), 0, pfun);
    } else if (consp(val) && car(val)->type == CODE) {
      superprint(cons(symbol(DEFCODE), cons(var, cdr(val))), 0, pfun);
    } else {
      superprint(cons(symbol(DEFVAR), cons(var, cons(quote(val), NULL))), 0, pfun);
    }
    pln(pfun);
    testescape();
    globals = cdr(globals);
  }
  return symbol(NOTHING);
}"#)

    #+esp
    (PPRINTALL nil 0 1 #"
/*
  (pprintall [str])
  Pretty-prints the definition of every function and variable defined in the uLisp workspace.
  If str is specified it prints to the specified stream. It returns no value.
*/
object *fn_pprintall (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  #if defined(gfxsupport)
  if (pfun == gfxwrite) ppwidth = GFXPPWIDTH;
  #endif
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pfun);
    if (consp(val) && symbolp(car(val)) && builtin(car(val)->name) == LAMBDA) {
      superprint(cons(bsymbol(DEFUN), cons(var, cdr(val))), 0, pfun);
    } else {
      superprint(cons(bsymbol(DEFVAR), cons(var, cons(quote(val), NULL))), 0, pfun);
    }
    pln(pfun);
    testescape();
    globals = cdr(globals);
  }
  ppwidth = PPWIDTH;
  return bsymbol(NOTHING);
}"#)

    #+(or riscv arm)
    (PPRINTALL nil 0 1 #"
/*
  (pprintall [str])
  Pretty-prints the definition of every function and variable defined in the uLisp workspace.
  If str is specified it prints to the specified stream. It returns no value.
*/
object *fn_pprintall (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  #if defined(gfxsupport)
  if (pfun == gfxwrite) ppwidth = GFXPPWIDTH;
  #endif
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pfun);
    if (consp(val) && symbolp(car(val)) && builtin(car(val)->name) == LAMBDA) {
      superprint(cons(bsymbol(DEFUN), cons(var, cdr(val))), 0, pfun);
    } else if (consp(val) && car(val)->type == CODE) {
      superprint(cons(bsymbol(DEFCODE), cons(var, cdr(val))), 0, pfun);
    } else {
      superprint(cons(bsymbol(DEFVAR), cons(var, cons(quote(val), NULL))), 0, pfun);
    }
    pln(pfun);
    testescape();
    globals = cdr(globals);
  }
  ppwidth = PPWIDTH;
  return bsymbol(NOTHING);
}"#)))

    ("Format"

     ((FORMAT nil 2 127 #"
/*
  (format output controlstring [arguments]*)
  Outputs its arguments formatted according to the format directives in controlstring.
*/
object *fn_format (object *args, object *env) {
  (void) env;
  pfun_t pfun = pserial;
  object *output = first(args);
  object *obj;
  if (output == nil) { obj = startstring(); pfun = pstr; }
  else if (!eq(output, tee)) pfun = pstreamfun(args);
  object *formatstr = checkstring(second(args));
  object *save = NULL;
  args = cddr(args);
  int len = stringlength(formatstr);
  uint8_t n = 0, width = 0, w, bra = 0;
  char pad = ' ';
  bool tilde = false, mute = false, comma = false, quote = false;
  while (n < len) {
    char ch = nthchar(formatstr, n);
    char ch2 = ch & ~0x20; // force to upper case
    if (tilde) {
     if (ch == '}') {
        if (save == NULL) formaterr(formatstr, PSTR("no matching ~{"), n);
        if (args == NULL) { args = cdr(save); save = NULL; } else n = bra;
        mute = false; tilde = false;
      }
      else if (!mute) {
        if (comma && quote) { pad = ch; comma = false, quote = false; }
        else if (ch == '\'') {
          if (comma) quote = true;
          else formaterr(formatstr, PSTR("quote not valid"), n);
        }
        else if (ch == '~') { pfun('~'); tilde = false; }
        else if (ch >= '0' && ch <= '9') width = width*10 + ch - '0';
        else if (ch == ',') comma = true;
        else if (ch == '%') { pln(pfun); tilde = false; }
        else if (ch == '&') { pfl(pfun); tilde = false; }
        else if (ch == '^') {
          if (save != NULL && args == NULL) mute = true;
          tilde = false;
        }
        else if (ch == '{') {
          if (save != NULL) formaterr(formatstr, PSTR("can't nest ~{"), n);
          if (args == NULL) formaterr(formatstr, noargument, n);
          if (!listp(first(args))) formaterr(formatstr, notalist, n);
          save = args; args = first(args); bra = n; tilde = false;
          if (args == NULL) mute = true;
        }
        else if (ch2 == 'A' || ch2 == 'S' || ch2 == 'D' || ch2 == 'G' || ch2 == 'X' || ch2 == 'B') {
          if (args == NULL) formaterr(formatstr, noargument, n);
          object *arg = first(args); args = cdr(args);
          uint8_t aw = atomwidth(arg);
          if (width < aw) w = 0; else w = width-aw;
          tilde = false;
          if (ch2 == 'A') { prin1object(arg, pfun); indent(w, pad, pfun); }
          else if (ch2 == 'S') { printobject(arg, pfun); indent(w, pad, pfun); }
          else if (ch2 == 'D' || ch2 == 'G') { indent(w, pad, pfun); prin1object(arg, pfun); }
          else if (ch2 == 'X' || ch2 == 'B') {
            if (integerp(arg)) {
              uint8_t base = (ch2 == 'B') ? 2 : 16;
              uint8_t hw = basewidth(arg, base); if (width < hw) w = 0; else w = width-hw;
              indent(w, pad, pfun); pintbase(arg->integer, base, pfun);
            } else {
              indent(w, pad, pfun); prin1object(arg, pfun);
            }
          }
          tilde = false;
        } else formaterr(formatstr, PSTR("invalid directive"), n);
      }
    } else {
      if (ch == '~') { tilde = true; pad = ' '; width = 0; comma = false; quote = false; }
      else if (!mute) pfun(ch);
    }
    n++;
  }
  if (output == nil) return obj;
  else return nil;
}"#)))


("LispLibrary"
     
     (
     (REQUIRE nil 1 1 #"
/*
  (require 'symbol)
  Loads the definition of a function defined with defun, or a variable defined with defvar, from the Lisp Library.
  It returns t if it was loaded, or nil if the symbol is already defined or isn't defined in the Lisp Library.
*/
object *fn_require (object *args, object *env) {
  object *arg = first(args);
  object *globals = GlobalEnv;
  if (!symbolp(arg)) error(notasymbol, arg);
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    if (symbolp(var) && var == arg) return nil;
    globals = cdr(globals);
  }
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    // Is this the definition we want
    symbol_t fname = first(line)->name;
    if ((fname == sym(DEFUN) || fname == sym(DEFVAR)) && symbolp(second(line)) && second(line)->name == arg->name) {
      eval(line, env);
      return tee;
    }
    line = read(glibrary);
  }
  return nil;
}"#)

     (LISTLIBRARY "list-library" 0 0 #"
/*
  (list-library)
  Prints a list of the functions defined in the List Library.
*/
object *fn_listlibrary (object *args, object *env) {
  (void) args, (void) env;
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    builtin_t bname = builtin(first(line)->name);
    if (bname == DEFUN || bname == DEFVAR) {
      printsymbol(second(line), pserial); pserial(' ');
    }
    line = read(glibrary);
  }
  return bsymbol(NOTHING);
}"#)))

#+doc
("Documentation"

 ((HELP "?" 1 1 #"
/*
  (? item)
  Prints the documentation string of a built-in or user-defined function.
*/
object *sp_help (object *args, object *env) {
  if (args == NULL) error2(noargument);
  object *docstring = documentation(first(args), env);
  if (docstring) {
    flags_t temp = Flags;
    clrflag(PRINTREADABLY);
    printstring(docstring, pserial);
    Flags = temp;
  }
  return bsymbol(NOTHING);
}"#)) "sp")
     
    #+doc
    (nil
     ((DOCUMENTATION nil 1 2 #"
/*
  (documentation 'symbol [type])
  Returns the documentation string of a built-in or user-defined function. The type argument is ignored.
*/
object *fn_documentation (object *args, object *env) {
  return documentation(first(args), env);
}"#)

     (APROPOS nil 1 1 #"
/*
  (apropos item)
  Prints the user-defined and built-in functions whose names contain the specified string or symbol.
*/
object *fn_apropos (object *args, object *env) {
  (void) env;
  apropos(first(args), true);
  return bsymbol(NOTHING);
}"#)

     (APROPOSLIST "apropos-list" 1 1 #"
/*
  (apropos-list item)
  Returns a list of user-defined and built-in functions whose names contain the specified string or symbol.
*/
object *fn_aproposlist (object *args, object *env) {
  (void) env;
  return apropos(first(args), false);
}"#)))

#+errors
("Error handling"
     
 ((UNWINDPROTECT "unwind-protect" 0 127 #"
/*
  (unwind-protect form1 [forms]*)
  Evaluates form1 and forms in order and returns the value of form1,
  but guarantees to evaluate forms even if an error occurs in form1.
*/
object *sp_unwindprotect (object *args, object *env) {
  if (args == NULL) error2(toofewargs);
  object *current_GCStack = GCStack;
  jmp_buf dynamic_handler;
  jmp_buf *previous_handler = handler;
  handler = &dynamic_handler;
  object *protected_form = first(args);
  object *result;

  bool signaled = false;
  if (!setjmp(dynamic_handler)) {
    result = eval(protected_form, env);
  } else {
    GCStack = current_GCStack;
    signaled = true;
  }
  handler = previous_handler;

  object *protective_forms = cdr(args);
  while (protective_forms != NULL) {
    eval(car(protective_forms), env);
    if (tstflag(RETURNFLAG)) break;
    protective_forms = cdr(protective_forms);
  }

  if (!signaled) return result;
  GCStack = NULL;
  longjmp(*handler, 1);
}"#)

(IGNOREERRORS "ignore-errors" 0 127 #"
/*
  (ignore-errors [forms]*)
  Evaluates forms ignoring errors.
*/
object *sp_ignoreerrors (object *args, object *env) {
  object *current_GCStack = GCStack;
  jmp_buf dynamic_handler;
  jmp_buf *previous_handler = handler;
  handler = &dynamic_handler;
  object *result = nil;

  bool muffled = tstflag(MUFFLEERRORS);
  setflag(MUFFLEERRORS);
  bool signaled = false;
  if (!setjmp(dynamic_handler)) {
    while (args != NULL) {
      result = eval(car(args), env);
      if (tstflag(RETURNFLAG)) break;
      args = cdr(args);
    }
  } else {
    GCStack = current_GCStack;
    signaled = true;
  }
  handler = previous_handler;
  if (!muffled) clrflag(MUFFLEERRORS);

  if (signaled) return bsymbol(NOTHING);
  else return result;
}"#)

(ERROR nil 1 127 #"
/*
  (error controlstring [arguments]*)
  Signals an error. The message is printed by format using the controlstring and arguments.
*/
object *sp_error (object *args, object *env) {
  object *message = eval(cons(bsymbol(FORMAT), cons(nil, args)), env);
  if (!tstflag(MUFFLEERRORS)) {
    flags_t temp = Flags;
    clrflag(PRINTREADABLY);
    pfstring(PSTR("Error: "), pserial); printstring(message, pserial);
    Flags = temp;
    pln(pserial);
  }
  GCStack = NULL;
  longjmp(*handler, 1);
}"#)) "sp")

      #+(or arm esp riscv avr)
("SD Card utilities"
 (
      (DIRECTORY nil 0 0 #"
/*
  (directory)
  Returns a list of the filenames of the files on the SD card.
*/
object *fn_directory (object *args, object *env) {
  (void) args, (void) env;
  #if defined(sdcardsupport)
  SDBegin();
  File root = SD.open("/");
  if (!root) error2("problem reading from SD card");
  object *result = cons(NULL, NULL);
  object *ptr = result;
  while (true) {
    File entry = root.openNextFile();
    if (!entry) break;
    object *filename = lispstring((char*)entry.name());
    cdr(ptr) = cons(filename, NULL);
    ptr = cdr(ptr);
    entry.close();
  }
  root.close();
  return cdr(result);
  #else
  error2("not supported");
  return nil;
  #endif
}"#)))

#+wifi
("Wi-Fi"
      (
       #+arm
    (WITHCLIENT "with-client" 1 127 #"
/*
  (with-client (str [address port]) form*)
  Evaluates the forms with str bound to a wifi-stream.
*/
object *sp_withclient (object *args, object *env) {
  #if defined(ULISP_WIFI)
  object *params = checkarguments(args, 1, 3);
  object *var = first(params);
  char buffer[BUFFERSIZE];
  params = cdr(params);
  int n;
  if (params == NULL) {
    client = server.available();
    if (!client) return nil;
    n = 2;
  } else {
    object *address = eval(first(params), env);
    object *port = eval(second(params), env);
    int success;
    if (stringp(address)) success = client.connect(cstring(address, buffer, BUFFERSIZE), checkinteger(port));
    else if (integerp(address)) success = client.connect(address->integer, checkinteger(port));
    else error2(PSTR("invalid address"));
    if (!success) return nil;
    n = 1;
  }
  object *pair = cons(var, stream(WIFISTREAM, n));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  client.stop();
  return result;
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

#+esp
    (WITHCLIENT "with-client" 1 127 #"
/*
  (with-client (str [address port]) form*)
  Evaluates the forms with str bound to a wifi-stream.
*/
object *sp_withclient (object *args, object *env) {
  object *params = checkarguments(args, 1, 3);
  object *var = first(params);
  char buffer[BUFFERSIZE];
  params = cdr(params);
  int n;
  if (params == NULL) {
    client = server.available();
    if (!client) return nil;
    n = 2;
  } else {
    object *address = eval(first(params), env);
    object *port = eval(second(params), env);
    int success;
    if (stringp(address)) success = client.connect(cstring(address, buffer, BUFFERSIZE), checkinteger(port));
    else if (integerp(address)) success = client.connect(address->integer, checkinteger(port));
    else error2(PSTR("invalid address"));
    if (!success) return nil;
    n = 1;
  }
  object *pair = cons(var, stream(WIFISTREAM, n));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  client.stop();
  return result;
}"#)) "sp")

#+wifi
(nil
 (
  #+esp
  (AVAILABLE nil 1 1 #"
/*
  (available stream)
  Returns the number of bytes available for reading from the wifi-stream, or zero if no bytes are available.
*/
object *fn_available (object *args, object *env) {
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(PSTR("invalid stream"));
  return number(client.available());
}"#)

 #+arm
 (AVAILABLE nil 1 1 #"
/*
  (available stream)
  Returns the number of bytes available for reading from the wifi-stream, or zero if no bytes are available.
*/
object *fn_available (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(PSTR("invalid stream"));
  return number(client.available());
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

     #+esp
     (WIFISERVER "wifi-server" 0 0 #"
/*
  (wifi-server)
  Starts a Wi-Fi server running. It returns nil.
*/
object *fn_wifiserver (object *args, object *env) {
  (void) args, (void) env;
  server.begin();
  return nil;
}"#)

     #+arm
     (WIFISERVER "wifi-server" 0 0 #"
/*
  (wifi-server)
  Starts a Wi-Fi server running. It returns nil.
*/
object *fn_wifiserver (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) args, (void) env;
  server.begin();
  return nil;
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

     #+esp
     (WIFISOFTAP "wifi-softap" 0 4 #"
/*
  (wifi-softap ssid [password channel hidden])
  Set up a soft access point to establish a Wi-Fi network.
  Returns the IP address as a string or nil if unsuccessful.
*/
object *fn_wifisoftap (object *args, object *env) {
  (void) env;
  char ssid[33], pass[65];
  if (args == NULL) return WiFi.softAPdisconnect(true) ? tee : nil;
  object *first = first(args); args = cdr(args);
  if (args == NULL) WiFi.softAP(cstring(first, ssid, 33));
  else {
    object *second = first(args);
    args = cdr(args);
    int channel = 1;
    bool hidden = false;
    if (args != NULL) {
      channel = checkinteger(first(args));
      args = cdr(args);
      if (args != NULL) hidden = (first(args) != nil);
    }
    WiFi.softAP(cstring(first, ssid, 33), cstring(second, pass, 65), channel, hidden);
  }
  return iptostring(WiFi.softAPIP());
}"#)

     #+arm
     (WIFISOFTAP "wifi-softap" 0 4 #"
/*
  (wifi-softap ssid [password channel hidden])
  Set up a soft access point to establish a Wi-Fi network.
  Returns the IP address as a string or nil if unsuccessful.
*/
object *fn_wifisoftap (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) env;
  char ssid[33], pass[65];
  object *first = first(args); args = cdr(args);
  if (args == NULL) WiFi.beginAP(cstring(first, ssid, 33));
  else {
    object *second = first(args);
    args = cdr(args);
    int channel = 1;
    if (args != NULL) {
      channel = checkinteger(first(args));
      args = cdr(args);
    }
    WiFi.beginAP(cstring(first, ssid, 33), cstring(second, pass, 65), channel);
  }
  return iptostring(WiFi.localIP());
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

     #+esp
     (CONNECTED nil 1 1 #"
/*
  (connected stream)
  Returns t or nil to indicate if the client on stream is connected.
*/
object *fn_connected (object *args, object *env) {
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(PSTR("invalid stream"));
  return client.connected() ? tee : nil;
}"#)

     #+arm
     (CONNECTED nil 1 1 #"
/*
  (connected stream)
  Returns t or nil to indicate if the client on stream is connected.
*/
object *fn_connected (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(PSTR("invalid stream"));
  return client.connected() ? tee : nil;
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

     #+esp
     (WIFILOCALIP "wifi-localip" 0 0 #"
/*
  (wifi-localip)
  Returns the IP address of the local network as a string.
*/
object *fn_wifilocalip (object *args, object *env) {
  (void) args, (void) env;
  return iptostring(WiFi.localIP());
}"#)

     #+arm
     (WIFILOCALIP "wifi-localip" 0 0 #"
/*
  (wifi-localip)
  Returns the IP address of the local network as a string.
*/
object *fn_wifilocalip (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) args, (void) env;
  return iptostring(WiFi.localIP());
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

     #+riscv
     (WIFILOCALIP "wifi-localip" 0 0 #"
/*
  (wifi-localip)
  Returns the IP address of the local network as a string.
*/
object *fn_wifilocalip (object *args, object *env) {
  (void) args, (void) env;
  return iptostring(WiFi.localIP());
}"#)

     #+esp
     (WIFICONNECT "wifi-connect" 0 3 #"
/*
  (wifi-connect [ssid pass])
  Connects to the Wi-Fi network ssid using password pass. It returns the IP address as a string.
*/
object *fn_wificonnect (object *args, object *env) {
  (void) env;
  char ssid[33], pass[65];
  if (args == NULL) { WiFi.disconnect(true); return nil; }
  if (cdr(args) == NULL) WiFi.begin(cstring(first(args), ssid, 33));
  else WiFi.begin(cstring(first(args), ssid, 33), cstring(second(args), pass, 65));
  int result = WiFi.waitForConnectResult();
  if (result == WL_CONNECTED) return iptostring(WiFi.localIP());
  else if (result == WL_NO_SSID_AVAIL) error2(PSTR("network not found"));
  else if (result == WL_CONNECT_FAILED) error2(PSTR("connection failed"));
  else error2(PSTR("unable to connect"));
  return nil;
}"#)

     #+arm
     (WIFICONNECT "wifi-connect" 0 3 #"
/*
  (wifi-connect [ssid pass])
  Connects to the Wi-Fi network ssid using password pass. It returns the IP address as a string.
*/
object *fn_wificonnect (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) env;
  char ssid[33], pass[65];
  int result = 0;
  if (args == NULL) { WiFi.disconnect(); return nil; }
  if (cdr(args) == NULL) WiFi.begin(cstring(first(args), ssid, 33));
  else {
    if (cddr(args) != NULL) WiFi.config(ipstring(third(args)));
    result = WiFi.begin(cstring(first(args), ssid, 33), cstring(second(args), pass, 65));
  }
  if (result == WL_CONNECTED) return iptostring(WiFi.localIP());
  else if (result == WL_NO_SSID_AVAIL) error2(PSTR("network not found"));
  else if (result == WL_CONNECT_FAILED) error2(PSTR("connection failed"));
  else error2(PSTR("unable to connect"));
  return nil;
  #else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

     #+riscv
     (WIFICONNECT "wifi-connect" 0 2 #"
/*
  (wifi-connect [ssid pass])
  Connects to the Wi-Fi network ssid using password pass. It returns the IP address as a string.
*/
object *fn_wificonnect (object *args, object *env) {
  (void) env;
  char ssid[33], pass[65];
  int status = WL_IDLE_STATUS;     // the Wifi radio's status
  // if (args == NULL) { WiFi.disconnect(true); return nil; }
  // if (cdr(args) == NULL) WiFi.begin(cstring(first(args), ssid, 33));
  while ( status != WL_CONNECTED) {
    status = WiFi.begin(cstring(first(args), ssid, 33), cstring(second(args), pass, 65));
  // int result = WiFi.waitForConnectResult();
  }
  return nil;
}"#)))

    #+gfx
    ("Graphics functions"

     ((WITHGFX "with-gfx" 1 127 #"
/*
  (with-gfx (str) form*)
  Evaluates the forms with str bound to an gfx-stream so you can print text
  to the graphics display using the standard uLisp print commands.
*/
object *sp_withgfx (object *args, object *env) {
#if defined(gfxsupport)
  object *params = checkarguments(args, 1, 1);
  object *var = first(params);
  object *pair = cons(var, stream(GFXSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  return result;
#else
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
#endif
}"#)) "sp" )

     
    #+gfx
    (nil

     ((DRAWPIXEL "draw-pixel" 2 3 #"
/*
  (draw-pixel x y [colour])
  Draws a pixel at coordinates (x,y) in colour, or white if omitted.
*/
object *fn_drawpixel (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(third(args));
  tft.drawPixel(checkinteger(first(args)), checkinteger(second(args)), colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (DRAWLINE "draw-line" 4 5 #"
/*
  (draw-line x0 y0 x1 y1 [colour])
  Draws a line from (x0,y0) to (x1,y1) in colour, or white if omitted.
*/
object *fn_drawline (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawLine(params[0], params[1], params[2], params[3], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (DRAWRECT "draw-rect" 4 5 #"
/*
  (draw-rect x y w h [colour])
  Draws an outline rectangle with its top left corner at (x,y), with width w,
  and with height h. The outline is drawn in colour, or white if omitted.
*/
object *fn_drawrect (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawRect(params[0], params[1], params[2], params[3], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (FILLRECT "fill-rect" 4 5 #"
/*
  (fill-rect x y w h [colour])
  Draws a filled rectangle with its top left corner at (x,y), with width w,
  and with height h. The outline is drawn in colour, or white if omitted.
*/
object *fn_fillrect (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.fillRect(params[0], params[1], params[2], params[3], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (DRAWCIRCLE "draw-circle" 3 4 #"
/*
  (draw-circle x y r [colour])
  Draws an outline circle with its centre at (x, y) and with radius r.
  The circle is drawn in colour, or white if omitted.
*/
object *fn_drawcircle (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawCircle(params[0], params[1], params[2], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (FILLCIRCLE "fill-circle" 3 4 #"
/*
  (fill-circle x y r [colour])
  Draws a filled circle with its centre at (x, y) and with radius r.
  The circle is drawn in colour, or white if omitted.
*/
object *fn_fillcircle (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.fillCircle(params[0], params[1], params[2], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (DRAWROUNDRECT "draw-round-rect" 5 6 #"
/*
  (draw-round-rect x y w h radius [colour])
  Draws an outline rounded rectangle with its top left corner at (x,y), with width w,
  height h, and corner radius radius. The outline is drawn in colour, or white if omitted.
*/
object *fn_drawroundrect (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (FILLROUNDRECT "fill-round-rect" 5 6 #"
/*
  (fill-round-rect x y w h radius [colour])
  Draws a filled rounded rectangle with its top left corner at (x,y), with width w,
  height h, and corner radius radius. The outline is drawn in colour, or white if omitted.
*/
object *fn_fillroundrect (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.fillRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (DRAWTRIANGLE "draw-triangle" 6 7 #"
/*
  (draw-triangle x0 y0 x1 y1 x2 y2 [colour])
  Draws an outline triangle between (x1,y1), (x2,y2), and (x3,y3).
  The outline is drawn in colour, or white if omitted.
*/
object *fn_drawtriangle (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (FILLTRIANGLE "fill-triangle" 6 7 #"
/*
  (fill-triangle x0 y0 x1 y1 x2 y2 [colour])
  Draws a filled triangle between (x1,y1), (x2,y2), and (x3,y3).
  The outline is drawn in colour, or white if omitted.
*/
object *fn_filltriangle (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.fillTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (DRAWCHAR "draw-char" 3 6 #"
/*
  (draw-char x y char [colour background size])
  Draws the character char with its top left corner at (x,y).
  The character is drawn in a 5 x 7 pixel font in colour against background,
  which default to white and black respectively.
  The character can optionally be scaled by size.
*/
object *fn_drawchar (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t colour = COLOR_WHITE, bg = COLOR_BLACK, size = 1;
  object *more = cdr(cddr(args));
  if (more != NULL) {
    colour = checkinteger(car(more));
    more = cdr(more);
    if (more != NULL) {
      bg = checkinteger(car(more));
      more = cdr(more);
      if (more != NULL) size = checkinteger(car(more));
    }
  }
  tft.drawChar(checkinteger(first(args)), checkinteger(second(args)), checkchar(third(args)),
    colour, bg, size);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (SETCURSOR "set-cursor" 2 2 #"
/*
  (set-cursor x y)
  Sets the start point for text plotting to (x, y).
*/
object *fn_setcursor (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.setCursor(checkinteger(first(args)), checkinteger(second(args)));
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (SETTEXTCOLOR "set-text-color" 1 2 #"
/*
  (set-text-color colour [background])
  Sets the text colour for text plotted using (with-gfx ...).
*/
object *fn_settextcolor (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  if (cdr(args) != NULL) tft.setTextColor(checkinteger(first(args)), checkinteger(second(args)));
  else tft.setTextColor(checkinteger(first(args)));
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (SETTEXTSIZE "set-text-size" 1 1 #"
/*
  (set-text-size scale)
  Scales text by the specified size, default 1.
*/
object *fn_settextsize (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.setTextSize(checkinteger(first(args)));
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (SETTEXTWRAP "set-text-wrap" 1 1 #"
/*
  (set-text-wrap boolean)
  Specified whether text wraps at the right-hand edge of the display; the default is t.
*/
object *fn_settextwrap (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.setTextWrap(first(args) != NULL);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (FILLSCREEN "fill-screen" 0 1 #"
/*
  (fill-screen [colour])
  Fills or clears the screen with colour, default black.
*/
object *fn_fillscreen (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t colour = COLOR_BLACK;
  if (args != NULL) colour = checkinteger(first(args));
  tft.fillScreen(colour);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (SETROTATION "set-rotation" 1 1 #"
/*
  (set-rotation option)
  Sets the display orientation for subsequent graphics commands; values are 0, 1, 2, or 3.
*/
object *fn_setrotation (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.setRotation(checkinteger(first(args)));
  #else
  (void) args;
  #endif
  return nil;
}"#)

     (INVERTDISPLAY "invert-display" 1 1 #"
/*
  (invert-display boolean)
  Mirror-images the display. 
*/
object *fn_invertdisplay (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.invertDisplay(first(args) != NULL);
  #else
  (void) args;
  #endif
  return nil;
}"#)

     #+ignore
     (GETPIXEL "get-pixel" 2 2 #"
#if defined(gfxsupport)
uint16_t Technoblogy_ST7735::getPixel (uint16_t x, uint16_t y) {
  uint32_t ret = 0;
  startWrite();
  setAddrWindow(x, y, 1, 1);
  writeCommand(ST77XX_RAMRD);
  pinMode(TFT_MOSI, INPUT);
  pinMode(TFT_SCLK, OUTPUT);
  for (int i=0; i<33; i++) {
    digitalWrite(TFT_SCLK, HIGH);
    ret = ret<<1 | digitalRead(TFT_MOSI);
    digitalWrite(TFT_SCLK, LOW);
  }
  pinMode(TFT_MOSI, OUTPUT);
  endWrite();
  return ((ret & 0xf80000)>>8 | (ret & 0xfc00)>>5 | (ret & 0xf8)>>3);
}
#endif

object *fn_getpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  return number(tft.getPixel(checkinteger(first(args)), checkinteger(second(args))));
  #endif
}"#)

     #+ignore
     (GETPIXEL "get-pixel" 2 2 #"
object *fn_getpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) args, (void) env;
  error2(PSTR("not supported"));
  #endif
  return nil;
}"#)

     #+ignore
     (XORPIXEL "xor-pixel" 2 3 #"
#if defined(gfxsupport)
void Technoblogy_ST7735::xorPixel (uint16_t x, uint16_t y, uint16_t color) {
  uint16_t lastcolor = getPixel(x, y);
  if ((x >= 0) && (x < _width) && (y >= 0) && (y < _height)) {
    startWrite();
    writeCommand(ST77XX_RAMWR);
    SPI_WRITE16(color ^ lastcolor);
    endWrite();
  }
}
#endif

object *fn_xorpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(third(args));
  tft.xorPixel(checkinteger(first(args)), checkinteger(second(args)), colour);
  #endif
  return nil;
}"#)

     #+ignore
     (XORPIXEL "xor-pixel" 2 3 #"
object *fn_xorpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) args, (void) env;
  error2(PSTR("not supported"));
  #endif
  return nil;
}"#)

     #+ignore
     (XORSPRITE "xor-sprite" 4 5 #"
#if defined(gfxsupport)
void Technoblogy_ST7735::xorSprite (uint16_t x, uint16_t y, uint32_t top, uint32_t bottom, uint16_t color) {
  uint16_t row[8];
  uint32_t col = 0;
  bool bit;
  if ((x >= 0) && (x+7 < _width) && (y >= 0) && (y+7 < _height)) {
    for (int yd=0; yd<8; yd++) {
      startWrite();
      setAddrWindow(x, y+yd, 8, 1);
      writeCommand(ST77XX_RAMRD);
      pinMode(TFT_MOSI, INPUT);
      pinMode(TFT_SCLK, OUTPUT);
      for (int i=0; i<9; i++) {
        digitalWrite(TFT_SCLK, HIGH);
        digitalWrite(TFT_SCLK, LOW);
      }
      for (int xd=0; xd<8; xd++) {
        for (int i=0; i<24; i++) {
          digitalWrite(TFT_SCLK, HIGH);
          col = col<<1 | digitalRead(TFT_MOSI);
          digitalWrite(TFT_SCLK, LOW);
        }
        row[xd] = ((col & 0xf80000)>>8 | (col & 0xfc00)>>5 | (col & 0xf8)>>3);
      }
      pinMode(TFT_MOSI, OUTPUT);
      endWrite();
      startWrite();
      writeCommand(ST77XX_RAMWR);
      for (int xd=0; xd<8; xd++) {
        if (yd < 4) bit = top>>(31 - xd - yd*8) & 1;
        else bit = bottom>>(31 - xd - (yd-4)*8) & 1;
        if (bit) SPI_WRITE16(row[xd] ^ color);
        else SPI_WRITE16(row[xd]);
      }
      endWrite();
    }
  }
}
#endif

object *fn_xorsprite (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint32_t params[4]; uint16_t colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.xorSprite(params[0], params[1], params[2], params[3], colour);
  #endif
  return nil;
}"#)

     #+ignore
     (XORSPRITE "xor-sprite" 4 5 #"
object *fn_xorsprite (object *args, object *env) {
  #if defined(gfxsupport)
  (void) args, (void) env;
  error2(PSTR("not supported"));
  #endif
  return nil;
}"#)))

    #+ignore
    ("Graphics functions"
     
     ((DRAWPIXEL "draw-pixel" 2 3 #"
/*
  (draw-pixel x y [colour])
  Draws a pixel at coordinates (x,y) in colour, or white if omitted.
*/
object *fn_drawpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(third(args));
  tft.drawPixel(checkinteger(first(args)), checkinteger(second(args)), colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (DRAWLINE "draw-line" 4 5 #"
/*
  (draw-line x0 y0 x1 y1 [colour])
  Draws a line from (x0,y0) to (x1,y1) in colour, or white if omitted.
*/
object *fn_drawline (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawLine(params[0], params[1], params[2], params[3], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (DRAWRECT "draw-rect" 4 5 #"
/*
  (draw-rect x y w h [colour])
  Draws an outline rectangle with its top left corner at (x,y), with width w,
  and with height h. The outline is drawn in colour, or white if omitted.
*/
object *fn_drawrect (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawRect(params[0], params[1], params[2], params[3], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (FILLRECT "fill-rect" 4 5 #"
/*
  (fill-rect x y w h [colour])
  Draws a filled rectangle with its top left corner at (x,y), with width w,
  and with height h. The outline is drawn in colour, or white if omitted.
*/
object *fn_fillrect (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.fillRect(params[0], params[1], params[2], params[3], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (DRAWCIRCLE "draw-circle" 3 4 #"
/*
  (draw-circle x y r [colour])
  Draws an outline circle with its centre at (x, y) and with radius r.
  The circle is drawn in colour, or white if omitted.
*/
object *fn_drawcircle (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawCircle(params[0], params[1], params[2], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (FILLCIRCLE "fill-circle" 3 4 #"
/*
  (fill-circle x y r [colour])
  Draws a filled circle with its centre at (x, y) and with radius r.
  The circle is drawn in colour, or white if omitted.
*/
object *fn_fillcircle (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.fillCircle(params[0], params[1], params[2], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (DRAWROUNDRECT "draw-round-rect" 5 6 #"
/*
  (draw-round-rect x y w h radius [colour])
  Draws an outline rounded rectangle with its top left corner at (x,y), with width w,
  height h, and corner radius radius. The outline is drawn in colour, or white if omitted.
*/
object *fn_drawroundrect (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (FILLROUNDRECT "fill-round-rect" 5 6 #"
/*
  (fill-round-rect x y w h radius [colour])
  Draws a filled rounded rectangle with its top left corner at (x,y), with width w,
  height h, and corner radius radius. The outline is drawn in colour, or white if omitted.
*/
object *fn_fillroundrect (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.fillRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (DRAWTRIANGLE "draw-triangle" 6 7 #"
/*
  (draw-triangle x0 y0 x1 y1 x2 y2 [colour])
  Draws an outline triangle between (x1,y1), (x2,y2), and (x3,y3).
  The outline is drawn in colour, or white if omitted.
*/
object *fn_drawtriangle (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.drawTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (FILLTRIANGLE "fill-triangle" 6 7 #"
/*
  (fill-triangle x0 y0 x1 y1 x2 y2 [colour])
  Draws a filled triangle between (x1,y1), (x2,y2), and (x3,y3).
  The outline is drawn in colour, or white if omitted.
*/
object *fn_filltriangle (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.fillTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (DRAWCHAR "draw-char" 3 6 #"
/*
  (draw-char x y char [colour background size])
  Draws the character char with its top left corner at (x,y).
  The character is drawn in a 5 x 7 pixel font in colour against background,
  which default to white and black respectively.
  The character can optionally be scaled by size.
*/
object *fn_drawchar (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE, bg = COLOR_BLACK, size = 1;
  object *more = cdr(cddr(args));
  if (more != NULL) {
    colour = checkinteger(car(more));
    more = cdr(more);
    if (more != NULL) {
      bg = checkinteger(car(more));
      more = cdr(more);
      if (more != NULL) size = checkinteger(car(more));
    }
  }
  tft.drawChar(checkinteger(first(args)), checkinteger(second(args)), checkchar(DRAWCHAR, third(args)),
    colour, bg, size);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (SETCURSOR "set-cursor" 2 2 #"
/*
  (set-cursor x y)
  Sets the start point for text plotting to (x, y).
*/
object *fn_setcursor (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  tft.setCursor(checkinteger(first(args)), checkinteger(second(args)));
  #endif
  return nil;
}"#)

     (SETTEXTCOLOR "set-text-color" 1 2 #"
/*
  (set-text-color colour [background])
  Sets the text colour for text plotted using (with-gfx ...).
*/
object *fn_settextcolor (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  if (cdr(args) != NULL) tft.setTextColor(checkinteger(first(args)), checkinteger(second(args)));
  else tft.setTextColor(checkinteger(first(args)));
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (SETTEXTSIZE "set-text-size" 1 1 #"
/*
  (set-text-size scale)
  Scales text by the specified size, default 1.
*/
object *fn_settextsize (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  tft.setTextSize(checkinteger(first(args)));
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (SETTEXTWRAP "set-text-wrap" 1 1 #"
/*
  (set-text-wrap boolean)
  Specified whether text wraps at the right-hand edge of the display; the default is t.
*/
object *fn_settextwrap (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  tft.setTextWrap(first(args) != NULL);
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (FILLSCREEN "fill-screen" 0 1 #"
/*
  (fill-screen [colour])
  Fills or clears the screen with colour, default black.
*/
object *fn_fillscreen (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_BLACK;
  if (args != NULL) colour = checkinteger(first(args));
  tft.fillScreen(colour);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (SETROTATION "set-rotation" 1 1 #"
/*
  (set-rotation option)
  Sets the display orientation for subsequent graphics commands; values are 0, 1, 2, or 3.
*/
object *fn_setrotation (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  tft.setRotation(checkinteger(first(args)));
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     (INVERTDISPLAY "invert-display" 1 1 #"
/*
  (invert-display boolean)
  Mirror-images the display. 
*/
object *fn_invertdisplay (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  tft.invertDisplay(first(args) != NULL);
  tft.display();
  #else
  (void) args, (void) env;
  #endif
  return nil;
}"#)

     #+ignore
     (GETPIXEL "get-pixel" 2 2 #"
#if defined(gfxsupport)
uint16_t Technoblogy_ST7735::getPixel (uint16_t x, uint16_t y) {
  uint32_t ret = 0;
  startWrite();
  setAddrWindow(x, y, 1, 1);
  writeCommand(ST77XX_RAMRD);
  pinMode(TFT_MOSI, INPUT);
  pinMode(TFT_SCLK, OUTPUT);
  for (int i=0; i<33; i++) {
    digitalWrite(TFT_SCLK, HIGH);
    ret = ret<<1 | digitalRead(TFT_MOSI);
    digitalWrite(TFT_SCLK, LOW);
  }
  pinMode(TFT_MOSI, OUTPUT);
  endWrite();
  return ((ret & 0xf80000)>>8 | (ret & 0xfc00)>>5 | (ret & 0xf8)>>3);
}
#endif

object *fn_getpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  return number(tft.getPixel(checkinteger(first(args)), checkinteger(second(args))));
  #endif
}"#)

     #+ignore
     (GETPIXEL "get-pixel" 2 2 #"
object *fn_getpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

     #+ignore
     (XORPIXEL "xor-pixel" 2 3 #"
#if defined(gfxsupport)
void Technoblogy_ST7735::xorPixel (uint16_t x, uint16_t y, uint16_t color) {
  uint16_t lastcolor = getPixel(x, y);
  if ((x >= 0) && (x < _width) && (y >= 0) && (y < _height)) {
    startWrite();
    writeCommand(ST77XX_RAMWR);
    SPI_WRITE16(color ^ lastcolor);
    endWrite();
  }
}
#endif

object *fn_xorpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(third(args));
  tft.xorPixel(checkinteger(first(args)), checkinteger(second(args)), colour);
  #endif
  return nil;
}"#)

     #+ignore
     (XORPIXEL "xor-pixel" 2 3 #"
object *fn_xorpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) args, (void) env;
  error2(PSTR("not supported"));
  return nil;
  #endif
}"#)

     #+ignore
     (XORSPRITE "xor-sprite" 4 5 #"
#if defined(gfxsupport)
void Technoblogy_ST7735::xorSprite (uint16_t x, uint16_t y, uint32_t top, uint32_t bottom, uint16_t color) {
  uint16_t row[8];
  uint32_t col = 0;
  bool bit;
  if ((x >= 0) && (x+7 < _width) && (y >= 0) && (y+7 < _height)) {
    for (int yd=0; yd<8; yd++) {
      startWrite();
      setAddrWindow(x, y+yd, 8, 1);
      writeCommand(ST77XX_RAMRD);
      pinMode(TFT_MOSI, INPUT);
      pinMode(TFT_SCLK, OUTPUT);
      for (int i=0; i<9; i++) {
        digitalWrite(TFT_SCLK, HIGH);
        digitalWrite(TFT_SCLK, LOW);
      }
      for (int xd=0; xd<8; xd++) {
        for (int i=0; i<24; i++) {
          digitalWrite(TFT_SCLK, HIGH);
          col = col<<1 | digitalRead(TFT_MOSI);
          digitalWrite(TFT_SCLK, LOW);
        }
        row[xd] = ((col & 0xf80000)>>8 | (col & 0xfc00)>>5 | (col & 0xf8)>>3);
      }
      pinMode(TFT_MOSI, OUTPUT);
      endWrite();
      startWrite();
      writeCommand(ST77XX_RAMWR);
      for (int xd=0; xd<8; xd++) {
        if (yd < 4) bit = top>>(31 - xd - yd*8) & 1;
        else bit = bottom>>(31 - xd - (yd-4)*8) & 1;
        if (bit) SPI_WRITE16(row[xd] ^ color);
        else SPI_WRITE16(row[xd]);
      }
      endWrite();
    }
  }
}
#endif

object *fn_xorsprite (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint32_t params[4]; uint16_t colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft.xorSprite(params[0], params[1], params[2], params[3], colour);
  return nil;
  #endif
}"#)

     #+ignore
     (XORSPRITE "xor-sprite" 4 5 #"
object *fn_xorsprite (object *args, object *env) {
  #if defined(gfxsupport)
  (void) args, (void) env;
  error2(PSTR("not supported"));
  #endif
  return nil;
}"#)))

    #+badge
    ("Lisp Badge plotting"
     
     ((PLOT nil 0 6 #"
void plotsub (uint8_t x, uint8_t y, uint8_t n, int ys[5]) {
  if (y<64) {
    uint8_t grey = 0x0F-n*3;
    uint8_t blob = grey;
    if ((x&1) == 0) { blob = grey<<4; ys[n] = y; }
    else {
      for (int i=0; i<5; i++) {
        if (y == ys[i]) blob = (0x0F-i*3)<<4 | grey;
      }
    }
    PlotByte(x>>1, y, blob);
  }
}

object *fn_plot (object *args, object *env) {
  int ys[5] = {-1, -1, -1, -1, -1};
  int xaxis = -1, yaxis = -1;
  delay(20);
  ClearDisplay(0); // Clear display
  if (args != NULL && integerp(first(args))) { xaxis = checkinteger(first(args)); args = cdr(args); }
  if (args != NULL && integerp(first(args))) { yaxis = checkinteger(first(args)); args = cdr(args); }
  int nargs = min(listlength(args),4);
  for (int x=0; x<256; x++) {
    object *rest = args;
    for (int n=0; n<nargs; n++) {
      object *function = first(rest);
      int y = checkinteger(apply(function, cons(number(x), NULL), env));
      plotsub(x, y, n+1, ys);
      rest = cdr(rest);
    }
    plotsub(x, yaxis, 0, ys);
    if (x == xaxis) for (int y=0; y<64; y++) plotsub(x, y, 0, ys);
    if ((x&1) != 0) for (int i=0; i<5; i++) ys[i] = -1;
  }
  while (!tstflag(ESCAPE)); clrflag(ESCAPE);
  return symbol(NOTHING);
}"#)

     (PLOT3D nil 0 3 #"
object *fn_plot3d (object *args, object *env) {
  int xaxis = -1, yaxis = -1;
  uint8_t blob;
  delay(20);
  ClearDisplay(0); // Clear display
  if (args != NULL && integerp(first(args))) { xaxis = checkinteger(PLOT3D, first(args)); args = cdr(args); }
  if (args != NULL && integerp(first(args))) { yaxis = checkinteger(PLOT3D, first(args)); args = cdr(args); }
  if (args != NULL) {
    object *function = first(args);
    for (int y=0; y<64; y++) {
      for (int x=0; x<256; x++) {
        int z = checkinteger(PLOT3D, apply(function, cons(number(x), cons(number(y), NULL)), env));
        if (x == xaxis || y == yaxis) z = 0xF;
        if ((x&1) == 0) blob = z<<4; else blob = blob | (z&0xF);
        PlotByte(x>>1, y, blob);
      }
    }
  }
  while (!tstflag(ESCAPE)); clrflag(ESCAPE);
  return symbol(NOTHING);
}"#)

     (GLYPHPIXEL "glyph-pixel" 3 3 #"
extern const uint8_t CharMap[96][6] PROGMEM;

object *fn_glyphpixel (object *args, object *env) {
  (void) env;
  uint8_t c = 0, x = 6, y = 8;
  c = checkchar(GLYPHPIXEL, first(args));
  x = checkinteger(second(args));
  y = checkinteger(third(args));
  if (x > 5 || y > 7) return number(0);
  return pgm_read_byte(&CharMap[(c & 0x7f) - 32][x]) & 1 << (7 - y) ? number(15) : number(0);
}"#)

     (PLOTPIXEL "plot-pixel" 2 3 #"
object *fn_plotpixel (object *args, object *env) {
  (void) env;
  int x = checkinteger(first(args));
  int y = checkinteger(second(args));
  args = cddr(args);
  uint8_t grey = 0xff;
  if (args != NULL) grey = checkinteger(first(args));
  PlotByte(x, y, grey);
  return nil;
}"#)

     (FILLSCREEN "fill-screen" 0 1 #"
object *fn_fillscreen (object *args, object *env) {
  (void) env;
  uint8_t grey = 0;
  if (args != NULL) grey = checkinteger(first(args));
  ClearDisplay(grey);
  return nil;
}"#)))

))