;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; AVR

(defparameter *header-avr*
#"/* uLisp AVR Version 3.6 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - unreleased

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// Lisp Library
const char LispLibrary[] PROGMEM = "";

// Compile options

#define checkoverflow
// #define resetautorun
#define printfreespace
// #define printgcs
// #define sdcardsupport
// #define lisplibrary
#define assemblerlist
// #define lineeditor
// #define vt100

// Includes

// #include "LispLibrary.h"
#include <avr/sleep.h>
#include <setjmp.h>
#include <SPI.h>
#include <limits.h>
#include <EEPROM.h>

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 172
#else
#define SDSIZE 0
#endif"#)

(defparameter *workspace-avr* #"
// Platform specific settings

#define WORDALIGNED __attribute__((aligned (2)))
#define BUFFERSIZE 21                     /* longest builtin name + 1 */

#if defined(__AVR_ATmega328P__)
  #define WORKSPACESIZE (314-SDSIZE)      /* Objects (4*bytes) */
  #define EEPROMSIZE 1024                 /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 0
  #define CPU_ATmega328P

#elif defined(__AVR_ATmega2560__)
  #define WORKSPACESIZE (1214-SDSIZE)     /* Objects (4*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define STACKDIFF 320
  #define CPU_ATmega2560

#elif defined(__AVR_ATmega1284P__)
  #include "optiboot.h"
  #define WORKSPACESIZE (2816-SDSIZE)     /* Objects (4*bytes) */
//  #define EEPROMSIZE 4096                 /* Bytes */
  #define FLASHWRITESIZE 16384            /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 96                     /* Bytes <= 256 */
  #define STACKDIFF 320
  #define CPU_ATmega1284P

#elif defined(ARDUINO_AVR_NANO_EVERY)
  #define WORKSPACESIZE (1060-SDSIZE)     /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320
  #define CPU_ATmega4809
  
#elif defined(ARDUINO_AVR_ATmega4809)     /* Curiosity Nano using MegaCoreX */
  #define Serial Serial3
  #define WORKSPACESIZE (1065-SDSIZE)     /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320
  #define CPU_ATmega4809

#elif defined(__AVR_ATmega4809__)
  #define WORKSPACESIZE (1065-SDSIZE)     /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320
  #define CPU_ATmega4809

#elif defined(__AVR_AVR128DA48__)
  #include <Flash.h>
  #define Serial Serial1
  #define WORKSPACESIZE (2800-SDSIZE)     /* Objects (4*bytes) */
  #define FLASHWRITESIZE 16384            /* Bytes */
  #define SYMBOLTABLESIZE 480             /* Bytes */
  #define CODESIZE 96                     /* Bytes <= 512 */
  #define STACKDIFF 320
  #define CPU_AVR128DX48

#elif defined(__AVR_AVR128DB48__)
  #include <Flash.h>
  #define Serial Serial3
  #define WORKSPACESIZE (2750-SDSIZE)     /* Objects (4*bytes) */
  #define FLASHWRITESIZE 16384            /* Bytes */
  #define SYMBOLTABLESIZE 480             /* Bytes */
  #define CODESIZE 96                     /* Bytes <= 512 */
  #define STACKDIFF 320
  #define CPU_AVR128DX48
  
#else
#error "Board not supported!"
#endif"#)

(defparameter *stream-interface-avr* #"
// Streams

inline int spiread () { return SPI.transfer(0); }
#if defined(CPU_ATmega1284P) || defined(CPU_AVR128DX48)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
#elif defined(CPU_ATmega2560)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
inline int serial2read () { while (!Serial2.available()) testescape(); return Serial2.read(); }
inline int serial3read () { while (!Serial3.available()) testescape(); return Serial3.read(); }
#endif
#if defined(sdcardsupport)
File SDpfile, SDgfile;
inline int SDread () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  return SDgfile.read();
}
#endif

void serialbegin (int address, int baud) {
  #if defined(CPU_ATmega328P)
  (void) address; (void) baud;
  #elif defined(CPU_ATmega1284P) || defined(CPU_AVR128DX48)
  if (address == 1) Serial1.begin((long)baud*100);
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
  #elif defined(CPU_ATmega2560)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  else if (address == 3) Serial3.begin((long)baud*100);
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
  #endif
}

void serialend (int address) {
  #if defined(CPU_ATmega328P)
  (void) address;
  #elif defined(CPU_ATmega1284P) || defined(CPU_AVR128DX48)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  #elif defined(CPU_ATmega2560)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  else if (address == 2) {Serial2.flush(); Serial2.end(); }
  else if (address == 3) {Serial3.flush(); Serial3.end(); }
  #endif
}

gfun_t gstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  gfun_t gfun = gserial;
  if (args != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) gfun = (gfun_t)I2Cread;
  else if (streamtype == SPISTREAM) gfun = spiread;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    #if defined(CPU_ATmega1284P) || defined(CPU_AVR128DX48)
    else if (address == 1) gfun = serial1read;
    #elif defined(CPU_ATmega2560)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    else if (address == 3) gfun = serial3read;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  else error2(0, PSTR("Unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
#if defined(CPU_ATmega1284P) || defined(CPU_AVR128DX48)
inline void serial1write (char c) { Serial1.write(c); }
#elif defined(CPU_ATmega2560)
inline void serial1write (char c) { Serial1.write(c); }
inline void serial2write (char c) { Serial2.write(c); }
inline void serial3write (char c) { Serial3.write(c); }
#endif
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif

pfun_t pstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  pfun_t pfun = pserial;
  if (args != NULL && first(args) != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) pfun = (pfun_t)I2Cwrite;
  else if (streamtype == SPISTREAM) pfun = spiwrite;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    #if defined(CPU_ATmega1284P) || defined(CPU_AVR128DX48)
    else if (address == 1) pfun = serial1write;
    #elif defined(CPU_ATmega2560)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    else if (address == 3) pfun = serial3write;
    #endif
  }   
  else if (streamtype == STRINGSTREAM) {
    pfun = pstr;
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return pfun;
}"#)

(defparameter *watchdog-avr* #"
// Watchdog

void watchdogenable (int interval) {
  int i = 5;
  while (interval) { interval = interval>>1; i++; }
  wdt_enable(i);
}

void watchdogreset () {
  wdt_reset();
}"#)


(defparameter *check-pins-avr* #"
// Check pins - these are board-specific not processor-specific

void checkanalogread (int pin) {
#if defined(__AVR_ATmega328P__)
  if (!(pin>=0 && pin<=5)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(__AVR_ATmega2560__)
  if (!(pin>=0 && pin<=15)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(__AVR_ATmega1284P__)
  if (!(pin>=0 && pin<=7)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_AVR_NANO_EVERY)
  if (!((pin>=14 && pin<=21))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_AVR_ATmega4809)  /* MegaCoreX core */
  if (!((pin>=22 && pin<=33) || (pin>=36 && pin<=39))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(__AVR_ATmega4809__)
  if (!(pin>=14 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(__AVR_AVR128DA48__)
  if (!(pin>=22 && pin<=39)) error(ANALOGREAD, invalidpin, number(pin));
  #endif
}

void checkanalogwrite (int pin) {
#if defined(__AVR_ATmega328P__)
  if (!((pin>=2 && pin<=13) || pin==4 || pin==7 || pin==8)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(__AVR_ATmega2560__)
  if (!((pin>=2 && pin<=13) || (pin>=44 && pin<=46))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(__AVR_ATmega1284P__)
  if (!(pin==3 || pin==4 || pin==6 || pin==7 || (pin>=12 && pin<=15))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_AVR_NANO_EVERY)
  if (!(pin==3 || (pin>=5 && pin<=6) || (pin>=9 && pin<=11))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_AVR_ATmega4809)  /* MegaCoreX core */
  if (!((pin>=16 && pin<=19) || (pin>=38 && pin<=39))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(__AVR_ATmega4809__)
  if (!(pin==3 || pin==5 || pin==6 || pin==9 || pin==10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(__AVR_AVR128DA48__)
  if (!((pin>=4 && pin<=5) || (pin>=8 && pin<=19) || (pin>=38 && pin<=39))) error(ANALOGREAD, invalidpin, number(pin));
#endif
}"#)


(defparameter *note-avr* #"
// Note

#if defined(CPU_ATmega4809) || defined(CPU_AVR128DX48)
const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};
#else
const uint8_t scale[] PROGMEM = {239,226,213,201,190,179,169,160,151,142,134,127};
#endif

void playnote (int pin, int note, int octave) {
#if defined(CPU_ATmega328P)
  if (pin == 3) {
    DDRD = DDRD | 1<<DDD3; // PD3 (Arduino D3) as output
    TCCR2A = 0<<COM2A0 | 1<<COM2B0 | 2<<WGM20; // Toggle OC2B on match
  } else if (pin == 11) {
    DDRB = DDRB | 1<<DDB3; // PB3 (Arduino D11) as output
    TCCR2A = 1<<COM2A0 | 0<<COM2B0 | 2<<WGM20; // Toggle OC2A on match
  } else error(NOTE, invalidpin, number(pin));
  int prescaler = 9 - octave - note/12;
  if (prescaler<3 || prescaler>6) error(NOTE, PSTR("octave out of range"), number(prescaler));
  OCR2A = pgm_read_byte(&scale[note%12]) - 1;
  TCCR2B = 0<<WGM22 | prescaler<<CS20;

#elif defined(CPU_ATmega2560)
  if (pin == 9) {
    DDRH = DDRH | 1<<DDH6; // PH6 (Arduino D9) as output
    TCCR2A = 0<<COM2A0 | 1<<COM2B0 | 2<<WGM20; // Toggle OC2B on match
  } else if (pin == 10) {
    DDRB = DDRB | 1<<DDB4; // PB4 (Arduino D10) as output
    TCCR2A = 1<<COM2A0 | 0<<COM2B0 | 2<<WGM20; // Toggle OC2A on match
  } else error(NOTE, invalidpin, number(pin));
  int prescaler = 9 - octave - note/12;
  if (prescaler<3 || prescaler>6) error(NOTE, PSTR("octave out of range"), number(prescaler));
  OCR2A = pgm_read_byte(&scale[note%12]) - 1;
  TCCR2B = 0<<WGM22 | prescaler<<CS20;

#elif defined(CPU_ATmega1284P)
  if (pin == 14) {
    DDRD = DDRD | 1<<DDD6; // PD6 (Arduino D14) as output
    TCCR2A = 0<<COM2A0 | 1<<COM2B0 | 2<<WGM20; // Toggle OC2B on match
  } else if (pin == 15) {
    DDRD = DDRD | 1<<DDD7; // PD7 (Arduino D15) as output
    TCCR2A = 1<<COM2A0 | 0<<COM2B0 | 2<<WGM20; // Toggle OC2A on match
  } else error(NOTE, invalidpin, number(pin));
  int prescaler = 9 - octave - note/12;
  if (prescaler<3 || prescaler>6) error(NOTE, PSTR("octave out of range"), number(prescaler));
  OCR2A = pgm_read_byte(&scale[note%12]) - 1;
  TCCR2B = 0<<WGM22 | prescaler<<CS20;

#elif defined(CPU_ATmega4809)
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, scale[note%12]>>prescaler);

#elif defined(CPU_AVR128DX48)
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, pgm_read_word(&scale[note%12])>>prescaler);
#endif
}

void nonote (int pin) {
#if defined(CPU_ATmega4809) || defined(CPU_AVR128DX48)
  noTone(pin);
#else
  (void) pin;
  TCCR2B = 0<<WGM22 | 0<<CS20;
#endif
}"#)

(defparameter *sleep-avr* #"
// Sleep

#if !defined(CPU_ATmega4809) && !defined(CPU_AVR128DX48)
  // Interrupt vector for sleep watchdog
  ISR(WDT_vect) {
  WDTCSR |= 1<<WDIE;
}
#endif

void initsleep () {
  set_sleep_mode(SLEEP_MODE_PWR_DOWN);
}

void sleep (int secs) {
#if !defined(CPU_ATmega4809) && !defined(CPU_AVR128DX48)
  // Set up Watchdog timer for 1 Hz interrupt
  WDTCSR = 1<<WDCE | 1<<WDE;
  WDTCSR = 1<<WDIE | 6<<WDP0;     // 1 sec interrupt
  delay(100);  // Give serial time to settle
  // Disable ADC and timer 0
  ADCSRA = ADCSRA & ~(1<<ADEN);
#if defined(CPU_ATmega328P)
  PRR = PRR | 1<<PRTIM0;
#elif defined(CPU_ATmega2560) || defined(CPU_ATmega1284P)
  PRR0 = PRR0 | 1<<PRTIM0;
#endif
  while (secs > 0) {
    sleep_enable();
    sleep_cpu();
    secs--;
  }
  WDTCSR = 1<<WDCE | 1<<WDE;     // Disable watchdog
  WDTCSR = 0;
  // Enable ADC and timer 0
  ADCSRA = ADCSRA | 1<<ADEN;
#if defined(CPU_ATmega328P)
  PRR = PRR & ~(1<<PRTIM0);
#elif defined(CPU_ATmega2560) || defined(CPU_ATmega1284P)
  PRR0 = PRR0 & ~(1<<PRTIM0);
#endif
#else
  delay(1000*secs);
#endif
}"#)

(defparameter *interrupts-avr* #"
// Interrupts

#if defined(CPU_ATmega328P)
#define NINTERRUPTS 2+1
#elif defined(CPU_ATmega2560)
#define NINTERRUPTS 8+1
#elif defined(CPU_ATmega1284P)
#define NINTERRUPTS 3+1
#endif

unsigned int InterruptCount[NINTERRUPTS];

void handleInterrupts () {
  if (tstflag(BUSY)) return;
  object *nullenv = NULL;
  setflag(BUSY);
  int ints, flag;
  cli(); flag = tstflag(INTERRUPT); clrflag(INTERRUPT); sei();
  if (flag) {
    for (int i=0; i<NINTERRUPTS; i++) {
      cli(); ints = InterruptCount[i]; InterruptCount[i] = 0; sei();
      if (ints) {
        object *pair = assoc(number(i),Events);
        object *arg = cons(number(ints), NULL);
        push(arg, GCStack);
        if (pair != NULL) apply(cdr(pair), arg, &nullenv);
        pop(GCStack);
      }
    }
  }
  clrflag(BUSY);
}

void interrupt (int n) {
  setflag(INTERRUPT);
  if (InterruptCount[n] < 0xFFFF) InterruptCount[n]++;
}

//ISR(TIMER1_OVF_vect) { interrupt(0); }
ISR(INT0_vect) { interrupt(0); }
ISR(INT1_vect) { interrupt(1); }
#if defined(CPU_ATmega1284P)
ISR(INT2_vect) { interrupt(2); }
#elif defined(CPU_ATmega2560)
ISR(INT2_vect) { interrupt(2); }
ISR(INT3_vect) { interrupt(3); }
ISR(INT4_vect) { interrupt(4); }
ISR(INT5_vect) { interrupt(5); }
ISR(INT6_vect) { interrupt(6); }
ISR(INT7_vect) { interrupt(7); }
#endif"#)

(defparameter *keywords-avr*
  '(("CPU_ATmega328P"
     ((DIGITALWRITE HIGH LOW)
      (PINMODE INPUT INPUT_PULLUP OUTPUT)
      (ANALOGREFERENCE DEFAULT INTERNAL EXTERNAL)))
    ("CPU_ATmega1284P"
     ((DIGITALWRITE HIGH LOW)
      (PINMODE INPUT INPUT_PULLUP OUTPUT)
      (ANALOGREFERENCE DEFAULT INTERNAL1V1 INTERNAL2V56 EXTERNAL)))
    ("CPU_ATmega2560"
     ((DIGITALWRITE HIGH LOW)
      (PINMODE INPUT INPUT_PULLUP OUTPUT)
      (ANALOGREFERENCE DEFAULT INTERNAL1V1 INTERNAL2V56 EXTERNAL)))
    ("CPU_ATmega4809"
     ((DIGITALWRITE HIGH LOW)
      (PINMODE INPUT INPUT_PULLUP OUTPUT)
      (ANALOGREFERENCE DEFAULT INTERNAL VDD INTERNAL0V55 INTERNAL1V1 INTERNAL1V5 INTERNAL2V5 INTERNAL4V3 EXTERNAL)))
    ("CPU_AVR128DX48"
     ((DIGITALWRITE HIGH LOW)
      (PINMODE INPUT INPUT_PULLUP OUTPUT)
      (ANALOGREFERENCE DEFAULT VDD INTERNAL1V024 INTERNAL2V048 INTERNAL4V096 INTERNAL2V5 EXTERNAL)
      (ANALOGREAD ADC_DAC0 ADC_TEMPERATURE)))))