;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; RISC-V

(defparameter *header-riscv*
#"/* uLisp RISC-V Version 3.5 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - unreleased

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// Lisp Library
const char LispLibrary[] PROGMEM = "";

// Compile options

// #define resetautorun
#define printfreespace
// #define printgcs
// #define sdcardsupport
// #define gfxsupport
// #define lisplibrary
#define assemblerlist
// #define lineeditor
// #define vt100

// Includes

// #include "LispLibrary.h"
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>

#if defined(gfxsupport)
#include <Sipeed_ST7789.h>
SPIClass spi_(SPI0); // MUST be SPI0 for Maix series on board LCD
Sipeed_ST7789 tft(320, 240, spi_);
#endif

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 172
#else
#define SDSIZE 0
#endif"#)

(defparameter *workspace-riscv* #"
// Platform specific settings

#define WORDALIGNED __attribute__((aligned (8)))
#define BUFFERSIZE 34  // Number of bits+2
#define RAMFUNC __attribute__ ((section (".ramfunctions")))

#if defined(BOARD_SIPEED_MAIX_DUINO)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096
  #define CPU_K210

#elif defined(BOARD_SIPEED_MAIX_BIT)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096
  #define CPU_K210

#elif defined(BOARD_SIPEED_MAIX_ONE_DOCK)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096
  #define CPU_K210

#else
#error "Board not supported!"
#endif"#)

(defparameter *stream-interface-riscv* #"
// Streams

inline int spiread () { return SPI.transfer(0); }
#if defined(BOARD_SIPEED_MAIX_DUINO)
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
  #if defined(BOARD_SIPEED_MAIX_DUINO)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  else if (address == 3) Serial3.begin((long)baud*100);
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
  #endif
}

void serialend (int address) {
  #if defined(BOARD_SIPEED_MAIX_DUINO)
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
  else if (streamtype == SPISTREAM) {
    if (address < 128) gfun = spiread;
  }
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    #if defined(BOARD_SIPEED_MAIX_DUINO)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    else if (address == 3) gfun = serial3read;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
#if defined(BOARD_SIPEED_MAIX_DUINO)
inline void serial1write (char c) { Serial1.write(c); }
inline void serial2write (char c) { Serial2.write(c); }
inline void serial3write (char c) { Serial3.write(c); }
#endif
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif
#if defined(gfxsupport)
inline void gfxwrite (char c) { tft.write(c); }
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
  else if (streamtype == SPISTREAM) {
    if (address < 128) pfun = spiwrite;
  }
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    #if defined(BOARD_SIPEED_MAIX_DUINO)
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
  #if defined(gfxsupport)
  else if (streamtype == GFXSTREAM) pfun = (pfun_t)gfxwrite;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return pfun;
}"#)


(defparameter *check-pins-riscv* #"
// Check pins

void checkanalogread (int pin) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  if (!((pin>=32 && pin<=36) || pin==39)) error(ANALOGREAD, invalidpin, number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  if (!(pin>=0 && pin<=13)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(BOARD_SIPEED_MAIX_BIT)
  if (!(pin>=0 && pin<=35)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(BOARD_SIPEED_MAIX_ONE_DOCK)
  if (!(pin>=0 && pin<=47)) error(ANALOGWRITE, invalidpin, number(pin));
#endif
}"#)

(defparameter *note-riscv* #"
// Note

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, scale[note%12]>>prescaler);
#endif
}

void nonote (int pin) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  noTone(pin);
#endif
}"#)

(defparameter *sleep-riscv* #"
// Sleep

void initsleep () { }

void sleep (int secs) {
  delay(1000 * secs);
}"#)

(defparameter *keywords-riscv*
  '((nil
     ((DIGITALWRITE HIGH LOW)
      (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)))))
