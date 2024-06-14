;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; RISC-V

(defparameter *title-riscv*
#"/* uLisp RISC-V Release ~a - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - ~a

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/"#)

(defparameter *header-riscv* #"
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
// #define extensions

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
#define BUFFERSIZE 36  // Number of bits+4
#define RAMFUNC __attribute__ ((section (".ramfunctions")))

#if defined(BOARD_SIPEED_MAIX_DUINO)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096
  #define CPU_K210

#elif defined(BOARD_SIPEED_MAIX_BIT)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096
  #define CPU_K210

#elif defined(BOARD_SIPEED_MAIX_ONE_DOCK)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096
  #define CPU_K210

#else
#error "Board not supported!"
#endif"#)

(defparameter *check-pins-riscv* #"
// Check pins

void checkanalogread (int pin) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  if (!((pin>=32 && pin<=36) || pin==39)) error(invalidpin, number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  if (!(pin>=0 && pin<=13)) error(invalidpin, number(pin));
#elif defined(BOARD_SIPEED_MAIX_BIT)
  if (!(pin>=0 && pin<=35)) error(invalidpin, number(pin));
#elif defined(BOARD_SIPEED_MAIX_ONE_DOCK)
  if (!(pin>=0 && pin<=47)) error(invalidpin, number(pin));
#endif
}"#)

(defparameter *note-riscv* #"
// Note

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  int oct = octave + note/12;
  int prescaler = 8 - oct;
  if (prescaler<0 || prescaler>8) error(PSTR("octave out of range"), number(oct));
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

void doze (int secs) {
  delay(1000 * secs);
}"#)

(defparameter *keywords-riscv*
  '((nil
     ((NIL LED_BUILTIN)
      (DIGITALWRITE HIGH LOW)
      (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)))))
