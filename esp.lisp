;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; ESP

(defparameter *title-esp*
#"/* uLisp ESP Release ~a - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - ~a

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/"#)

(defparameter *header-esp* #"
// Lisp Library
const char LispLibrary[] PROGMEM = "";

// Compile options

// #define resetautorun
#define printfreespace
// #define printgcs
// #define sdcardsupport
// #define gfxsupport
// #define lisplibrary
// #define lineeditor
// #define vt100
// #define extensions

// Includes

// #include "LispLibrary.h"
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>
#include <WiFi.h>

#if defined(gfxsupport)
#define COLOR_WHITE ST77XX_WHITE
#define COLOR_BLACK ST77XX_BLACK
#include <Adafruit_GFX.h>    // Core graphics library
#include <Adafruit_ST7789.h> // Hardware-specific library for ST7789
#if defined(ARDUINO_ESP32_DEV)
Adafruit_ST7789 tft = Adafruit_ST7789(5, 16, 19, 18);
#define TFT_BACKLITE 4
#else
Adafruit_ST7789 tft = Adafruit_ST7789(TFT_CS, TFT_DC, MOSI, SCK, TFT_RST);
#endif
#endif

#if defined(sdcardsupport)
  #include <SD.h>
  #define SDSIZE 172
#else
  #define SDSIZE 0
#endif"#)

(defparameter *workspace-esp* #"
// Platform specific settings

#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 36  // Number of bits+4

#if defined(ARDUINO_FEATHER_ESP32)
  #define WORKSPACESIZE (9216-SDSIZE)     /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13

#elif defined(ARDUINO_ADAFRUIT_FEATHER_ESP32S2) || defined(ARDUINO_ADAFRUIT_FEATHER_ESP32S2_TFT)
  #define WORKSPACESIZE (8160-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13

#elif defined(ARDUINO_ADAFRUIT_QTPY_ESP32_PICO)
  #define WORKSPACESIZE (9216-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13
  
#elif defined(ARDUINO_ADAFRUIT_QTPY_ESP32S2)
  #define WORKSPACESIZE (8160-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13

#elif defined(ARDUINO_ADAFRUIT_QTPY_ESP32C3)
  #define WORKSPACESIZE (9216-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13

#elif defined(ARDUINO_FEATHERS2)                 /* UM FeatherS2 */
  #define WORKSPACESIZE (8160-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13

#elif defined(ARDUINO_ESP32_DEV)                 /* For TTGO T-Display */
  #define WORKSPACESIZE (9216-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13

#elif defined(ARDUINO_ESP32S2_DEV)
  #define WORKSPACESIZE (8100-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13

#elif defined(ARDUINO_ESP32C3_DEV)
  #define WORKSPACESIZE (9216-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13

#elif defined(ARDUINO_ESP32S3_DEV)
  #define WORKSPACESIZE (22000-SDSIZE)            /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13

#elif defined(ESP32)
  #define WORKSPACESIZE (9216-SDSIZE)     /* Cells (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13
  #define LED_BUILTIN 13

#else
#error "Board not supported!"
#endif"#)

(defparameter *check-pins-esp* #"
// Check pins

void checkanalogread (int pin) {
#if defined(ESP32) || defined(ARDUINO_ESP32_DEV)
  if (!(pin==0 || pin==2 || pin==4 || (pin>=12 && pin<=15) || (pin>=25 && pin<=27) || (pin>=32 && pin<=36) || pin==39))
    error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_FEATHER_ESP32)
  if (!(pin==4 || (pin>=12 && pin<=15) || (pin>=25 && pin<=27) || (pin>=32 && pin<=36) || pin==39))
    error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_ADAFRUIT_FEATHER_ESP32S2) || defined(ARDUINO_ADAFRUIT_FEATHER_ESP32S2_TFT)
  if (!(pin==8 || (pin>=14 && pin<=18))) error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_ADAFRUIT_QTPY_ESP32_PICO)
  if (!(pin==4 || pin==7 || (pin>=12 && pin<=15) || (pin>=25 && pin<=27) || (pin>=32 && pin<=33))) error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_ADAFRUIT_QTPY_ESP32S2)
  if (!((pin>=5 && pin<=9) || (pin>=16 && pin<=18))) error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_ADAFRUIT_QTPY_ESP32C3)
  if (!((pin>=0 && pin<=1) || (pin>=3 && pin<=5))) error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_FEATHERS2) | defined(ARDUINO_ESP32S2_DEV)
  if (!((pin>=1 && pin<=20))) error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_ESP32C3_DEV)
  if (!((pin>=0 && pin<=5))) error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_ESP32S3_DEV)
  if (!((pin>=1 && pin<=20))) error(PSTR("invalid pin"), number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(ESP32) || defined(ARDUINO_FEATHER_ESP32) || defined(ARDUINO_ESP32_DEV) || defined(ARDUINO_ADAFRUIT_QTPY_ESP32_PICO)
  if (!(pin>=25 && pin<=26)) error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_ADAFRUIT_FEATHER_ESP32S2) || defined(ARDUINO_ADAFRUIT_FEATHER_ESP32S2_TFT) || defined(ARDUINO_ADAFRUIT_QTPY_ESP32S2) || defined(ARDUINO_FEATHERS2) || defined(ARDUINO_ESP32S2_DEV)
  if (!(pin>=17 && pin<=18)) error(PSTR("invalid pin"), number(pin));
#elif defined(ARDUINO_ESP32C3_DEV) | defined(ARDUINO_ESP32S3_DEV) | defined(ARDUINO_ADAFRUIT_QTPY_ESP32C3)
  error2(ANALOGWRITE, PSTR("not supported"));
#endif
}"#)


(defparameter *note-esp* #"
// Note

void tone (int pin, int note) {
  (void) pin, (void) note;
}

void noTone (int pin) {
  (void) pin;
}

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
  int oct = octave + note/12;
  int prescaler = 8 - oct;
  if (prescaler<0 || prescaler>8) error(PSTR("octave out of range"), number(oct));
  tone(pin, scale[note%12]>>prescaler);
}

void nonote (int pin) {
  noTone(pin);
}"#)

(defparameter *sleep-esp* #"
// Sleep

void initsleep () { }

void doze (int secs) {
  delay(1000 * secs);
}"#)

(defparameter *keywords-esp*
  '((nil
     ((NIL LED_BUILTIN)
      (DIGITALWRITE HIGH LOW)
      (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)))))