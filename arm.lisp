;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; Arm

(defparameter *header-arm*
#"/* uLisp ARM Version 4.3 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - ???
   
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
#include <Adafruit_GFX.h>    // Core graphics library
#include <Adafruit_ST7735.h> // Hardware-specific library for ST7735
#define COLOR_WHITE 0xffff
#define COLOR_BLACK 0

// Adafruit PyBadge/PyGamer
#define TFT_CS        44  // Chip select
#define TFT_RST       46  // Display reset
#define TFT_DC        45  // Display data/command select
#define TFT_BACKLIGHT 47  // Display backlight pin
#define TFT_MOSI      41  // Data out
#define TFT_SCLK      42  // Clock out
Adafruit_ST7735 tft = Adafruit_ST7735(TFT_CS, TFT_DC, TFT_MOSI, TFT_SCLK, TFT_RST);
#endif

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 91
#else
#define SDSIZE 0
#endif"#)

(defparameter *workspace-arm* #"
// Platform specific settings

#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 36  // Number of bits+4
#define RAMFUNC __attribute__ ((section (".ramfunctions")))
#define MEMBANK

#if defined(ARDUINO_GEMMA_M0) || defined(ARDUINO_SEEED_XIAO_M0) || defined(ARDUINO_QTPY_M0)
  #define WORKSPACESIZE (2816-SDSIZE)     /* Objects (8*bytes) */
  #define EEPROMFLASH
  #define FLASHSIZE 32768                 /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define STACKDIFF 320
  #define CPU_ATSAMD21

#elif defined(ARDUINO_ITSYBITSY_M0) || defined(ARDUINO_SAMD_FEATHER_M0_EXPRESS)
  #define WORKSPACESIZE (2816-SDSIZE)     /* Objects (8*bytes) */
  #define DATAFLASH
  #define FLASHSIZE 2048000               /* 2 MBytes */
  #define CODESIZE 128                    /* Bytes */
  #define SDCARD_SS_PIN 4
  #define STACKDIFF 320
  #define CPU_ATSAMD21

#elif defined(ADAFRUIT_FEATHER_M0)        /* Feather M0 without DataFlash */
  #define WORKSPACESIZE (2816-SDSIZE)     /* Objects (8*bytes) */
  #define EEPROMFLASH
  #define FLASHSIZE 32768                 /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define SDCARD_SS_PIN 4
  #define STACKDIFF 320
  #define CPU_ATSAMD21

#elif defined(ARDUINO_METRO_M4) || defined(ARDUINO_ITSYBITSY_M4) || defined(ARDUINO_FEATHER_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
  #define WORKSPACESIZE (20608-SDSIZE)    /* Objects (8*bytes) */
  #define DATAFLASH
  #define FLASHSIZE 2048000               /* 2 MBytes */
  #define CODESIZE 256                    /* Bytes */
  #define SDCARD_SS_PIN 10
  #define STACKDIFF 400
  #define CPU_ATSAMD51

#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  #define WORKSPACESIZE (28800-SDSIZE)    /* Objects (8*bytes) */
  #define DATAFLASH
  #define FLASHSIZE 8192000               /* 8 MBytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 400
  #define CPU_ATSAMD51

#elif defined(ARDUINO_SAMD_MKRZERO)
  #define WORKSPACESIZE (2640-SDSIZE)     /* Objects (8*bytes) */
  #define EEPROMFLASH
  #define FLASHSIZE 32768                 /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define STACKDIFF 840
  #define CPU_ATSAMD21

#elif defined(ARDUINO_SAMD_ZERO)          /* Put this last, otherwise overrides the Adafruit boards */
  #define WORKSPACESIZE (2640-SDSIZE)     /* Objects (8*bytes) */
  #define EEPROMFLASH
  #define FLASHSIZE 32768                 /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define SDCARD_SS_PIN 10
  #define STACKDIFF 320
  #define CPU_ATSAMD21

#elif defined(ARDUINO_BBC_MICROBIT)
  #define WORKSPACESIZE 1344              /* Objects (8*bytes) */
  #define CODESIZE 64                     /* Bytes */
  #define STACKDIFF 320
  #define CPU_NRF51822

#elif defined(ARDUINO_BBC_MICROBIT_V2)
  #define WORKSPACESIZE 12928              /* Objects (8*bytes) */
  #define CODESIZE 128                     /* Bytes */
  #define STACKDIFF 320
  #define CPU_NRF52833

#elif defined(ARDUINO_CALLIOPE_MINI)
  #define WORKSPACESIZE 3392              /* Objects (8*bytes) */
  #define CODESIZE 64                     /* Bytes */
  #define STACKDIFF 320
  #define CPU_NRF51822

#elif defined(ARDUINO_SINOBIT)
  #define WORKSPACESIZE 1344              /* Objects (8*bytes) */
  #define CODESIZE 64                     /* Bytes */
  #define STACKDIFF 320
  #define CPU_NRF51822

#elif defined(ARDUINO_NRF52840_ITSYBITSY) || defined(ARDUINO_NRF52840_CLUE)
  #define WORKSPACESIZE (21120-SDSIZE)    /* Objects (8*bytes) */
  #define DATAFLASH
  #define FLASHSIZE 2048000               /* 2 MBytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 1200
  #define CPU_NRF52840

#elif defined(MAX32620)
  #define WORKSPACESIZE (24704-SDSIZE)    /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 320
  #define CPU_MAX32620
  #define Wire1 Wire2

#elif defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41)
  #define WORKSPACESIZE 60000             /* Objects (8*bytes) */
  #define LITTLEFS (960 * 1024)
  #include <LittleFS.h>
  LittleFS_Program LittleFS;
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 15000
  #define CPU_iMXRT1062
  #define SDCARD_SS_PIN BUILTIN_SDCARD
  #define BitOrder uint8_t
  #undef RAMFUNC
  #define RAMFUNC FASTRUN
  #undef MEMBANK
  #define MEMBANK DMAMEM

#elif defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040)
  #define WORKSPACESIZE (22912-SDSIZE)    /* Objects (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define FILE_WRITE_BEGIN "w"
  #define FILE_READ "r"
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 320
  #define CPU_RP2040

#elif defined(ARDUINO_RASPBERRY_PI_PICO_W)
  #define WORKSPACESIZE (15872-SDSIZE)    /* Objects (8*bytes) */
  #define LITTLEFS
  #include <WiFi.h>
  #include <LittleFS.h>
  #define FILE_WRITE_BEGIN "w"
  #define FILE_READ "r"
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 320
  #define CPU_RP2040

#else
#error "Board not supported!"
#endif"#)

(defparameter *stream-interface-arm* #"
// Streams

// Simplify board differences
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4) || defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W)
#define ULISP_SPI1
#endif
#if defined(ARDUINO_BBC_MICROBIT_V2) || defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41) || defined(MAX32620) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040)
#define ULISP_I2C1
#endif
#if defined(ARDUINO_SAM_DUE) || defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41)
#define ULISP_SERIAL3
#elif defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W)
#define ULISP_SERIAL2
#elif !defined(CPU_NRF51822) && !defined(CPU_NRF52833) && !defined(ARDUINO_FEATHER_F405)
#define ULISP_SERIAL1
#endif
#if defined(ARDUINO_RASPBERRY_PI_PICO_W)
#define ULISP_WIFI
#endif

inline int spiread () { return SPI.transfer(0); }
#if defined(ULISP_SPI1)
inline int spi1read () { return SPI1.transfer(0); }
#endif
inline int i2cread () { return I2Cread(&Wire); }
#if defined(ULISP_I2C1)
inline int i2c1read () { return I2Cread(&Wire1); }
#endif
#if defined(ULISP_SERIAL3)
inline int serial3read () { while (!Serial3.available()) testescape(); return Serial3.read(); }
#endif
#if defined(ULISP_SERIAL3) || defined(ULISP_SERIAL2)
inline int serial2read () { while (!Serial2.available()) testescape(); return Serial2.read(); }
#endif
#if defined(ULISP_SERIAL3) || defined(ULISP_SERIAL2) || defined(ULISP_SERIAL1)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
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

#if defined(ULISP_WIFI)
WiFiClient client;
WiFiServer server(80);

inline int WiFiread () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  return client.read();
}
#endif

void serialbegin (int address, int baud) {
  #if defined(ULISP_SERIAL3)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  else if (address == 3) Serial3.begin((long)baud*100);
  #elif defined(ULISP_SERIAL2)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  #elif defined(ULISP_SERIAL1)
  if (address == 1) Serial1.begin((long)baud*100);
  #else
  (void) baud;
  if (false);
  #endif
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
}

void serialend (int address) {
  #if defined(ULISP_SERIAL3)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  else if (address == 2) {Serial2.flush(); Serial2.end(); }
  else if (address == 3) {Serial3.flush(); Serial3.end(); }
  #elif defined(ULISP_SERIAL2)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  else if (address == 2) {Serial2.flush(); Serial2.end(); }
  #elif defined(ULISP_SERIAL1)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  #else
  (void) baud;
  if (false);
  #endif
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
}

gfun_t gstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  gfun_t gfun = gserial;
  if (args != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) {
    if (address < 128) gfun = i2cread;
    #if defined(ULISP_I2C1)
    else gfun = i2c1read;
    #endif
  } else if (streamtype == SPISTREAM) {
    if (address < 128) gfun = spiread;
    #if defined(ULISP_SPI1)
    else gfun = spi1read;
    #endif
  }
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    #if defined(ULISP_SERIAL3)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    else if (address == 3) gfun = serial3read;
    #elif defined(ULISP_SERIAL2)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    #elif defined(ULISP_SERIAL1)
    else if (address == 1) gfun = serial1read;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  #if defined(ULISP_WIFI)
  else if (streamtype == WIFISTREAM) gfun = (gfun_t)WiFiread;
  #endif
  else error2(NIL, PSTR("unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
#if defined(ULISP_SPI1)
inline void spi1write (char c) { SPI1.transfer(c); }
#endif
inline void i2cwrite (char c) { I2Cwrite(&Wire, c); }
#if defined(ULISP_I2C1)
inline void i2c1write (char c) { I2Cwrite(&Wire1, c); }
#endif
#if defined(ULISP_SERIAL3)
inline void serial1write (char c) { Serial1.write(c); }
inline void serial2write (char c) { Serial2.write(c); }
inline void serial3write (char c) { Serial3.write(c); }
#elif defined(ULISP_SERIAL2)
inline void serial2write (char c) { Serial2.write(c); }
inline void serial1write (char c) { Serial1.write(c); }
#elif defined(ULISP_SERIAL1)
inline void serial1write (char c) { Serial1.write(c); }
#endif
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif
#if defined(ULISP_WIFI)
inline void WiFiwrite (char c) { client.write(c); }
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
  if (streamtype == I2CSTREAM) {
    if (address < 128) pfun = i2cwrite;
    #if defined(ULISP_I2C1)
    else pfun = i2c1write;
    #endif
  } else if (streamtype == SPISTREAM) {
    if (address < 128) pfun = spiwrite;
    #if defined(ULISP_SPI1)
    else pfun = spi1write;
    #endif
  } else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    #if defined(ULISP_SERIAL3)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    else if (address == 3) pfun = serial3write;
    #elif defined(ULISP_SERIAL2)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    #elif defined(ULISP_SERIAL1)
    else if (address == 1) pfun = serial1write;
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
  #if defined(ULISP_WIFI)
  else if (streamtype == WIFISTREAM) pfun = (pfun_t)WiFiwrite;
  #endif
  else error2(NIL, PSTR("unknown stream type"));
  return pfun;
}"#)

(defparameter *check-pins-arm* #"
// Check pins - these are board-specific not processor-specific

void checkanalogread (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!(pin>=54 && pin<=65)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!(pin>=15 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!(pin>=14 && pin<=25)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin>=8 && pin<=10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_QTPY_M0)
  if (!((pin>=0 && pin<=3) || (pin>=6 && pin<=10))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SEEED_XIAO_M0)
  if (!(pin>=0 && pin<=10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=14 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin>=14 && pin<=20)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin>=14 && pin<=20)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=67 && pin<=74) || (pin>=54 && pin<=61)))  error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT)
  if (!((pin>=0 && pin<=4) || pin==10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT_V2)
  if (!((pin>=0 && pin<=4) || pin==10 || pin==29)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_CALLIOPE_MINI)
  if (!(pin==1 || pin==2 || (pin>=4 && pin<=6) || pin==21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SINOBIT)
  if (!((pin>=0 && pin<=4) || pin==10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  if (!(pin>=14 && pin<=20)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_CLUE)
  if (!((pin>=0 && pin<=4) || pin==10 || pin==12 || pin==16)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!(pin>=49 && pin<=52)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY40)
  if (!((pin>=14 && pin<=27))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY41)
  if (!((pin>=14 && pin<=27) || (pin>=38 && pin<=41))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  if (!(pin>=26 && pin<=29)) error(ANALOGREAD, invalidpin, number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!((pin>=2 && pin<=13) || pin==66 || pin==67)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || pin==14)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!((pin>=0 && pin<=8) || pin==10 || pin==18 || pin==19)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || (pin>=15 && pin<=16) || (pin>=22 && pin<=25))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin==0 || pin==2 || pin==9 || pin==10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_QTPY_M0)
  if (!(pin==0 || (pin>=2 && pin<=10))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SEEED_XIAO_M0)
  if (!(pin>=0 && pin<=10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=0 && pin<=15)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin==0 || pin==1 || pin==4 || pin==5 || pin==7 || (pin>=9 && pin<=15) || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin==0 || pin==1 || (pin>=4 && pin<=6) || (pin>=9 && pin<=13) || pin==14 || pin==15 || pin==17 || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=2 && pin<=9) || pin==11 || (pin>=13 && pin<=45) || pin==48 || (pin>=50 && pin<=53) || pin==58 || pin==61 || pin==68 || pin==69)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT)
  if (!(pin>=0 && pin<=32)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT_V2)
  if (!(pin>=0 && pin<=32)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_CALLIOPE_MINI)
  if (!(pin>=0 && pin<=30)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SINOBIT)
  if (!(pin>=0 && pin<=32)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  if (!(pin>=0 && pin<=25)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_CLUE)
  if (!(pin>=0 && pin<=46)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!((pin>=20 && pin<=29) || pin==32 || (pin>=40 && pin<=48))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY40)
  if (!((pin>=0 && pin<=15) || (pin>=18 && pin<=19) || (pin>=22 && pin<=25) || (pin>=28 && pin<=29) || (pin>=33 && pin<=39))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY41)
  if (!((pin>=0 && pin<=15) || (pin>=18 && pin<=19) || (pin>=22 && pin<=25) || (pin>=28 && pin<=29) || pin==33 || (pin>=36 && pin<=37))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  if (!(pin>=0 && pin<=29)) error(ANALOGWRITE, invalidpin, number(pin));
#endif
}"#)

(defparameter *note-arm* #"
// Note

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  if (!(pin>=26 && pin<=29)) error(ANALOGREAD, invalidpin, number(pin));
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, scale[note%12]>>prescaler);
#else
  (void) pin, (void) note, (void) octave;
#endif
}

void nonote (int pin) {
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  noTone(pin);
#else
  (void) pin;
#endif
}"#)

(defparameter *sleep-arm* #"
// Sleep

#if defined(CPU_ATSAMD21)
void WDT_Handler(void) {
  // ISR for watchdog early warning
  WDT->CTRL.bit.ENABLE = 0;        // Disable watchdog
  while(WDT->STATUS.bit.SYNCBUSY); // Sync CTRL write
  WDT->INTFLAG.bit.EW  = 1;        // Clear interrupt flag
}
#endif

void initsleep () {
#if defined(CPU_ATSAMD21)
 // One-time initialization of watchdog timer.

  // Generic clock generator 2, divisor = 32 (2^(DIV+1))
  GCLK->GENDIV.reg = GCLK_GENDIV_ID(2) | GCLK_GENDIV_DIV(4);
  // Enable clock generator 2 using low-power 32KHz oscillator.
  // With /32 divisor above, this yields 1024Hz clock.
  GCLK->GENCTRL.reg = GCLK_GENCTRL_ID(2) |
                      GCLK_GENCTRL_GENEN |
                      GCLK_GENCTRL_SRC_OSCULP32K |
                      GCLK_GENCTRL_DIVSEL;
  while(GCLK->STATUS.bit.SYNCBUSY);
  // WDT clock = clock gen 2
  GCLK->CLKCTRL.reg = GCLK_CLKCTRL_ID_WDT |
                      GCLK_CLKCTRL_CLKEN |
                      GCLK_CLKCTRL_GEN_GCLK2;

  // Enable WDT early-warning interrupt
  NVIC_DisableIRQ(WDT_IRQn);
  NVIC_ClearPendingIRQ(WDT_IRQn);
  NVIC_SetPriority(WDT_IRQn, 0);         // Top priority
  NVIC_EnableIRQ(WDT_IRQn);
#endif
}

void sleep (int secs) {
#if defined(CPU_ATSAMD21)
  WDT->CTRL.reg = 0;                     // Disable watchdog for config
  while(WDT->STATUS.bit.SYNCBUSY);
  WDT->INTENSET.bit.EW   = 1;            // Enable early warning interrupt
  WDT->CONFIG.bit.PER    = 0xB;          // Period = max
  WDT->CONFIG.bit.WINDOW = 0x7;          // Set time of interrupt = 1024 cycles = 1 sec
  WDT->CTRL.bit.WEN      = 1;            // Enable window mode
  while(WDT->STATUS.bit.SYNCBUSY);       // Sync CTRL write

  SysTick->CTRL = 0;                     // Stop SysTick interrupts

  while (secs > 0) {
    WDT->CLEAR.reg = WDT_CLEAR_CLEAR_KEY;// Clear watchdog interval
    while(WDT->STATUS.bit.SYNCBUSY);
    WDT->CTRL.bit.ENABLE = 1;            // Start watchdog now!
    while(WDT->STATUS.bit.SYNCBUSY);
    SCB->SCR |= SCB_SCR_SLEEPDEEP_Msk;   // Deepest sleep
    __DSB();
    __WFI();                             // Wait for interrupt
    secs--;
  }
  SysTick->CTRL = 7;                     // Restart SysTick interrupts
#else
  delay(1000*secs);
#endif
}"#)

(defparameter *keywords-arm*
  '((nil
     ((NIL LED_BUILTIN)
      (DIGITALWRITE HIGH LOW)))
    ("CPU_ATSAMD21"
     ((PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
      (ANALOGREFERENCE AR_DEFAULT AR_INTERNAL1V0 AR_INTERNAL1V65 AR_INTERNAL2V23 AR_EXTERNAL)
      (REGISTER (PA_DIR "PORT->Group[0].DIR.reg") (PA_DIRCLR "PORT->Group[0].DIRCLR.reg") (PA_DIRSET "PORT->Group[0].DIRSET.reg")
                (PA_DIRTGL "PORT->Group[0].DIRTGL.reg") (PA_OUT "PORT->Group[0].OUT.reg") (PA_OUTCLR "PORT->Group[0].OUTCLR.reg")
                (PA_OUTSET "PORT->Group[0].OUTSET.reg") (PA_OUTTGL "PORT->Group[0].OUTTGL.reg") (PA_IN "PORT->Group[0].IN.reg")
                (PB_DIR "PORT->Group[1].DIR.reg") (PB_DIRCLR "PORT->Group[1].DIRCLR.reg") (PB_DIRSET "PORT->Group[1].DIRSET.reg")
                (PB_DIRTGL "PORT->Group[1].DIRTGL.reg") (PB_OUT "PORT->Group[1].OUT.reg") (PB_OUTCLR "PORT->Group[1].OUTCLR.reg")
                (PB_OUTSET "PORT->Group[1].OUTSET.reg") (PB_OUTTGL "PORT->Group[1].OUTTGL.reg") (PB_IN "PORT->Group[1].IN.reg"))))
    ("CPU_ATSAMD51"
     ((PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
      (ANALOGREFERENCE AR_DEFAULT AR_INTERNAL1V0 AR_INTERNAL1V1 AR_INTERNAL1V2 AR_INTERNAL1V25 AR_INTERNAL1V65 AR_INTERNAL2V0
                       AR_INTERNAL2V2 AR_INTERNAL2V23 AR_INTERNAL2V4 AR_INTERNAL2V5 AR_EXTERNAL)
      (REGISTER (PA_DIR "PORT->Group[0].DIR.reg") (PA_DIRCLR "PORT->Group[0].DIRCLR.reg") (PA_DIRSET "PORT->Group[0].DIRSET.reg")
                (PA_DIRTGL "PORT->Group[0].DIRTGL.reg") (PA_OUT "PORT->Group[0].OUT.reg") (PA_OUTCLR "PORT->Group[0].OUTCLR.reg")
                (PA_OUTSET "PORT->Group[0].OUTSET.reg") (PA_OUTTGL "PORT->Group[0].OUTTGL.reg") (PA_IN "PORT->Group[0].IN.reg")
                (PB_DIR "PORT->Group[1].DIR.reg") (PB_DIRCLR "PORT->Group[1].DIRCLR.reg") (PB_DIRSET "PORT->Group[1].DIRSET.reg")
                (PB_DIRTGL "PORT->Group[1].DIRTGL.reg") (PB_OUT "PORT->Group[1].OUT.reg") (PB_OUTCLR "PORT->Group[1].OUTCLR.reg")
                (PB_OUTSET "PORT->Group[1].OUTSET.reg") (PB_OUTTGL "PORT->Group[1].OUTTGL.reg") (PB_IN "PORT->Group[1].IN.reg"))))
    ("CPU_NRF51822"
     ((PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
      (ANALOGREFERENCE AR_DEFAULT AR_VBG AR_SUPPLY_ONE_HALF AR_SUPPLY_ONE_THIRD AR_EXT0 AR_EXT1)
      (REGISTER (P0_OUT "NRF_GPIO->OUT") (P0_OUTSET "NRF_GPIO->OUTSET") (P0_OUTCLR "NRF_GPIO->OUTCLR") (P0_IN "NRF_GPIO->IN")
                (P0_DIR "NRF_GPIO->DIR") (P0_DIRSET "NRF_GPIO->DIRSET") (P0_DIRCLR "NRF_GPIO->DIRCLR"))))
    ("CPU_NRF52840"
     ((PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
      (ANALOGREFERENCE AR_DEFAULT AR_INTERNAL AR_INTERNAL_3_0 AR_INTERNAL_2_4 AR_INTERNAL_1_8 AR_INTERNAL_1_2 AR_VDD4)
      (REGISTER (P0_OUT "NRF_P0->OUT") (P0_OUTSET "NRF_P0->OUTSET") (P0_OUTCLR "NRF_P0->OUTCLR") (P0_IN "NRF_P0->IN")
                (P0_DIR "NRF_P0->DIR") (P0_DIRSET "NRF_P0->DIRSET") (P0_DIRCLR "NRF_P0->DIRCLR")
                (P1_OUT "NRF_P1->OUT") (P1_OUTSET "NRF_P1->OUTSET") (P1_OUTCLR "NRF_P1->OUTCLR") (P1_IN "NRF_P1->IN")
                (P1_DIR "NRF_P1->DIR") (P1_DIRSET "NRF_P1->DIRSET") (P1_DIRCLR "NRF_P1->DIRCLR"))))
    ("CPU_NRF52833"
     ((PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
      (ANALOGREFERENCE AR_DEFAULT AR_INTERNAL AR_VDD4)
      (REGISTER (P0_OUT "NRF_P0->OUT") (P0_OUTSET "NRF_P0->OUTSET") (P0_OUTCLR "NRF_P0->OUTCLR") (P0_IN "NRF_P0->IN")
                (P0_DIR "NRF_P0->DIR") (P0_DIRSET "NRF_P0->DIRSET") (P0_DIRCLR "NRF_P0->DIRCLR")
                (P1_OUT "NRF_P1->OUT") (P1_OUTSET "NRF_P1->OUTSET") (P1_OUTCLR "NRF_P1->OUTCLR") (P1_IN "NRF_P1->IN")
                (P1_DIR "NRF_P1->DIR") (P1_DIRSET "NRF_P1->DIRSET") (P1_DIRCLR "NRF_P1->DIRCLR"))))
    ("CPU_iMXRT1062"
     ((PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT OUTPUT_OPENDRAIN)))
    ("CPU_MAX32620"
     ((PINMODE INPUT INPUT_PULLUP OUTPUT)
      (ANALOGREFERENCE DEFAULT EXTERNAL)))
    ("CPU_RP2040"
     ((PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
      (REGISTER (GPIO_IN "(SIO_BASE+SIO_GPIO_IN_OFFSET)") (GPIO_OUT "(SIO_BASE+SIO_GPIO_OUT_OFFSET)")
                (GPIO_OUT_SET "(SIO_BASE+SIO_GPIO_OUT_SET_OFFSET)") (GPIO_OUT_CLR "(SIO_BASE+SIO_GPIO_OUT_CLR_OFFSET)")
                (GPIO_OUT_XOR "(SIO_BASE+SIO_GPIO_OUT_XOR_OFFSET)") (GPIO_OE "(SIO_BASE+SIO_GPIO_OE_OFFSET)")
                (GPIO_OE_SET "(SIO_BASE+SIO_GPIO_OE_SET_OFFSET)") (GPIO_OE_CLR "(SIO_BASE+SIO_GPIO_OE_CLR_OFFSET)")
                (GPIO_OE_XOR "(SIO_BASE+SIO_GPIO_OE_XOR_OFFSET)"))))))