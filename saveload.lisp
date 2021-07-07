;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

(defparameter *compactimage* '(

#"
// Compact image"#

#+(or avr badge)
#"
/*
  movepointer - corrects pointers to an object that has moved from 'from' to 'to'
*/
void movepointer (object *from, object *to) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    unsigned int type = (obj->type) & ~MARKBIT;
    if (marked(obj) && (type >= STRING || type==ZZERO || (type == SYMBOL && longsymbolp(obj)))) {
      if (car(obj) == (object *)((uintptr_t)from | MARKBIT))
        car(obj) = (object *)((uintptr_t)to | MARKBIT);
      if (cdr(obj) == from) cdr(obj) = to;
    }
  }
  // Fix strings and long symbols
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (marked(obj)) {
      unsigned int type = (obj->type) & ~MARKBIT;
      if (type == STRING || (type == SYMBOL && longsymbolp(obj))) {
        obj = cdr(obj);
        while (obj != NULL) {
          if (cdr(obj) == to) cdr(obj) = from;
          obj = (object *)((uintptr_t)(car(obj)) & ~MARKBIT);
        }
      }
    }
  }
}"#

#-avr
#"
/*
  movepointer - corrects pointers to an object that has moved from 'from' to 'to'
*/
void movepointer (object *from, object *to) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    unsigned int type = (obj->type) & ~MARKBIT;
    if (marked(obj) && (type >= ARRAY || type==ZZERO || (type == SYMBOL && longsymbolp(obj)))) {
      if (car(obj) == (object *)((uintptr_t)from | MARKBIT))
        car(obj) = (object *)((uintptr_t)to | MARKBIT);
      if (cdr(obj) == from) cdr(obj) = to;
    }
  }
  // Fix strings and long symbols
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (marked(obj)) {
      unsigned int type = (obj->type) & ~MARKBIT;
      if (type == STRING || (type == SYMBOL && longsymbolp(obj))) {
        obj = cdr(obj);
        while (obj != NULL) {
          if (cdr(obj) == to) cdr(obj) = from;
          obj = (object *)((uintptr_t)(car(obj)) & ~MARKBIT);
        }
      }
    }
  }
}"#

#"
/*
  compactimage - compacts the image by moving objects to the lowest possible position in the workspace
*/
uintptr_t compactimage (object **arg) {
  markobject(tee);
  markobject(GlobalEnv);
  markobject(GCStack);
  object *firstfree = Workspace;
  while (marked(firstfree)) firstfree++;
  object *obj = &Workspace[WORKSPACESIZE-1];
  while (firstfree < obj) {
    if (marked(obj)) {
      car(firstfree) = car(obj);
      cdr(firstfree) = cdr(obj);
      unmark(obj);
      movepointer(obj, firstfree);
      if (GlobalEnv == obj) GlobalEnv = firstfree;
      if (GCStack == obj) GCStack = firstfree;
      if (*arg == obj) *arg = firstfree;
      while (marked(firstfree)) firstfree++;
    }
    obj--;
  }
  sweep();
  return firstfree - Workspace;
}"#))

(defparameter *make-filename* '(

#-esp
#"
// Make SD card filename

char *MakeFilename (object *arg, char *buffer) {
  int max = BUFFERSIZE-1;
  int i = 0;
  do {
    char c = nthchar(arg, i);
    if (c == '\0') break;
    buffer[i++] = c;
  } while (i<max);
  buffer[i] = '\0';
  return buffer;
}"#

#+esp
#"
// Make SD card filename

char *MakeFilename (object *arg, char *buffer) {
  int max = BUFFERSIZE-1;
  buffer[0]='/';
  int i = 1;
  do {
    char c = nthchar(arg, i-1);
    if (c == '\0') break;
    buffer[i++] = c;
  } while (i<max);
  buffer[i] = '\0';
  return buffer;
}"#))

#+(and avr (not badge))
(defparameter *saveimage* #"
// Save-image and load-image

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
}

int SDReadInt (File file) {
  uint8_t b0 = file.read(); uint8_t b1 = file.read();
  return b0 | b1<<8;
}
#elif defined(FLASHWRITESIZE)
#if defined (CPU_ATmega1284P)
// save-image area is the 16K bytes (64 256-byte pages) from 0x1bc00 to 0x1fc00
const uint32_t BaseAddress = 0x1bc00;
uint8_t FlashCheck() {
  return 0;
}

void FlashWriteInt (uint32_t *addr, int data) {
  if (((*addr) & 0xFF) == 0) optiboot_page_erase(BaseAddress + ((*addr) & 0xFF00));
  optiboot_page_fill(BaseAddress + *addr, data);
  if (((*addr) & 0xFF) == 0xFE) optiboot_page_write(BaseAddress + ((*addr) & 0xFF00));
  (*addr)++; (*addr)++;
}

void FlashEndWrite (uint32_t *addr) {
  if (((*addr) & 0xFF) != 0) optiboot_page_write((BaseAddress + ((*addr) & 0xFF00)));
}

uint8_t FlashReadByte (uint32_t *addr) {
  return pgm_read_byte_far(BaseAddress + (*addr)++);
}

int FlashReadInt (uint32_t *addr) {
  int data = pgm_read_word_far(BaseAddress + *addr);
  (*addr)++; (*addr)++;
  return data;
}
#elif defined (CPU_AVR128DX48)
// save-image area is the 16K bytes (32 512-byte pages) from 0x1c000 to 0x20000
const uint32_t BaseAddress = 0x1c000;
uint8_t FlashCheck() {
  return Flash.checkWritable();
}

void FlashWriteInt (uint32_t *addr, int data) {
  if (((*addr) & 0x1FF) == 0) Flash.erasePage(BaseAddress + ((*addr) & 0xFE00));
  Flash.writeWord(BaseAddress + *addr, data);
  (*addr)++; (*addr)++;
}

void FlashEndWrite (uint32_t *addr) {
}

uint8_t FlashReadByte (uint32_t *addr) {
  return Flash.readByte(BaseAddress + (*addr)++);
}

int FlashReadInt (uint32_t *addr) {
  int data = Flash.readWord(BaseAddress + *addr);
  (*addr)++; (*addr)++;
  return data;
}
#endif
#else
void EEPROMWriteInt (unsigned int *addr, int data) {
  EEPROM.write((*addr)++, data & 0xFF); EEPROM.write((*addr)++, data>>8 & 0xFF);
}

int EEPROMReadInt (unsigned int *addr) {
  uint8_t b0 = EEPROM.read((*addr)++); uint8_t b1 = EEPROM.read((*addr)++);
  return b0 | b1<<8;
}
#endif

unsigned int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    char buffer[BUFFERSIZE];
    file = SD.open(MakeFilename(arg, buffer), O_RDWR | O_CREAT | O_TRUNC);
    if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card or invalid filename"));
    arg = NULL;
  } else if (arg == NULL || listp(arg)) {
    file = SD.open("/ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
    if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  }
  else error(SAVEIMAGE, invalidarg, arg);
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) file.write(MyCode[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(FLASHWRITESIZE)
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  if (FlashCheck()) error2(SAVEIMAGE, PSTR("flash write not supported"));
  // Save to Flash
  int bytesneeded = 10 + CODESIZE + imagesize*4;
  if (bytesneeded > FLASHWRITESIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  uint32_t addr = 0;
  FlashWriteInt(&addr, (uintptr_t)arg);
  FlashWriteInt(&addr, imagesize);
  FlashWriteInt(&addr, (uintptr_t)GlobalEnv);
  FlashWriteInt(&addr, (uintptr_t)GCStack);
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE/2; i++) FlashWriteInt(&addr, MyCode[i*2] | MyCode[i*2+1]<<8);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWriteInt(&addr, (uintptr_t)car(obj));
    FlashWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  FlashEndWrite(&addr);
  return imagesize;
#else
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  int bytesneeded = imagesize*4 + 10;
  if (bytesneeded > EEPROMSIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  unsigned int addr = 0;
  EEPROMWriteInt(&addr, (unsigned int)arg);
  EEPROMWriteInt(&addr, imagesize);
  EEPROMWriteInt(&addr, (unsigned int)GlobalEnv);
  EEPROMWriteInt(&addr, (unsigned int)GCStack);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    EEPROMWriteInt(&addr, (uintptr_t)car(obj));
    EEPROMWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  return imagesize;
#endif
}

unsigned int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    char buffer[BUFFERSIZE];
    file = SD.open(MakeFilename(arg, buffer));
    if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card or invalid filename"));
  }
  else if (arg == NULL) {
    file = SD.open("/ULISP.IMG");
    if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  }
  else error(LOADIMAGE, invalidarg, arg);
  SDReadInt(file);
  unsigned int imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) MyCode[i] = file.read();
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(FLASHWRITESIZE)
  if (FlashCheck()) error2(SAVEIMAGE, PSTR("flash write not supported"));
  uint32_t addr = 0;
  FlashReadInt(&addr); // Skip eval address
  unsigned int imagesize = FlashReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashReadInt(&addr);
  GCStack = (object *)FlashReadInt(&addr);
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) MyCode[i] = FlashReadByte(&addr);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashReadInt(&addr);
    cdr(obj) = (object *)FlashReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
#else
  unsigned int addr = 2; // Skip eval address
  unsigned int imagesize = EEPROMReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)EEPROMReadInt(&addr);
  GCStack = (object *)EEPROMReadInt(&addr);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)EEPROMReadInt(&addr);
    cdr(obj) = (object *)EEPROMReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error2(NIL, PSTR("problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#elif defined(FLASHWRITESIZE)
  uint32_t addr = 0;
  object *autorun = (object *)FlashReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(NIL, autorun, NULL, NULL);
  }
#else
  unsigned int addr = 0;
  object *autorun = (object *)EEPROMReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(NIL, autorun, NULL, NULL);
  }
#endif
}"#)

#+badge
(defparameter *saveimage* #"
// Save-image and load-image

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
}
#elif defined(FLASHWRITESIZE)
// save-image area is the 16K bytes (64 256-byte pages) from 0x1bc00 to 0x1fc00
const uint32_t BaseAddress = 0x1bc00;
uint8_t FlashCheck() {
  return 0;
}

void FlashWriteInt (uint32_t *addr, int data) {
  if (((*addr) & 0xFF) == 0) optiboot_page_erase(BaseAddress + ((*addr) & 0xFF00));
  optiboot_page_fill(BaseAddress + *addr, data);
  if (((*addr) & 0xFF) == 0xFE) optiboot_page_write(BaseAddress + ((*addr) & 0xFF00));
  (*addr)++; (*addr)++;
}

void FlashEndWrite (uint32_t *addr) {
  if (((*addr) & 0xFF) != 0) optiboot_page_write((BaseAddress + ((*addr) & 0xFF00)));
}
#else
void EEPROMWriteInt (unsigned int *addr, int data) {
  EEPROM.write((*addr)++, data & 0xFF); EEPROM.write((*addr)++, data>>8 & 0xFF);
}
#endif

unsigned int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), O_RDWR | O_CREAT | O_TRUNC);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("/ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
  else error(SAVEIMAGE, invalidarg, arg);
  if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SDWriteInt(file, (uintptr_t)SymbolTop);
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) file.write(SymbolTable[i]);
  #endif
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) file.write(MyCode[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(FLASHWRITESIZE)
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  if (FlashCheck()) error2(SAVEIMAGE, PSTR("flash write not supported"));
  // Save to Flash
  int SymbolUsed = SymbolTop - SymbolTable;
  int bytesneeded = 10 + SYMBOLTABLESIZE + CODESIZE + imagesize*4;
  if (bytesneeded > FLASHWRITESIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  uint32_t addr = 0;
  FlashWriteInt(&addr, (uintptr_t)arg);
  FlashWriteInt(&addr, imagesize);
  FlashWriteInt(&addr, (uintptr_t)GlobalEnv);
  FlashWriteInt(&addr, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  FlashWriteInt(&addr, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE/2; i++) FlashWriteInt(&addr, SymbolTable[i*2] | SymbolTable[i*2+1]<<8);
  #endif
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE/2; i++) FlashWriteInt(&addr, MyCode[i*2] | MyCode[i*2+1]<<8);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWriteInt(&addr, (uintptr_t)car(obj));
    FlashWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  FlashEndWrite(&addr);
  return imagesize;
#else
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  int SymbolUsed = SymbolTop - SymbolTable;
  int bytesneeded = imagesize*4 + SymbolUsed + 10;
  if (bytesneeded > EEPROMSIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  unsigned int addr = 0;
  EEPROMWriteInt(&addr, (unsigned int)arg);
  EEPROMWriteInt(&addr, imagesize);
  EEPROMWriteInt(&addr, (unsigned int)GlobalEnv);
  EEPROMWriteInt(&addr, (unsigned int)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  EEPROMWriteInt(&addr, (unsigned int)SymbolTop);
  for (int i=0; i<SymbolUsed; i++) EEPROM.write(addr++, SymbolTable[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    EEPROMWriteInt(&addr, (uintptr_t)car(obj));
    EEPROMWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  return imagesize;
#endif
}

#if defined(sdcardsupport)
int SDReadInt (File file) {
  uint8_t b0 = file.read(); uint8_t b1 = file.read();
  return b0 | b1<<8;
}
#elif defined(FLASHWRITESIZE)
uint8_t FlashReadByte (uint32_t *addr) {
  return pgm_read_byte_far(BaseAddress + (*addr)++);
}

int FlashReadInt (uint32_t *addr) {
  int data = pgm_read_word_far(BaseAddress + *addr);
  (*addr)++; (*addr)++;
  return data;
}
#else
int EEPROMReadInt (unsigned int *addr) {
  uint8_t b0 = EEPROM.read((*addr)++); uint8_t b1 = EEPROM.read((*addr)++);
  return b0 | b1<<8;
}
#endif

unsigned int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("/ULISP.IMG");
  else error(LOADIMAGE, invalidarg, arg);
  if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  SDReadInt(file);
  unsigned int imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)SDReadInt(file);
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) SymbolTable[i] = file.read();
  #endif
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) MyCode[i] = file.read();
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(FLASHWRITESIZE)
  if (FlashCheck()) error2(SAVEIMAGE, PSTR("flash write not supported"));
  uint32_t addr = 0;
  FlashReadInt(&addr); // Skip eval address
  unsigned int imagesize = FlashReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashReadInt(&addr);
  GCStack = (object *)FlashReadInt(&addr);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)FlashReadInt(&addr);
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = FlashReadByte(&addr);
  #endif
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) MyCode[i] = FlashReadByte(&addr);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashReadInt(&addr);
    cdr(obj) = (object *)FlashReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
#else
  unsigned int addr = 2; // Skip eval address
  unsigned int imagesize = EEPROMReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)EEPROMReadInt(&addr);
  GCStack = (object *)EEPROMReadInt(&addr);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)EEPROMReadInt(&addr);
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) SymbolTable[i] = EEPROM.read(addr++);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)EEPROMReadInt(&addr);
    cdr(obj) = (object *)EEPROMReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error2(NIL, PSTR("problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#elif defined(FLASHWRITESIZE)
  uint32_t addr = 0;
  object *autorun = (object *)FlashReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(0, autorun, NULL, NULL);
  }
#else
  unsigned int addr = 0;
  object *autorun = (object *)EEPROMReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(0, autorun, NULL, NULL);
  }
#endif
}"#)

#+arm
(defparameter *saveimage* #"
// Save-image and load-image

#if defined(sdcardsupport)
void SDWrite32 (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
  file.write(data>>16 & 0xFF); file.write(data>>24 & 0xFF);
}

int SDRead32 (File file) {
  uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
  uintptr_t b2 = file.read(); uintptr_t b3 = file.read();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#elif defined(DATAFLASH)
// Winbond DataFlash support for Adafruit M4 Express boards
#define PAGEPROG      0x02
#define READSTATUS    0x05
#define READDATA      0x03
#define WRITEENABLE   0x06
#define BLOCK64K      0xD8
#define READID        0x90

// Arduino pins used for dataflash
#if defined(ARDUINO_ITSYBITSY_M0) || defined(ARDUINO_SAMD_FEATHER_M0_EXPRESS)
const int sck = 38, ssel = 39, mosi = 37, miso = 36;
#elif defined(EXTERNAL_FLASH_USE_QSPI)
const int sck = PIN_QSPI_SCK, ssel = PIN_QSPI_CS, mosi = PIN_QSPI_IO0, miso = PIN_QSPI_IO1;
#endif

void FlashBusy () {
  digitalWrite(ssel, 0);
  FlashWrite(READSTATUS);
  while (FlashReadByte() & 1 != 0);
  digitalWrite(ssel, 1);
}

inline void FlashWrite (uint8_t data) {
  shiftOut(mosi, sck, MSBFIRST, data);
}

inline uint8_t FlashReadByte () {
  return shiftIn(miso, sck, MSBFIRST);
}

void FlashWriteByte (uint32_t *addr, uint8_t data) {
  // New page
  if (((*addr) & 0xFF) == 0) {
    digitalWrite(ssel, 1);
    FlashBusy();
    FlashWriteEnable();
    digitalWrite(ssel, 0);
    FlashWrite(PAGEPROG);
    FlashWrite((*addr)>>16);
    FlashWrite((*addr)>>8);
    FlashWrite(0);
  }
  FlashWrite(data);
  (*addr)++;
}

void FlashWriteEnable () {
  digitalWrite(ssel, 0);
  FlashWrite(WRITEENABLE);
  digitalWrite(ssel, 1);
}

bool FlashCheck () {
  uint8_t manID, devID;
  digitalWrite(ssel, HIGH); pinMode(ssel, OUTPUT);
  pinMode(sck, OUTPUT);
  pinMode(mosi, OUTPUT);
  pinMode(miso, INPUT);
  digitalWrite(sck, LOW); digitalWrite(mosi, HIGH);
  digitalWrite(ssel, LOW);
  FlashWrite(READID);
  for(uint8_t i=0; i<4; i++) manID = FlashReadByte();
  devID = FlashReadByte();
  digitalWrite(ssel, HIGH);
  return (devID == 0x14 || devID == 0x15 || devID == 0x16); // true = found correct device
}

void FlashBeginWrite (uint32_t *addr, uint32_t bytes) {
  *addr = 0;
  uint32_t blocks = (bytes+65535)/65536;
  // Erase 64K
  for (int b=0; b<blocks; b++) {
    FlashWriteEnable();
    digitalWrite(ssel, 0);
    FlashWrite(BLOCK64K);
    FlashWrite(b); FlashWrite(0); FlashWrite(0);
    digitalWrite(ssel, 1);
    FlashBusy();
  }
}

void FlashWrite32 (uint32_t *addr, uint32_t data) {
  FlashWriteByte(addr, data & 0xFF); FlashWriteByte(addr, data>>8 & 0xFF);
  FlashWriteByte(addr, data>>16 & 0xFF); FlashWriteByte(addr, data>>24 & 0xFF);
}

inline void FlashEndWrite (uint32_t *addr) {
  (void) addr;
  digitalWrite(ssel, 1);
  FlashBusy();
}

void FlashBeginRead (uint32_t *addr) {
  *addr = 0;
  FlashBusy();
  digitalWrite(ssel, 0);
  FlashWrite(READDATA);
  FlashWrite(0); FlashWrite(0); FlashWrite(0);
}

uint32_t FlashRead32 (uint32_t *addr) {
  (void) addr;
  uint8_t b0 = FlashReadByte(); uint8_t b1 = FlashReadByte();
  uint8_t b2 = FlashReadByte(); uint8_t b3 = FlashReadByte();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}

inline void FlashEndRead(uint32_t *addr) {
  (void) addr;
  digitalWrite(ssel, 1);
}

#elif defined(EEPROMFLASH)
// For ATSAMD21
__attribute__((__aligned__(256))) static const uint8_t flash_store[FLASHSIZE] = { };

void row_erase (const volatile void *addr) {
  NVMCTRL->ADDR.reg = ((uint32_t)addr) / 2;
  NVMCTRL->CTRLA.reg = NVMCTRL_CTRLA_CMDEX_KEY | NVMCTRL_CTRLA_CMD_ER;
  while (!NVMCTRL->INTFLAG.bit.READY);
}

void page_clear () {
  // Execute "PBC" Page Buffer Clear
  NVMCTRL->CTRLA.reg = NVMCTRL_CTRLA_CMDEX_KEY | NVMCTRL_CTRLA_CMD_PBC;
  while (NVMCTRL->INTFLAG.bit.READY == 0);
}

void page_write () {
  NVMCTRL->CTRLA.reg = NVMCTRL_CTRLA_CMDEX_KEY | NVMCTRL_CTRLA_CMD_WP;
  while (NVMCTRL->INTFLAG.bit.READY == 0);
}

bool FlashCheck() {
  return true;
}

void FlashBeginWrite(uint32_t *addr, uint32_t bytes) {
  (void) bytes;
  *addr = (uint32_t)flash_store;
  // Disable automatic page write
  NVMCTRL->CTRLB.bit.MANW = 1;
}

void FlashWrite32 (uint32_t *addr, uint32_t data) {
  if (((*addr) & 0xFF) == 0) row_erase((const volatile void *)(*addr));
  if (((*addr) & 0x3F) == 0) page_clear();
  *(volatile uint32_t *)(*addr) = data;
  (*addr) = (*addr) + 4;
  if (((*addr) & 0x3F) == 0) page_write();
}

void FlashEndWrite (uint32_t *addr) {
  if (((*addr) & 0x3F) != 0) page_write();
}

void FlashBeginRead(uint32_t *addr) {
  *addr = (uint32_t)flash_store;
}

uint32_t FlashRead32 (uint32_t *addr) {
  uint32_t data = *(volatile const uint32_t *)(*addr);
  (*addr) = (*addr) + 4;
  return data;
}

void FlashEndRead (uint32_t *addr) {
}
#elif defined(BLOCKDEVICE)
// For Raspberry Pi Pico and RP2040 boards
#include "FlashIAPBlockDevice.h"
#include "KVStore.h"
#include "TDBStore.h"

// 512KB block device, starting 1MB inside the flash
FlashIAPBlockDevice bd(XIP_BASE + 1024*1024, 1024*512);
mbed::TDBStore eeprom(&bd);
#endif

int saveimage (object *arg) {
#if defined(sdcardsupport)
  unsigned int imagesize = compactimage(&arg);
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    char buffer[BUFFERSIZE];
    file = SD.open(MakeFilename(arg, buffer), O_RDWR | O_CREAT | O_TRUNC);
    if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card or invalid filename"));
    arg = NULL;
  } else if (arg == NULL || listp(arg)) {
    file = SD.open("/ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
    if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  }
  else error(SAVEIMAGE, invalidarg, arg);
  SDWrite32(file, (uintptr_t)arg);
  SDWrite32(file, imagesize);
  SDWrite32(file, (uintptr_t)GlobalEnv);
  SDWrite32(file, (uintptr_t)GCStack);
  for (int i=0; i<CODESIZE; i++) file.write(MyCode[i]);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWrite32(file, (uintptr_t)car(obj));
    SDWrite32(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(DATAFLASH) || defined(EEPROMFLASH)
  unsigned int imagesize = compactimage(&arg);
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  if (!FlashCheck()) error2(SAVEIMAGE, PSTR("flash not available"));
  // Save to flash
  uint32_t bytesneeded = 16 + CODESIZE + imagesize*8;
  if (bytesneeded > FLASHSIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  uint32_t addr;
  FlashBeginWrite(&addr, bytesneeded);
  FlashWrite32(&addr, (uintptr_t)arg);
  FlashWrite32(&addr, imagesize);
  FlashWrite32(&addr, (uintptr_t)GlobalEnv);
  FlashWrite32(&addr, (uintptr_t)GCStack);
  for (int i=0; i<CODESIZE; i=i+4) {
    union { uint32_t u32; uint8_t u8[4]; };
    u8[0] = MyCode[i]; u8[1] = MyCode[i+1]; u8[2] = MyCode[i+2]; u8[3] = MyCode[i+3];
    FlashWrite32(&addr, u32);
  }
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWrite32(&addr, (uintptr_t)car(obj));
    FlashWrite32(&addr, (uintptr_t)cdr(obj));
  }
  FlashEndWrite(&addr);
  return imagesize;
#elif defined(BLOCKDEVICE)
  uint32_t imagesize = compactimage(&arg);
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  if (eeprom.init() != MBED_SUCCESS) error2(SAVEIMAGE, PSTR("block device not available"));
  if (eeprom.reset() != MBED_SUCCESS) error2(SAVEIMAGE, PSTR("block device error"));
  // Save to flash
  uint32_t bytesneeded = 20 + CODESIZE + imagesize*8;
  if (bytesneeded > FLASHSIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  eeprom.set("eval", &arg, 4, 0);
  eeprom.set("size", &imagesize, 4, 0);
  eeprom.set("genv", &GlobalEnv, 4, 0);
  eeprom.set("gstk", &GCStack, 4, 0);
  eeprom.set("code", &MyCode, CODESIZE, 0);
  eeprom.set("work", &Workspace, WORKSPACESIZE*4, 0);
  return imagesize;
#else
  (void) arg;
  error2(SAVEIMAGE, PSTR("not available"));
  return 0;
#endif
}

int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    char buffer[BUFFERSIZE];
    file = SD.open(MakeFilename(arg, buffer));
    if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card or invalid filename"));
  }
  else if (arg == NULL) {
    file = SD.open("/ULISP.IMG");
    if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  }
  else error(LOADIMAGE, invalidarg, arg);
  SDRead32(file);
  unsigned int imagesize = SDRead32(file);
  GlobalEnv = (object *)SDRead32(file);
  GCStack = (object *)SDRead32(file);
  for (int i=0; i<CODESIZE; i++) MyCode[i] = file.read();
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDRead32(file);
    cdr(obj) = (object *)SDRead32(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(DATAFLASH) || defined(EEPROMFLASH)
  if (!FlashCheck()) error2(LOADIMAGE, PSTR("flash not available"));
  uint32_t addr;
  FlashBeginRead(&addr);
  FlashRead32(&addr); // Skip eval address
  uint32_t imagesize = FlashRead32(&addr);
  if (imagesize == 0 || imagesize == 0xFFFFFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashRead32(&addr);
  GCStack = (object *)FlashRead32(&addr);
  for (int i=0; i<CODESIZE; i=i+4) {
    union { uint32_t u32; uint8_t u8[4]; };
    u32 = FlashRead32(&addr);
    MyCode[i] = u8[0]; MyCode[i+1] = u8[1]; MyCode[i+2] = u8[2]; MyCode[i+3] = u8[3];
  }
  for (uint32_t i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashRead32(&addr);
    cdr(obj) = (object *)FlashRead32(&addr);
  }
  FlashEndRead(&addr);
  gc(NULL, NULL);
  return imagesize;
#elif defined(BLOCKDEVICE)
  if (eeprom.init() != MBED_SUCCESS) error2(SAVEIMAGE, PSTR("block device not available"));
  uint32_t imagesize;
  if (eeprom.get("size", &imagesize, 4) != MBED_SUCCESS) error2(LOADIMAGE, PSTR("no saved image"));
  eeprom.get("genv", &GlobalEnv, 4);
  eeprom.get("gstk", &GCStack, 4);
  eeprom.get("code", &MyCode, CODESIZE);
  eeprom.get("work", &Workspace, WORKSPACESIZE*4);
  gc(NULL, NULL);
  return imagesize;
#else
  (void) arg;
  error2(LOADIMAGE, PSTR("not available"));
  return 0;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error2(NIL, PSTR("problem autorunning from SD card"));
  object *autorun = (object *)SDRead32(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(NIL, autorun, NULL, NULL);
  }
#elif defined(DATAFLASH) || defined(EEPROMFLASH)
  if (!FlashCheck()) error2(NIL, PSTR("flash not available"));
  uint32_t addr;
  FlashBeginRead(&addr);
  object *autorun = (object *)FlashRead32(&addr);
  FlashEndRead(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFFFFFF) {
    loadimage(nil);
    apply(NIL, autorun, NULL, NULL);
  }
#elif defined(BLOCKDEVICE)
  if (eeprom.init() != MBED_SUCCESS) error2(SAVEIMAGE, PSTR("block device not available"));
  object *autorun;
  eeprom.get("eval", &autorun, 4);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFFFFFF) {
    loadimage(nil);
    apply(NIL, autorun, NULL, NULL);
  }
#else
  error2(NIL, PSTR("autorun not available"));
#endif
}"#)

#+riscv
(defparameter *saveimage* #"
// Save-image and load-image

typedef union {
  uintptr_t sdpointer;
  uint8_t sdbyte[8];
} sdbuffer_t;

#if defined(sdcardsupport)
void SDWriteInt (File file, uintptr_t data) {
  sdbuffer_t sdbuf;
  sdbuf.sdpointer = data;
  for (int i=0; i<8; i++) file.write(sdbuf.sdbyte[i]);
}
#endif

int saveimage (object *arg) {
#if defined(sdcardsupport)
  uintptr_t imagesize = compactimage(&arg);
  if (!SD.begin(SDCARD_SS_PIN)) error2(SAVEIMAGE, PSTR("problem initialising SD card"));
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), FILE_WRITE);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
  else error(SAVEIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, (uintptr_t)imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SDWriteInt(file, (uintptr_t)SymbolTop);
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) file.write(SymbolTable[i]);
  #endif
  for (int i=0; i<CODESIZE; i++) file.write(MyCode[i]);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#else
  (void) arg;
  error2(SAVEIMAGE, PSTR("not available"));
  return 0;
#endif
}

#if defined(sdcardsupport)
uintptr_t SDReadInt (File file) {
  sdbuffer_t sdbuf;
  for (int i=0; i<8; i++) sdbuf.sdbyte[i] = file.read();
  return sdbuf.sdpointer;
}
#endif

int loadimage (object *arg) {
#if defined(sdcardsupport)
  if (!SD.begin(SDCARD_SS_PIN)) error2(LOADIMAGE, PSTR("problem initialising SD card"));
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("ULISP.IMG");
  else error(LOADIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  SDReadInt(file);
  uintptr_t imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)SDReadInt(file);
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) SymbolTable[i] = file.read();
  #endif
  for (int i=0; i<CODESIZE; i++) MyCode[i] = file.read();
  for (int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#else
  (void) arg;
  error2(LOADIMAGE, PSTR("not available"));
  return 0;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error2(NIL, PSTR("problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#else
  error2(NIL, PSTR("autorun not available"));
#endif
}"#)

#+esp
(defparameter *saveimage* #"
// Save-image and load-image

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
  file.write(data>>16 & 0xFF); file.write(data>>24 & 0xFF);
}

int SDReadInt (File file) {
  uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
  uintptr_t b2 = file.read(); uintptr_t b3 = file.read();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#else
void EpromWriteInt(int *addr, uintptr_t data) {
  EEPROM.write((*addr)++, data & 0xFF); EEPROM.write((*addr)++, data>>8 & 0xFF);
  EEPROM.write((*addr)++, data>>16 & 0xFF); EEPROM.write((*addr)++, data>>24 & 0xFF);
}

int EpromReadInt (int *addr) {
  uint8_t b0 = EEPROM.read((*addr)++); uint8_t b1 = EEPROM.read((*addr)++);
  uint8_t b2 = EEPROM.read((*addr)++); uint8_t b3 = EEPROM.read((*addr)++);
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#endif

unsigned int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), FILE_WRITE);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("/ULISP.IMG", FILE_WRITE);
  else error(SAVEIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#else
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, PSTR("illegal argument"), arg);
  int bytesneeded = imagesize*8 + 36;
  if (bytesneeded > EEPROMSIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  EEPROM.begin(EEPROMSIZE);
  int addr = 0;
  EpromWriteInt(&addr, (uintptr_t)arg);
  EpromWriteInt(&addr, imagesize);
  EpromWriteInt(&addr, (uintptr_t)GlobalEnv);
  EpromWriteInt(&addr, (uintptr_t)GCStack);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    EpromWriteInt(&addr, (uintptr_t)car(obj));
    EpromWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  EEPROM.commit();
  return imagesize;
#endif
}

unsigned int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("/ULISP.IMG");
  else error(LOADIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  SDReadInt(file);
  unsigned int imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#else
  EEPROM.begin(EEPROMSIZE);
  int addr = 0;
  EpromReadInt(&addr); // Skip eval address
  unsigned int imagesize = EpromReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFFFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)EpromReadInt(&addr);
  GCStack = (object *)EpromReadInt(&addr);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)EpromReadInt(&addr);
    cdr(obj) = (object *)EpromReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("/ULISP.IMG");
  if (!file) error2(NIL, PSTR("problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#else
  EEPROM.begin(EEPROMSIZE);
  int addr = 0;
  object *autorun = (object *)EpromReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#endif
}"#)

#+stm32
(defparameter *saveimage* #"
// Save-image and load-image
const unsigned int Eeprom = 0x801D800;

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
  file.write(data>>16 & 0xFF); file.write(data>>24 & 0xFF);
}
#else
void FlashSetup () {
  FLASH_Unlock();
  uint16_t Status;
  for (int page = Eeprom; page < 0x8020000; page = page + 0x400) {
    Status = FLASH_ErasePage(page);
    if (Status != FLASH_COMPLETE) error2(SAVEIMAGE, PSTR("flash erase failed"));
  }
}

void FlashWrite16 (unsigned int *addr, uint16_t data) {
  uint16_t Status = FLASH_ProgramHalfWord((*addr) + Eeprom, data);
  if (Status != FLASH_COMPLETE) error2(SAVEIMAGE, PSTR("flash write failed"));
  (*addr) = (*addr) + 2;
}

void FlashWriteInt (unsigned int *addr, int data) {
  FlashWrite16(addr, data & 0xFFFF); FlashWrite16(addr, data>>16 & 0xFFFF);
}
#endif

int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), O_RDWR | O_CREAT | O_TRUNC);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
  else error3(SAVEIMAGE, PSTR("illegal argument"));
  if (!file) error(PSTR("Problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SDWriteInt(file, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE; i++) file.write(SymbolTable[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#else
  FlashSetup();
  // Save to EEPROM
  int bytesneeded = imagesize*8 + SYMBOLTABLESIZE + 20;
  if (bytesneeded > EEPROMSIZE) {
    pfstring(PSTR("Error: image too large: "), pserial);
    pint(imagesize, pserial); pln(pserial);
    GCStack = NULL;
    longjmp(exception, 1);
  }
  unsigned int addr = 0;
  FlashWriteInt(&addr, (uintptr_t)arg);
  FlashWriteInt(&addr, imagesize);
  FlashWriteInt(&addr, (uintptr_t)GlobalEnv);
  FlashWriteInt(&addr, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  FlashWriteInt(&addr, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE; i=i+2) FlashWrite16(&addr, SymbolTable[i] | SymbolTable[i+1]<<8);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWriteInt(&addr, (uintptr_t)car(obj));
    FlashWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  return imagesize; 
}
#endif

#if defined(sdcardsupport)
int SDReadInt (File file) {
  uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
  uintptr_t b2 = file.read(); uintptr_t b3 = file.read();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#else
uint16_t FlashRead16 (unsigned int *addr) {
  uint16_t data = (*(__IO uint16*)((*addr) + Eeprom));
  (*addr) = (*addr) + 2;
  return data;
}

int FlashReadInt (unsigned int *addr) {
  uint16_t b0 = FlashRead16(addr);
  uint16_t b1 = FlashRead16(addr);
  return b0 | b1<<16;
}
#endif

int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("/ULISP.IMG");
  else error3(LOADIMAGE, PSTR("illegal argument"));
  if (!file) error(PSTR("Problem loading from SD card"));
  SDReadInt(file);
  unsigned int imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)SDReadInt(file);
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = file.read();
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#else
  unsigned int addr = 0;
  FlashReadInt(&addr); // Skip eval address
  unsigned int imagesize = FlashReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFFFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashReadInt(&addr);
  GCStack = (object *)FlashReadInt(&addr);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)FlashReadInt(&addr);
  for (int i=0; i<SYMBOLTABLESIZE; i=i+2) {
    uint16_t bytes = FlashRead16(&addr);
    SymbolTable[i] = bytes & 0xFF;
    SymbolTable[i+1] = bytes>>8 & 0xFF;
  }
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashReadInt(&addr);
    cdr(obj) = (object *)FlashReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error(PSTR("Error: Problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#else
  unsigned int addr = 0;
  object *autorun = (object *)FlashReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFFFFFF) {
    loadimage(nil);
    apply(0, autorun, NULL, NULL);
  }
#endif
}"#)

#+msp430
(defparameter *saveimage* #"
// Save-image and load-image

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
}
#elif defined(__MSP430F5529__)
#include "MspFlash.h"
const int segmentsize = 0x200;                // 512
unsigned char image[13*segmentsize] PERSIST;  // We need 12*512 in the middle of this
#define FLASH SEGPTR(image)
#elif defined(__MSP430FR5969__) || defined(__MSP430FR5994__) || defined(__MSP430FR6989__)
struct image_struct {
  object *eval;
  unsigned int datasize;
  object *globalenv;
  object *gcstack;
  #if SYMBOLTABLESIZE > BUFFERSIZE
  char *symboltop;
  char table[SYMBOLTABLESIZE];
  #endif
  object data[IMAGEDATASIZE];
};

struct image_struct image PERSIST;
#endif

int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), O_RDWR | O_CREAT | O_TRUNC);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("/ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
  else error3(SAVEIMAGE, PSTR("illegal argument"));
  if (!file) error(PSTR("Problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SDWriteInt(file, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE; i++) file.write(SymbolTable[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(__MSP430F5529__)
  if (!(arg == NULL || listp(arg))) error3(SAVEIMAGE, PSTR(" illegal argument"));
  int bytesneeded = imagesize*4 + SYMBOLTABLESIZE + 10;
  if (imagesize > IMAGEDATASIZE) {
    pfstring(PSTR("Error: image too large: "), pserial);
    pint(imagesize, pserial); pln(pserial);
    GCStack = NULL;
    longjmp(exception, 1);
  }
  // Erase flash
  for (int i=0; i<12; i++) Flash.erase(FLASH + i*segmentsize);
  unsigned char *workstart = FLASH+8;
  Flash.write(FLASH, (unsigned char*)&imagesize, 2);
  Flash.write(FLASH+2, (unsigned char*)&arg, 2);
  Flash.write(FLASH+4, (unsigned char*)&GlobalEnv, 2);
  Flash.write(FLASH+6, (unsigned char*)&GCStack, 2);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  Flash.write(FLASH+8, (unsigned char*)&SymbolTop, 2);
  Flash.write(FLASH+10, (unsigned char*)SymbolTable, SYMBOLTABLESIZE);
  workstart = FLASH + SYMBOLTABLESIZE + 10;
  #endif
  Flash.write(workstart, (unsigned char*)Workspace, imagesize*4);
  return imagesize;
#elif defined(__MSP430FR5969__) || defined(__MSP430FR5994__) || defined(__MSP430FR6989__)
  if (!(arg == NULL || listp(arg))) error3(SAVEIMAGE, PSTR(" illegal argument"));
  int bytesneeded = imagesize*4 + SYMBOLTABLESIZE + 10;
  if (imagesize > IMAGEDATASIZE) {
    pfstring(PSTR("Error: image too large: "), pserial);
    pint(imagesize, pserial); pln(pserial);
    GCStack = NULL;
    longjmp(exception, 1);
  }
  image.datasize = imagesize;
  image.eval = arg;
  image.globalenv = GlobalEnv;
  image.gcstack = GCStack;
  #if SYMBOLTABLESIZE > BUFFERSIZE
  image.symboltop = SymbolTop;
  for (int i=0; i<SYMBOLTABLESIZE; i++) image.table[i] = SymbolTable[i];
  #endif
  for (unsigned int i=0; i<imagesize; i++) image.data[i] = Workspace[i];
  return imagesize;
#endif
}

#if defined(sdcardsupport)
int SDReadInt (File file) {
  uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
  return b0 | b1<<8;
}
#endif

int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("/ULISP.IMG");
  else error3(LOADIMAGE, PSTR("illegal argument"));
  if (!file) error(PSTR("Problem loading from SD card"));
  SDReadInt(file);
  unsigned int imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)SDReadInt(file);
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = file.read();
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(__MSP430F5529__)
  unsigned int imagesize;
  unsigned char *workstart = FLASH+8;
  Flash.read(FLASH, (unsigned char*)&imagesize, 2);
  Flash.read(FLASH+4, (unsigned char*)&GlobalEnv, 2);
  Flash.read(FLASH+6, (unsigned char*)&GCStack, 2);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  Flash.read(FLASH+8, (unsigned char*)&SymbolTop, 2);
  Flash.read(FLASH+10, (unsigned char*)SymbolTable, SYMBOLTABLESIZE);
  workstart = FLASH + SYMBOLTABLESIZE + 10;
  #endif
  Flash.read(workstart, (unsigned char*)Workspace, imagesize*4);
  gc(NULL, NULL);
  return imagesize;
#elif defined(__MSP430FR5969__) || defined(__MSP430FR5994__) || defined(__MSP430FR6989__)
  unsigned int imagesize;
  imagesize = image.datasize;
  GlobalEnv = image.globalenv;
  GCStack = image.gcstack;
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = image.symboltop;
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = image.table[i];
  #endif
  for (unsigned int i=0; i<imagesize; i++) Workspace[i] = image.data[i];
  gc(NULL, NULL);
  return imagesize;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error(PSTR("Problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(autorun, NULL, NULL);
  }
#elif defined(__MSP430F5529__)
  object *autorun;
  Flash.read(FLASH+2, (unsigned char*)&autorun, 2);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(autorun, NULL, NULL);
  }
#elif defined(__MSP430FR5969__) || defined(__MSP430FR5994__) || defined(__MSP430FR6989__)
  object *autorun = image.eval;
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(autorun, NULL, NULL);
  }
#endif
}"#)
        