CC = clang

TARGET = pdfkit-reader-module.dylib
SOURCE = pdfkit-reader-module.m

EMACS_HEADER_SEARCH_PATHS = \
	/Applications/Emacs.app/Contents/Resources/include \
	/opt/homebrew/include \
	/usr/local/include

ifdef EMACS_INCLUDE_DIR
  ifeq ($(wildcard $(EMACS_INCLUDE_DIR)/emacs-module.h),)
    $(error emacs-module.h not found in EMACS_INCLUDE_DIR="$(EMACS_INCLUDE_DIR)")
  endif
else
  EMACS_INCLUDE_DIR := $(firstword \
	$(foreach path,$(EMACS_HEADER_SEARCH_PATHS),\
		$(if $(wildcard $(path)/emacs-module.h),$(path),)))
  ifeq ($(EMACS_INCLUDE_DIR),)
    $(error emacs-module.h was not found; set EMACS_INCLUDE_DIR)
  endif
endif

ARCH = $(shell uname -m)
CFLAGS = -Wall -Wextra -Wpedantic -O2 -fPIC -fobjc-arc \
	-Wno-unused-parameter -I"$(EMACS_INCLUDE_DIR)" -arch $(ARCH)
LDFLAGS = -dynamiclib -framework Cocoa -framework PDFKit -arch $(ARCH)

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

clean:
	rm -f $(TARGET)
