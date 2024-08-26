TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

QMAKE_CFLAGS += -std=c99 -ffreestanding
QMAKE_CFLAGS += -Wall -Wextra
QMAKE_CFLAGS += -pedantic-errors
QMAKE_LFLAGS += -Xlinker -Map=output.map

SOURCES += \
	extra/errors.c \
	src/eei_defaults.c \
	src/eei_symboltable.c \
	src/eei_vm.c \
	src/emexpr.c \
	test/compare.c \
	test/main.c \
	test/test_sizes.c

HEADERS += \
	src/eei_rules.h \
	src/eei_symboltable.h \
	src/eei_vm.h \
	src/eelib/emexpr.h \
	extra/errors.h \
	src/emexpr.h
