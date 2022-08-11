TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

QMAKE_CFLAGS += -std=c99 -ffreestanding
QMAKE_CFLAGS += -Wall -Wextra
QMAKE_CFLAGS += -pedantic-errors

SOURCES += \
	src/eelib/errors.c \
	src/emexpr.c \
	test/compare.c \
	test/main.c

HEADERS += \
	src/eelib/emexpr.h \
	src/eelib/errors.h \
	src/emexpr.h
