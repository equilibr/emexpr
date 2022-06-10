TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

QMAKE_CFLAGS += -std=c99 -ffreestanding
QMAKE_CFLAGS += -Wall -Wextra
QMAKE_CFLAGS += -pedantic-errors

SOURCES += \
	src/emexpr.c \
	test/main.c

HEADERS += \
	src/emexpr.h
