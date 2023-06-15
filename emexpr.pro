TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

QMAKE_CFLAGS += -std=c99 -ffreestanding
QMAKE_CFLAGS += -Wall -Wextra
QMAKE_CFLAGS += -pedantic-errors

SOURCES += \
	extra/errors.c \
	src/emexpr.c \
	test/compare.c \
	test/main.c \
	test/test_sizes.c

HEADERS += \
	src/eelib/emexpr.h \
	extra/errors.h \
	src/emexpr.h
