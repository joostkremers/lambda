CC = cc
CFLAGS = -std=c99 -Wall -g
LFLAGS = 
FILES = lambda
PLATFORM = $(shell uname)

ifeq ($(findstring Linux,$(PLATFORM)),Linux)
	LFLAGS += -ledit -lm
endif

ifeq ($(findstring Darwin,$(PLATFORM)),Darwin)
	LFLAGS += -ledit -lm
endif

all: $(FILES)

%: %.c mpc.c
	$(CC) $(CFLAGS) $^ $(LFLAGS) -o $@

