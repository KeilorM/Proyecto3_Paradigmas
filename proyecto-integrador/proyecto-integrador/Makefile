GHC      ?= ghc
SRC      := $(shell find src app -name '*.hs')
ENTRADA  ?= problema.txt
SALIDA   ?= salida

.PHONY: all run test test-haskell test-prolog consultas doc evidencia clean

all: build/proyecto-integrador

build/proyecto-integrador: $(SRC)
	mkdir -p build
	$(GHC) -Wall -isrc -iapp -outputdir build -o $@ app/Main.hs

run: build/proyecto-integrador
	build/proyecto-integrador $(ENTRADA) $(SALIDA)

build/spec: $(SRC) test/Spec.hs
	mkdir -p build/test
	$(GHC) -Wall -isrc -itest -outputdir build/test -o $@ test/Spec.hs

test: test-haskell test-prolog

test-haskell: build/spec
	build/spec

# Las pruebas Prolog son para el problema.txt de ejemplo del repositorio.
test-prolog: build/proyecto-integrador
	build/proyecto-integrador problema.txt salida > /dev/null
	swipl -q test/pruebas.pl

consultas: run
	swipl -q -g ejecutar_consultas -t halt $(SALIDA)/consultas.pl

doc:
	doxygen Doxyfile

# Guarda la salida de las pruebas como evidencia.
evidencia: build/spec build/proyecto-integrador
	mkdir -p docs/evidencia
	build/proyecto-integrador problema.txt salida > docs/evidencia/ejecucion_haskell.txt
	build/spec > docs/evidencia/pruebas_haskell.txt
	swipl -q test/pruebas.pl > docs/evidencia/pruebas_prolog.txt
	swipl -q -g ejecutar_consultas -t halt salida/consultas.pl > docs/evidencia/consultas_prolog.txt

clean:
	rm -rf build docs/doxygen
