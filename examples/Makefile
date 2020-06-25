# Do not remove .wasm files
.PRECIOUS: %.wasm

CC = clang
CXX = clang++
WASM2WAT ?= wasm2wat

export PATH := /usr/local/opt/llvm/bin:$(PATH)

%.wasm: %.cpp
	"$(CXX)" -nostdlib --target=wasm32 -Wl,--allow-undefined --std=c++17 -o "$(basename $<).wasm" $(CXXFLAGS) "$<"

%.wasm: %.c
	"$(CC)" -nostdlib --target=wasm32 -Wl,--allow-undefined -o "$(basename $<).wasm" $(CFLAGS) "$<"

%.wat: %.wasm
	"$(WASM2WAT)" $(WASM2WATFLAGS) "$<" > "$(basename $<).wat"

.PHONY: all
all:
	make $(patsubst %.c,%.wat,$(wildcard *.c hello/*.c)) $(patsubst %.cpp,%.wat,$(wildcard *.cpp))

.PHONY: clean
clean:
	rm -f *.wat hello/*.wat *.wasm hello/*.wasm
