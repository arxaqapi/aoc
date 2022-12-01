EXECNAME := solutions
BIN := bin

run: b
	@echo "Program compiled"

b build: $(BIN)
	@dune build
	@rm -f ./bin/$(EXECNAME)
	@mv _build/default/src/main.exe ./bin/$(EXECNAME)

$(BIN):
	mkdir -p $@

t test: b
	dune test
.PHONY: clean
c clean:	
	@rm -rf _build/
	@rm -rf $(BIN)