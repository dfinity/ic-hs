HS_TO_COQ ?= hs-to-coq
SRC ?= ../impl/src

MODULES = \
  IC/Utils \
  IC/Id/Fresh \
  IC/Id/Forms \
  IC/Types \
  IC/Canister \
  IC/Ref \

VFILES_GEN     = $(addprefix lib/,$(addsuffix .v,$(MODULES)))

VFILES   = $(VFILES_GEN)

all:  vfiles coqfiles lib theories
vfiles: $(VFILES)
coqfiles: lib/README.md lib/Makefile vfiles

lib/_CoqProject : Makefile
	mkdir -p lib
	> $@
	echo '-Q . ""' >> $@
	echo $(addsuffix .v,$(MODULES)) >> $@

lib/Makefile: lib/_CoqProject
	cd lib; coq_makefile -f _CoqProject -o Makefile
theories/Makefile: theories/_CoqProject
	cd theories; coq_makefile -f _CoqProject -o Makefile

lib/README.md:
	mkdir -p lib
	> $@
	echo 'This directory contains a Coq’ified versions of ic-ref' >> $@
	echo 'Do not edit files here!' >> $@

coq: lib/Makefile $(VFILES)
	$(MAKE) -C lib -f Makefile OPT=$(COQFLAGS)

theories: theories/Makefile
	$(MAKE) -C theories -f Makefile OPT=$(COQFLAGS)

HS_TO_COQ_OPTS := \
    -e edits \
    -N \
    -i $(SRC) \
    --iface-dir lib \
    --ghc -DCANISTER_FAKE \
    -o lib \


.SECONDEXPANSION:
$(VFILES_GEN): lib/%.v : $$(wildcard module-edits/$$*/preamble.v) $$(wildcard module-edits/$$*/midamble.v)  $$(wildcard module-edits/$$*/edits) edits
	$(HS_TO_COQ) $(addprefix -e , $(wildcard module-edits/$*/edits)) \
	             $(addprefix -p , $(wildcard module-edits/$*/preamble.v)) \
	             $(addprefix --midamble , $(wildcard module-edits/$*/midamble.v)) \
		     $(HS_TO_COQ_OPTS) \
		     $(SRC)/$*.hs
	test -e $@

# Every rule that builds a .v also produces the corresponding interface file
%.h2ci: %.v
	test -e $@

clean:
	rm -rf lib
	rm -f $(THEORIES)/Makefile
