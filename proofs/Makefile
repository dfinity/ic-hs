HS_TO_COQ ?= hs-to-coq
SRC ?= ../impl/src
OUT ?= lib

MODULES = \
  IC/Utils \
  IC/Id/Fresh \
  IC/Id/Forms \
  IC/Types \
  IC/Canister \
  IC/Ref \

VFILES_GEN     = $(addprefix $(OUT)/,$(addsuffix .v,$(MODULES)))

VFILES   = $(VFILES_GEN)

all:  vfiles coqfiles coq
vfiles: $(VFILES)
coqfiles: $(OUT)/README.md $(OUT)/Makefile vfiles

$(OUT)/_CoqProject : Makefile
	mkdir -p $(OUT)
	> $@
	echo '-Q . ""' >> $@
	echo $(addsuffix .v,$(MODULES)) >> $@

$(OUT)/Makefile: $(OUT)/_CoqProject
	cd $(OUT); coq_makefile -f _CoqProject -o Makefile

$(OUT)/README.md:
	mkdir -p $(OUT)
	> $@
	echo 'This directory contains a Coq’ified versions of ic-ref' >> $@
	echo 'Do not edit files here!' >> $@

coq: $(OUT)/Makefile $(VFILES)
	$(MAKE) -C $(OUT) -f Makefile OPT=$(COQFLAGS)

HS_TO_COQ_OPTS := \
    -e edits \
    -N \
    -i $(SRC) \
    --iface-dir $(OUT) \
    --ghc -DCANISTER_FAKE \
    -o $(OUT) \


.SECONDEXPANSION:
$(VFILES_GEN): $(OUT)/%.v : $$(wildcard module-edits/$$*/preamble.v) $$(wildcard module-edits/$$*/midamble.v)  $$(wildcard module-edits/$$*/edits) edits
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
	rm -rf $(OUT)
