TARGET_EXEC ?= compilo

CAML := ocamlc
BUILD_DIR ?= .
SRC_DIRS ?= .

SRCS := ASMType.ml ASMGram.ml ASMLex.ml CLessType.ml Tools.ml CLessGram.ml CLessLex.ml PrintC.ml PrintAST.ml\
	interpreter.ml Generate.ml Optimisation.ml reg_assignment.ml compilo.ml
#GenerateReg.ml	
#$(shell find $(SRC_DIRS) -name '*.ml')
OBJS := $(SRCS:.ml=.cmo)
DEPS := $(OBJS:.cmo=.d)

INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_FLAGS :=
LDFLAGS := unix.cma

CPPFLAGS ?= $(INC_FLAGS)

$(BUILD_DIR)/$(TARGET_EXEC): $(OBJS)
	$(CAML) $(LDFLAGS) $(OBJS) -o $@ 

#Generate.cmo: Generate.ml
#	$(CAML) -ppx ./ppx_asm $(CPPFLAGS) -c $< -o $@	

# ml source
%.cmo: %.ml
#	$(MKDIR_P) $(dir $@)
	$(CAML) -c $< -o $@
%cmi: %mli
	$(CAML) $<

.PHONY: clean

package:
	zip -r compilo.zip ./

clean:
	$(RM) $(BUILD_DIR)/*.cmo
	$(RM) $(BUILD_DIR)/*.cmi
	$(RM) $(TARGET_EXEC)

-include $(DEPS)

MKDIR_P ?= mkdir -p

ASMType.cmo : ASMType.cmi
ASMType.cmx : ASMType.cmi
ASMGram.cmo : ASMType.cmo ASMGram.cmi
ASMGram.cmx : ASMType.cmx ASMGram.cmi
ASMLex.cmo : ASMType.cmo ASMGram.cmi
ASMLex.cmx : ASMType.cmx ASMGram.cmx
CLessType.cmo : ASMType.cmo ASMLex.cmo ASMGram.cmi
CLessType.cmx : ASMType.cmx ASMLex.cmx ASMGram.cmx
CLessGram.cmo : CLessType.cmo CLessGram.cmi
CLessGram.cmx : CLessType.cmx CLessGram.cmi
CLessLex.cmo : CLessType.cmo CLessGram.cmi
CLessLex.cmx : CLessType.cmx CLessGram.cmx
Generate.cmo : CLessType.cmo ASMType.cmo Tools.cmo Generate.cmi
Generate.cmx : CLessType.cmx ASMType.cmx Tools.cmx Generate.cmi
#GenerateReg.cmo : CLessType.cmo ASMType.cmo Tools.cmo reg_assignment.cmo
#GenerateReg.cmx : CLessType.cmx ASMType.cmx Tools.cmx reg_assignment.cmx
Optimisation.cmo : ASMType.cmo
Optimisation.cmx : ASMType.cmx
PrintAST.cmo : CLessType.cmo
PrintAST.cmx : CLessType.cmx
PrintC.cmo : CLessType.cmo
PrintC.cmx : CLessType.cmx
compilo.cmo : PrintC.cmo PrintAST.cmo Optimisation.cmo interpreter.cmi \
    Generate.cmi CLessType.cmo ASMType.cmo reg_assignment.cmo
compilo.cmx : PrintC.cmx PrintAST.cmx Optimisation.cmx interpreter.cmx \
    Generate.cmx CLessType.cmx ASMType.cmx reg_assignment.cmx
interpreter.cmo : ASMType.cmo
interpreter.cmx : ASMType.cmx
reg_assignment.cmo : ASMType.cmi
reg_assignment.cmx : ASMType.cmx
