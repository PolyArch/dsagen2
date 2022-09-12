# Include Spec
include ../dsagen2.mk

# Compile Spec
COMPILE_FLAGS := BITSTREAM=1 COMPAT_ADG=0 SBCONFIG=$(ADG) GEM5=0 COUNT=1 TEMPORAL_FALLBACK=1
OBJS = $(addsuffix .riscv, $(addprefix ss-, $(SS_TESTS)))
ifdef NUM_CORES
  COMPILE_FLAGS += NUM_CORES=$(NUM_CORES)
endif
ifdef FAKE
  COMPILE_FLAGS += FAKE=1
endif


.PHONY: all
all: $(OBJS)

# Compile Single Stream-Specialized Application
ss-%.riscv: 
	mkdir -p $(WORKLOAD)
	make -C $(SS_APP_DIR) $(COMPILE_FLAGS) ss-$*.riscv
	cp $(SS_APP_DIR)/ss-$*.riscv $(WORKLOAD)

.PHONY: clean
clean:
	make -C $(SS_APP_DIR) clean
	rm -rf microbench
	rm -rf *.riscv

