# Directory setup
SS 		?= /home/sihao/repo/ss-stack
DSAGEN_MF_PATH 	:= $(abspath $(lastword $(MAKEFILE_LIST)))
DSAGEN_DIR 	?= $(dir $(DSAGEN_MF_PATH))
ADG_DIR 	?= $(DSAGEN_DIR)adg
SS_BINARY_DIR 	?= $(DSAGEN_DIR)workloads

ifndef DSAGEN_DIR
$(error DSAGEN_DIR is unset. You must set it yourself)
else
# $(info Running with DSAGEN_DIR=$(DSAGEN_DIR))
export ADG_DIR=$(DSAGEN_DIR)adg
endif

# Workloads set
WORKLOAD_SET := vision machsuite dsp

# Workloads (SS: Stream Specialized)
MICROBENCH := \
	vecadd vecmax matadd move submat transpose triangle fansum slp coal dual copy copy-4d \
	pad acc max-acc prefix sr fanout tri-simp move-spad spad mv mv-unroll invariant outer update-trivial cdiv \
	ind-simple ind-diverge ind-1d-2d ind-2d-read ind-2d-acc update update-nb update-small mvo mvo-no-pipe sr-sum \
	biased-mv \
	#hist join buffet buffet-4d # not tested
	#acc-pene norm norm-2d temporal    # broken

# Xilinx Vitis Vision Kernels
VISION_KERNELS := accumulate accumulate_weighted accumulate_squared blur vecmax convert_bitdepth channel_extract bgr2grayscale derivative

# MachSuite Kernels
MACHSUITE_KERNELS := stencil-3d stencil-2d gemm ellpack crs #backprop

# DSP Kernels 
DSP_KERNELS := mm2 fft cholesky solver fir

# Default workloads
ifeq ($(WORKLOAD), vision)
  SS_TESTS := $(VISION_KERNELS)
  SS_APP_DIR := $(SS)/ss-workloads/Compilation/MultiCore/Vision
else ifeq ($(WORKLOAD), machsuite)
  SS_TESTS := $(MACHSUITE_KERNELS) 
  SS_APP_DIR := $(SS)/ss-workloads/Compilation/MultiCore/MachSuite
else ifeq ($(WORKLOAD), dsp)
  SS_TESTS := $(DSP_KERNELS)
  SS_APP_DIR := $(SS)/ss-workloads/Compilation/MultiCore/Dsp
else
  WORKLOAD := microbench
  SS_TESTS := $(MICROBENCH)
  SS_APP_DIR := $(SS)/ss-workloads/Compilation/Tests
endif
  
# Default Hardware (Architecture Description Graph, ADG)
ifndef ADG
  ADG := $(ADG_DIR)/Mesh7x5-Full64-Full7I5O.json
endif

# Run all Stream Specialized workloads
ss-run-%:
	make CONFIG=$(CONFIG) BINARY=$(SS_BINARY_DIR)/$(WORKLOAD)/ss-$*.riscv run-binary-fast
.PHONY: ss-run
ss-run: $(addprefix ss-run-, $(SS_TESTS))

# Debug all Stream Specialized workloads
ss-debug-%:
	make CONFIG=$(CONFIG) BINARY=$(SS_BINARY_DIR)/$(WORKLOAD)/ss-$*.riscv run-binary-debug
.PHONY: ss-debug
ss-debug: $(addprefix ss-debug-, $(SS_TESTS))

# Compile each workload set
compile-%:
	make -j -C workloads WORKLOAD=$* all

# Clean each workload set
clean-%:
	rm -rf workloads/$*
	make -C workloads WORKLOAD=$* clean

# Run each workload set
run-%:
	make -j WORKLOAD=$* ss-run

debug-%:
	make -j WORKLOAD=$* ss-debug

# Run all workload sets
runall: $(addprefix run-, $(WORKLOAD_SET))
debugall: $(addprefix debug-, $(WORKLOAD_SET))

# Collect result
collectcycle-%:
	$(SS)/ss-workloads/Compilation/Common/wash_trace.sh $(SS_BINARY_DIR)/$(WORKLOAD)/ss-$*.riscv $(output_dir)/ss-$*.out
collectcycle: $(addprefix collectcycle-, $(SS_TESTS))
collectset-%:
	make WORKLOAD=$* collectcycle
collectall: $(addprefix collectset-, $(WORKLOAD_SET))

