include dsagen2.mk

.PHONY: all
all: compile workloads

.PHONY: compile
compile: clean
	sbt compile

.PHONY: test
test: compile
	mkdir -p vsrc
	mkdir -p sims
	sbt test

.PHONY: fus
fus: 
	ALL=1 sbt "testOnly dsagen2.comp.impl.FuGenTest"

.PHONY: workloads
workloads: $(addprefix compile-, $(WORKLOAD_SET))

.PHONY: clean
clean: clean-workloads
	rm -rf vsrc sims adg/*20*-*.json adg/*20*-*.dot ucli.key test_run_dir *.v *.anno.json *.fir 

.PHONY: clean-ide
clean-ide:
	rm -rf .idea .bsp

.PHONY: clean-workloads
clean-workloads: $(addprefix clean-, $(WORKLOAD_SET))
	make -C workloads clean

.PHONY: purge
purge: clean clean-ide clean-workloads
	rm -rf project/project project/target target

