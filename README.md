# Domain-Specific Accelerator Generator 2

Contributors: [Sihao Liu](https://www.linkedin.com/in/sihao-liu/), [Dylan Kupsh](https://www.linkedin.com/in/dkupsh/), [Maxim Zhulin](https://github.com/map0te), [Lucheng Zhang](https://lucian.run/)

***DSAGen2 Development Status***

**Control System**
--------------------------------------------------------------------
*Stream Dispatcher*
- A fixed Out-of-Order design  
- Support Bus / Tree reconfiguration network @ Generation
- Support reconfigurable depth of stream entry queue @ Generation

**Memory System**
--------------------------------------------------------------------
*Direct Memory Access* (Main memory access)
- Support optional Linear 1D, 2D, 3D stream access
- Support optional Indirect Index (1D), Indirect Stride2D, Indirect Length1D, stream access
- Support hybrid Linear/Indirect stream access pattern
- Support optional stated stream
- Support reconfigurable depth of ROB

*Scratchpad Memory* (High bandwidth access)
- A fully pipelined banked conflict-solved scratchpad design, with fixed latency = 3 cycles
- Support atomic operation
- Support optional Linear 1D, 2D, 3D stream access
- Support optional Indirect Index (1D), Indirect Stride2D, Indirect Length1D, stream access
- Support hybrid Linear/Indirect stream access pattern
- Support optional stated stream
- Support reconfigurable depth of ROB

*Generate Engine* (Generate numerical sequence as input streams)
- Support optional Linear 1D, 2D, 3D stream access
- Support optional Indirect Index (1D), Indirect Stride2D, Indirect Length1D, stream access
- Support hybrid Linear/Indirect stream access pattern
- Support optional stated stream

*Recurrence Engine* (Send output streams as input streams)
- A round-robin stream scheduler design among read and write streams
- Support output stream recurrence as input stream

*Discard Engine* (Garbage of stream)
- Support stream discard from output vector port

*Register Engine* (Communication between CPU)
- Support value retrieve from output vector port and sent back to CPU

**Synchronization System**
--------------------------------------------------------------------
*Input Vector Port*
- Support synchronization between response vector from memory and dataflow vector to compute
- Support stated/non-stated stream
- Support compute vector repeating
- Support compute vector predication padding based on state of stream
- Support fully-crossbar/limited-crossbar/non-crossbar design
- Support reconfigurable depth
 
*Output Vector Port*
- Support synchronization between dataflow vector from compute and request vector to memory 
- Support stated/non-stated stream
- Support fully-crossbar/limited-crossbar/non-crossbar design
- Support reconfigurable depth

**Compute System**
--------------------------------------------------------------------
- *Dynamic + Shared execution model is not supported for now*
- *Decomposable execution are not completely supported for now*

*Processing Element*
- Support reconfigurable ALU: standard fixed-point, floating-point (hardfloat), trigonometric (LUT) operation
- Support optional control scheme: operands reuse, result discard, register (optional) reset
- Support different data type as decomposable operation: like Add_I8x8, Add_I64 share ALU
- Support different execution models: (Static / Dynamic instruction scheduling) x (Dedicated / Shared instruction placement)

*Switch*
- Support reconfigurable connectivity-matrix routing configuration
- Support different execution models: (Static / Dynamic instruction scheduling) x (Dedicated / Shared instruction placement)
