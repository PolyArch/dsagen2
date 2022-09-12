package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Config
import dsagen2.comp.config.{CompNodeParameters, WithCompNode}
import dsagen2.top.config.operation.DataType._
import dsagen2.top.config.operation.Operation._
import dsagen2.top.diplomacy.DSANodeType.ProcessingElement

class PEAddMulConfig
    extends Config(
      new WithRegister() ++
        new WithMetaControl() ++
        new WithDsaOperations(
          Set(
            FixedAdd + SignedInt64,
            FixedMul + SignedInt64,
            FloatAdd + Double64,
            FloatMul + Double64
          )
        ) ++
        new WithCompNode(CompNodeParameters(nodeType = ProcessingElement, compUnitBits = 64))
    )

class PEMaxMinConfig
    extends Config(
      new WithRegister() ++
        new WithMetaControl() ++
        new WithDsaOperations(
          Set(
            FixedMax + SignedInt64,
            FixedMin + SignedInt64
          )
        ) ++
        new WithCompNode(CompNodeParameters(nodeType = ProcessingElement, compUnitBits = 64))
    )

class PEAddMulMaxMinConfig
    extends Config(
      new WithRegister() ++
        new WithMetaControl() ++
        new WithDsaOperations(
          Set(
            FixedAdd + SignedInt64,
            FixedMul + SignedInt64,
            FloatAdd + Double64,
            FloatMul + Double64,
            FixedMax + SignedInt64,
            FixedMin + SignedInt64
          )
        ) ++
        new WithCompNode(CompNodeParameters(nodeType = ProcessingElement, compUnitBits = 64))
    )

class PE_Simple64_Config
    extends Config(
      new WithRegister() ++
        new WithMetaControl() ++
        new WithDsaOperations(
          Set(
            // Fixed Point Operation
            FixedAdd + SignedInt64,
            FixedSub + SignedInt64,
            FixedMul + SignedInt64,
            FixedMax + SignedInt64,
            FixedMin + SignedInt64,
            // Float Point Operation
            FloatAdd + Double64,
            FloatSub + Double64,
            FloatMul + Double64
          )
        ) ++
        new WithCompNode(CompNodeParameters(nodeType = ProcessingElement, compUnitBits = 64))
    )

class PE_Full64_Config
    extends Config(
      new WithRegister() ++
        new WithMetaControl() ++
        new WithDsaOperations(
          Set(
            // Fixed Operation
            FixedAdd + SignedInt64,
            FixedSub + SignedInt64,
            FixedMul + SignedInt64,
            FixedMax + SignedInt64,
            FixedMin + SignedInt64,
            FixedDiv + SignedInt64,
            // Float Operation
            FloatAdd + Double64,
            FloatSub + Double64,
            FloatMul + Double64,
            FloatDiv + Double64,
            FloatSqrt + Double64
          )
        ) ++
        new WithCompNode(CompNodeParameters(nodeType = ProcessingElement, compUnitBits = 64))
    )

class PE_Full64_SIMD_Config
    extends Config(
      new WithRegister() ++
        new WithMetaControl() ++
        new WithDsaOperations(
          Set(
            // Fixed Operation
            FixedAdd + SignedInt64,
            FixedSub + SignedInt64,
            FixedMul + SignedInt64,
            FixedDiv + SignedInt64,
            FixedMax + SignedInt64,
            FixedMin + SignedInt64,
            // Floating Operation
            FloatAdd + Double64,
            FloatSub + Double64,
            FloatMul + Double64,
            FloatDiv + Double64,
            FloatSqrt + Double64,
            // Adhoc SIMD Operation
            FixedHLAdd + SignedInt64,
            FixedHLAdd + SignedInt32x2,
            Concat + SignedInt64,
            Concat + SignedInt32x2,
            FixedAdd + SignedInt16x4,
            FixedMul + SignedInt16x4,
            FixedDiv + SignedInt16x4
          )
        ) ++
        new WithCompNode(CompNodeParameters(nodeType = ProcessingElement, compUnitBits = 64))
    )

class PEAdd32Config
    extends Config(
      new WithRegister() ++
        new WithMetaControl() ++
        new WithDsaOperations(
          Set(
            // Fixed Operation
            FixedAdd + SignedInt32x2
          )
        ) ++
        new WithCompNode(CompNodeParameters(nodeType = ProcessingElement, compUnitBits = 64))
    )
