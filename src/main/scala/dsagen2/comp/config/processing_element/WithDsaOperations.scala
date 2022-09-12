package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Config
import dsagen2.comp.config.CompKeys._
import dsagen2.top.config.operation.OperDataType
import dsagen2.top.config.operation.OperDataType.AllDsaOperationDataType

import scala.collection.Set

class WithDsaOperations(
  opDataType:        Set[OperDataType.DsaOperDataType] = AllDsaOperationDataType,
  instSlotSize:      Int = 1,
  maxFifoDepth:      Int = 4, // 0 means dynamic fifo
  maxInstRepeatTime: Int = 0,
  definedLatency:    Int = 0,
  isDynamic:         Boolean = true)
    extends Config((site, here, up) => { case DsaOperations =>
      // Check whether it is previously defined
      up.lift(DsaOperations) match {
        // Add new operation if it is predefined
        case Some(old_para) =>
          old_para.copy(
            givenOpDataTypeSet = old_para.opDataTypeSet ++ opDataType,
            instSlotSize,
            maxFifoDepth,
            maxInstRepeatTime,
            definedLatency,
            isDynamic
          )
        // Defined new if not defined yet
        case None =>
          PEDsaOperationParameters(opDataType, instSlotSize, maxFifoDepth, maxInstRepeatTime, definedLatency, isDynamic)
      }
    })
