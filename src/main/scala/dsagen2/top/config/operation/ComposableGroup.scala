package dsagen2.top.config.operation

import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.config.operation.Operation.{DsaOperation, FixedAdd, FixedMul, FixedSub}

object ComposableGroup extends Enumeration {
  case class ComposableOperationGroup(opSet: Set[DsaOperation], impl: IsFunctionUnitImplementation) extends super.Val

  import dsagen2.comp.impl.fu.fixed

  // For now I only know that Add and Mul is composable, meaning for example 8x8-bit Add can produce 64-bit sum easily
  val FixedAddComposable: ComposableOperationGroup = ComposableOperationGroup(Set(FixedAdd, FixedSub), fixed.AddSub)
  val FixedMulComposable: ComposableOperationGroup = ComposableOperationGroup(Set(FixedMul), fixed.Mul)
}
