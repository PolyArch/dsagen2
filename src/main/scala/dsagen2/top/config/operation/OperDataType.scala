package dsagen2.top.config.operation

import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.config.operation.Operation.{DsaOperation, FixedAdd}
import dsagen2.top.config.operation.OperationGroup.{BitwiseOperation, DsaOperGroup}

import scala.collection.Set
import scala.language.implicitConversions

/** Each Specific Operation has a related data type.
  * for example, Unsigned 8-bit Addition -> Add_uint8; Floating Point Multiplication for half -> FAdd_half
  */
object OperDataType {

  // Do cross product between set of operation and set of data type
  def crossproductOpDataType(
    ops:   Set[Operation.DsaOperation],
    types: Set[DataType.DsaDataType]
  ): Set[DsaOperDataType] = {
    for {
      op <- ops
      typ <- types
    } yield {
      tuple2opDataType(op, typ)
    }
  }

  // Implicit convert from tuple
  implicit def tuple2opDataType(tuple: (Operation.DsaOperation, DataType.DsaDataType)): DsaOperDataType = {
    tuple match {
      case (operation, dataType) => DsaOperDataType(operation, Some(dataType))
    }
  }

  // String convert
  implicit def string2opDataType(str: String): DsaOperDataType = {
    val listStr: List[String] = str.split("_").toList
    listStr match {
      case bitWiseOp :: Nil          => DsaOperDataType(bitWiseOp, None)
      case opName :: typeName :: Nil => DsaOperDataType(opName, Some(typeName))
      case _                         => require(requirement = false, s"Wrong format: $str"); DsaOperDataType(FixedAdd, None)
    }
  }

  // Implicit convert from DsaOperation to DsaOperDataType for Bitwise operation
  implicit def bitwise2opDataType(op: DsaOperation): DsaOperDataType = {
    if (op.is(BitwiseOperation)) DsaOperDataType(op, None) // if it is bitwise, then just create
    else
      DsaOperDataType(op, Some(op.opGroup.applicableDTs(op).head)) // it it is not bitwise, then take head of supported
  }

  // Operation + DataType
  case class DsaOperDataType(
    operation:      Operation.DsaOperation,
    dataType:       Option[DataType.DsaDataType],
    predefinedImpl: Option[IsFunctionUnitImplementation] = None) {
    require(
      (operation.is(BitwiseOperation) && dataType.isEmpty) || (!operation.is(BitwiseOperation) && dataType.isDefined),
      s"You operation + data type definition is problematic, $operation + $dataType"
    )

    // Convert to String, connect with "_"
    override def toString: String = {
      if (!operation.is(BitwiseOperation)) operation.toString + '_' + dataType.get.str
      else {
        require(dataType.isEmpty);
        operation.toString
      }
    }

    // Compact String
    def compactString: String = {
      if (!operation.is(BitwiseOperation)) operation.toString + dataType.get.str
      else {
        require(dataType.isEmpty);
        operation.toString
      }
    }

    // Operation Properties
    def numOperand: Int = operation.numOperand

    def numResult: Int = operation.numResult

    // Get the number of total bits
    def totalBits: Option[Int] = {
      if (operation.is(BitwiseOperation)) {
        require(dataType.isEmpty, s"Bitwise operation should not have data Type defined");
        None
      } else Some(dataType.get.compBits)
    }

    // Get the number of unit bits
    def unitBits: Option[Int] = {
      if (operation.is(BitwiseOperation)) {
        require(dataType.isEmpty, s"Bitwise operation should not have data Type defined");
        None
      } else Some(dataType.get.unitBits)
    }

    // Check the operation type
    def is(typ: OperationGroup.DsaOperGroup): Boolean = operation.is(typ)

    // Operation DataType coverage for composable Function Unit
    def cover(that: DsaOperDataType): Boolean = {
      // Find whether that operation exist in the same composable group
      val isSameComposableGroup: Boolean = {
        ComposableGroup.values.exists {
          case group: ComposableGroup.ComposableOperationGroup =>
            group.opSet.contains(this.operation) && group.opSet.contains(that.operation)
          case _ => false
        }
      }
      // Check if it is coverable operation
      if (isSameComposableGroup && !this.is(BitwiseOperation) && !that.is(BitwiseOperation)) {
        // If this OperationDataType can cover that, it requires that this's vecWidth is finer
        this.dataType.get.vecWidth > that.dataType.get.vecWidth
      } else false
    }

    // Implementation Get and Set
    var actualImpl: Option[IsFunctionUnitImplementation] = predefinedImpl match {
      case Some(predefined) => Some(predefined)
      case None             => None
    }

    def setImpl(impl: IsFunctionUnitImplementation): Unit = {
      // implementation is applicable
      require(
        impl.supportOperation.contains(this.operation),
        s"Implementation $impl does not support ${this.operation}"
      )
      this.dataType match {
        case Some(dt) =>
          require(impl.supportDataType.contains(dt), s"Implementation $impl does not support ${dt}")
        case None =>
      }
      actualImpl = Some(impl)
    }

    def getImpl: IsFunctionUnitImplementation = {
      actualImpl match {
        // Actual implementation is defined
        case Some(exist) => exist
        // Actual implementation is not defined
        case None =>
          val candidates: Set[IsFunctionUnitImplementation] = getImpls
          if (candidates.nonEmpty) {
            candidates.head // TODO: for now it is trivial, just take the first one, optimize for PPA later
          } else {
            require(
              requirement = false,
              s"The actual implementation of $this is not defined, and search for " +
                s"the implementation did not find any possible candidate implementation"
            )
            candidates.head // never reach
          }
      }
    }

    // Return all possible hardware implementation of this operation
    def getImpls: Set[IsFunctionUnitImplementation] = {
      dsagen2.comp.impl.IsFunctionUnitImplementation.allImpls.filter { impl =>
        val supportDataType: Boolean = this.dataType match {
          case Some(dt) => impl.supportDataType.contains(dt)
          case None =>
            require(this.operation.is(BitwiseOperation), s"None data type and it is not bitwise");
            true
        }
        impl.supportOperation.contains(this.operation) && supportDataType
      }
    }

    // Equality
    override def equals(obj: Any): Boolean =
      obj match {
        case o: DsaOperDataType =>
          (dataType, o.dataType) match {
            case (Some(thisType), Some(thatType)) => operation.equals(o.operation) && thisType.equals(thatType)
            case (None, None)                     => operation.equals(o.operation)
            case _                                => false
          }
        case _ => false
      }

    override def hashCode(): Int = {
      if (dataType.isDefined) {
        dataType.get.hashCode() + operation.hashCode()
      } else {
        operation.hashCode()
      }
    }
  }

  // Filter all operation with specific data type
  def DsaOperationOfDataType(
    dataBits:     Int,
    unitBits:     Int = -1,
    withoutGroup: Set[DsaOperGroup] = Set.empty
  ): Set[DsaOperDataType] =
    Operation.values
      .map(
        _.WithDataType(
          dataBits, {
            if (unitBits == -1) dataBits else unitBits
          },
          withoutGroup
        )
      )
      .reduce(_ ++ _)

  // All Operation + Data Type Group
  val AllDsaOperationDataType: Set[DsaOperDataType] =
    Operation.values.map(_.withAllDataType).reduce(_ ++ _)
  val AllDsaOperDataTypeID: Map[DsaOperDataType, Int] = AllDsaOperationDataType.zipWithIndex.map { case (opt, i) =>
    opt -> i
  }.toMap
}
