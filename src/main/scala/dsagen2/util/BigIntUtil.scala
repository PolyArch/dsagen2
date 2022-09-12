package dsagen2.util

import chisel3.util.{isPow2, log2Ceil}

import scala.annotation.tailrec
import scala.language.implicitConversions

object BigIntUtil {

  class HwEnhancedBigInt(val b : BigInt, val set_width : Int = -1){

    /**
     * Concat two BigInts
     * @param that the tuple of other BigInt and its width
     * @return the Concated BigInt
     */
    def :: (that : HwEnhancedBigInt) : BigInt = {
      BigInt(that.bStr + this.bStr, 2)
    }

    def :: (value : Int, width : Int) : BigInt = ::((value, width))
    def :: (value : BigInt, width : Int) : BigInt = ::((value, width))

    /**
     * Return the BigInt with given width, operate based on binary string
     * If the given width is wider than its original width, the value will not changed
     * If the given width is narrower than its original width, then the value may changed (MSB cut off)
     * @param fixedWidth the given width
     * @return Sign-not-changed BigInt with given width
     */
    def width (fixedWidth : Int) : BigInt = {
      // Convert binear string to BigInt
      val fixedWidthBigInt : BigInt = BigInt(b.bStr(fixedWidth), 2)
      // Check the generated BigInt with given width. whether its bitLength is equal to given width
      require(fixedWidthBigInt.bitLength == fixedWidth,
        s"Generated BigInt's bitLength = ${fixedWidthBigInt.bitLength}, which is not equal to given $fixedWidth")
      // Print out warning if the fixedWidthBigInt is not equal to original
      val verbose : Int = 0
      if(verbose > 2 && fixedWidthBigInt != b){
        println(s"[Warning] After width fixed, BigInt = $b becomes BigInt = $fixedWidthBigInt")
      }
      // return fixed width BigInt
      fixedWidthBigInt
    }

    // Just a wrapper for width, use the set_width in case width is given by user
    def width : BigInt = width(set_width)

    /**
     * Binray String of BigInt with given width (will cut off), no signed
     * @param width the given width of binary string
     */
    def bStr(width : Int) : String = {
      // Get the default string
      val str : String = if(b > 0){
        leading0(b.toString(2), width)
      }else if(b == 0){
        leading0("", width)
      }else {
        // Calculate the abs of negative BigInt
        val abs_b : BigInt = 0 - b
        // How many bit needed to represent this abs number
        val half_bit : Int = log2Ceil(abs_b)
        // Minimum bits needed for this negative BigInt calculated
        val minimum_bit : Int = half_bit + 1
        // Get -1 or FFF...F of this minimum bit
        val minus1 : BigInt = getLargestBigInt(minimum_bit)
        // Calculate the positive number whose binary string is identical with 2nd of negative number
        val pBigInt : BigInt = minus1 - abs_b + 1
        // Return Binary String
        pBigInt.toString(2)
      }

      // Cut off MSB
      val binaryString = if(str.length > width){
        str.substring(str.length - width)
      }else{
        // Adding leading 0 / 1
        if(b < 0){
          leading1(str, width)
        }else {
          leading0(str, width)
        }
      }

      // Return String
      require(binaryString.length == width,
        s"generated binary string's length (${binaryString.length}) " +
          s"is not equal to the one given ($width)")
      binaryString
    }

    // Just a wrapper of bStr, use set_width as width
    def bStr : String = bStr(set_width)
  }

  // Add Implicit function support for hardware enhanced BigInt class
  implicit def bigint2HwEnBigInt (b : BigInt): HwEnhancedBigInt = {
    if(b >= 0){
      new HwEnhancedBigInt(b, b.bitLength)
    }else{
      new HwEnhancedBigInt(b, b.bitLength + 1) // Add signed bit for negative BigInt
    }
  }

  // Add Implicit convertion from Int to Hardware Enhanced BigInt class
  implicit def int2HwEnBigInt (i : Int) : HwEnhancedBigInt = bigint2HwEnBigInt(BigInt(i))

  // Add implicit function support for hardware enhanced BigInt class
  implicit def tuple2HwEnBigInt (b_width : (BigInt, Int)) : HwEnhancedBigInt =
    new HwEnhancedBigInt(b_width._1, b_width._2)

  // Add implicit function support for hardware enhanced BigInt class
  implicit def intTuple2HwEnBigInt (b_width : (Int, Int)) : HwEnhancedBigInt =
    new HwEnhancedBigInt(b_width._1, b_width._2)

  // Add implicit conversion from binary String to hw enhanved BigInt class
  implicit def bStr2HwEnBigInt(bStr : String) : HwEnhancedBigInt = {
    // should only contains 0/1 (binary string)
    require(bStr.forall(c => c == '0' || c == '1'), "only take binary string")
    new HwEnhancedBigInt(BigInt(bStr, 2), bStr.length)
  }

  /**
   * Recursive function that calculate the largest BigInt of given width
   * @param width remaining width
   * @param acc accumulate string
   * @return Biggest BigInt with width given
   */
  @tailrec
  private def createLargestBigInt(width:Int, acc : String):BigInt={
    if(width > 0){
      createLargestBigInt(width - 1, acc + "1")
    }else{
      BigInt(acc, 2)
    }
  }

  /**
   * Calculate the biggest BigInt of given width
   * @param width Input parameter, set the width of BigInt
   * @return The biggest BigInt of given width
   */
  def getLargestBigInt(width:Int):BigInt={
    createLargestBigInt(width, "")
  }

  /**
   * Add leading zeros to string to a fixed width, if the width is already
   * larger than given width, then just return the given string
   * @param str string to be added leading zero
   * @param width the fixed width of return width
   * @return String with leading zero or Original string
   */
  @tailrec
  def leading0(str : String, width : Int) : String = {
    if(str.length < width){
      leading0("0" + str, width)
    }else{
      str
    }
  }

  /**
   * Add leading ones to string to a fixed width, if the width is already
   * larger than given width, then just return the given string
   * @param str string to be added leading one
   * @param width the fixed width of return width
   * @return String with leading one or Original string
   */
  @tailrec
  def leading1(str : String, width : Int) : String = {
    if(str.length < width){
      leading1("1" + str, width)
    }else{
      str
    }
  }

  /**
   * Convert BigInt to String where radix = 2 and length is width (padding
   * with leading zero)
   * @param v The BigInt value
   * @param width The return String width
   * @return String of BigInt whose radix is 2 with leading zero
   */
  def BigInt2String(v : BigInt, width : Int) : String = {
    BigInt2String(v , width , radix = 2)

  }

  /**
   * Convert BigInt to String where radix is given as parameters and length
   * is width (padding with leading zero)
   * @param v Given BigInt value
   * @param width String Length
   * @param radix String Radix
   * @return String of BigInt of certain radix with leading zeros
   */
  def BigInt2String(v : BigInt, width : Int, radix : Int ) : String = {
    if(v >= 0) {
      leading0(v.toString(radix), width)
    }else{
      v.bStr(width)
    }
  }

  /**
   * Calculate the 2nd complement of given BigInt
   * @param value input BigInt to be calculated 2nd complement
   * @param width Width of complement
   * @return 2's complement of given value
   */
  def complement2nd(value: BigInt, width : Int):BigInt={
    if(value < 0){
      val comp2nd = getLargestBigInt(width) + value + 1
      require(comp2nd >= 0)
      require(comp2nd.bitLength == width)
      comp2nd
    }else{
      value
    }
  }

  /**
   * Split the BigInt value into multiple narrow BigInt as Little-Endian
   * for example:
   *   1110010111010000 -> Seq(0000, 1101, 0101, 1110)
   * @param value Input wider BigInt
   * @param single_data_width data width of each returned value
   * @param seq_length The length of output sequence
   * @return A sequence of BigInt whose width is narrower
   */
  def BigInt2Seq(value : BigInt, seq_length : Int, single_data_width : Int) : Seq[BigInt] = {
    // Calculate the bit length of input BigInt
    val full_size : Int = seq_length * single_data_width
    // Calculate the binary string of input string as 2nd complement
    // with fixed length
    val full_str = leading0(complement2nd(value,full_size).toString(2), full_size)
    // Convert the binary string of input BigInt to multiple narrow BigInt
    // as Little-Endian
    val seq = full_str.grouped(single_data_width).toSeq.reverseMap(BigInt(_, 2))
    // Check the length matched
    require(seq.length == seq_length, s"Output length = ${seq.length} != " +
      s"given length = $seq_length")
    // Check bit width matched
    require(seq.forall{v => v.bitLength <= single_data_width},
      s"Max narrower width (${seq max}) > given data width ($single_data_width)")
    seq
  }

  /**
   * Concat the BigInt Sequence as Little-Endian to form a long BigInt
   * for example:
   *  Seq(0000, 1101, 0101, 1110) -> 1110010111010000
   * @param seq The BigInt sequence
   * @param seq_length The length of sequence
   * @param single_data_width The width of each BigInt Value
   * @return Concat BigInt value as Little-Endian
   */
  def Seq2BigInt(seq : Seq[BigInt], seq_length : Int, single_data_width : Int)
  : BigInt = {
    // Calculate the binary 2nd complement string of input BigInt
    val seq_str = seq.map(v =>
      leading0(complement2nd(v,single_data_width).toString(2), single_data_width)
    )
    // Check length matched
    require(seq.length == seq_length, s"Input sequence length (${seq.length}) " +
      s"!= given length ($seq_length)")
    // Check bit length of value matched
    require(seq_str.forall{str => str.length <= single_data_width}, s"Max bit length " +
      s"of input BigInt sequence (${seq_str.map(_.length) max}) >= given width " +
      s"($single_data_width)")
    // Concat to big string
    val full_str : String = seq_str.reduceLeft((l, h) => h + l)
    // Make the bit length matched
    BigInt(full_str, 2).width(seq_length * single_data_width)
  }

  /**
   * Convert a sequence of BigInt of certain width to a sequence of BigInt of
   * another width
   * @param seq Input sequence of BigInt
   * @param old_width Input width of each BigInt in seq
   * @param new_width Output width of each BigInt in seq
   * @return BigInt sequence whose width is new width
   */
  def BigIntSeqReform(seq: Seq[BigInt],
                      old_width : Int,
                      new_width : Int) : Seq[BigInt] = {
    /* Requirement Check */
    // The width of original sequence should not be wider than old width
    require(seq.forall(_.bitLength <= old_width),
      s"The bit width of input is wider than the old width set")
    // If you use finer data to form coarser data, the sequence should be multiple of
    // data width ratio long
    if(old_width < new_width){// Finer to Coarse -> Merge to Coarse Data
      // Coarse width needs to be multiple of narrow width
      require(new_width % old_width == 0,
        s"Wider width ($new_width) needs to be multiple of narrower width ($old_width)")
      val ratio : Int = new_width / old_width
      // Fine to Coarse: Length needs to be multiple of ratio
      require(seq.length % ratio == 0,
        s"New width ($new_width) is $ratio times of old width ($old_width), " +
          s"but the length of BigInt sequence is not multiple of $ratio")
      // Group finer data into coarse data width
      seq.grouped(ratio).map{ group => Seq2BigInt(group,ratio,old_width)}.toSeq
    }else if(old_width == new_width){// Not changed
      seq
    }else{// Coarse data width split into fine data
      // Calculate ratio
      val ratio = old_width / new_width
      seq.flatMap{data => BigInt2Seq(data,ratio,new_width)}
    }
  }

  /**
   * Mask the BigInt sequence
   * @param n_seq new sequence
   * @param o_seq old sequence
   * @param mask mask sequence
   * @return masked BigInt sequence
   */
  def MaskSeqBigInt(n_seq : Seq[BigInt], o_seq : Seq[BigInt], mask:Seq[Boolean])
  : Seq[BigInt] = {
    require(n_seq.length == o_seq.length && o_seq.length == mask.length)
    n_seq zip o_seq zip mask map { case ((n, o), m) => if(m) n else o}
  }
  def MaskSeqBigInt(n_seq : Seq[BigInt], o_seq : Seq[BigInt], mask:BigInt)
  : Seq[BigInt] = {
    val seq_m : Seq[Int] =
      BigInt2Seq(mask,n_seq.length, 1).map(_.toInt)
    val seq_b : Seq[Boolean] = seq_m.map{
      case 1 => true
      case 0 => false
      case _ => assert(assertion = false, s"mask seq parsed incorrectly");false
    }
    MaskSeqBigInt(n_seq, o_seq, seq_b)
  }

  /**
   * Return an non-zero BigInt value with given numBits
   * @param numBits The number of bits for BigInt
   * @return as name of function
   */
  def RandomNotZeroBigInt(numBits : Int) : BigInt = {
    var potential : BigInt = BigInt(numBits, scala.util.Random)
    while(potential == BigInt(0)){
      potential = BigInt(numBits, scala.util.Random)
    }
    potential
  }

  /**
   * Check whether something is supported and larger than minimum value and
   * is also power of two; If support, check; otherwise, return  true
   * @param value To be checked value
   * @param support Support whether or not
   * @param min minimum value, value should larger than (does not equal)
   * @param name The name of this value
   * @return Satisfied or not
   */
  def support_min_pow2(value : BigInt, support : Boolean,
                       min : Int, name : String) : Boolean ={
    val check : Boolean = (!support) || (value > min && isPow2(value))
    require(check,
      s"$name need to be larger than $min and should be power of 2, " +
        s"but yours is $value")
    check
  }

  /**
   * When you have a hardware value, which is XLEN-bit wide.
   *
   * If it is an unsigned value, the maximum is max_unsigned, and the value
   * will satisfy: 0 `<=` value `<` max_unsigned
   *
   * If it is a signed value, the maximum is max_signed, and the value will be
   * constrained as; -max_signed `<=` value `<` max_signed
   *
   * @param width width of value
   * @return max_signed and max_unsigned value
   */
  def get_max_unsigned_signed(width : Int) : (BigInt, BigInt) = {
    (BigInt(1) << width, BigInt(1) << (width - 1))
  }
}

