package com.googlecode.aviator.runtime.function.seq;

/**
 * seq.max function to find the largest element in sequence.
 * 
 * @since 4.1.2
 * @author dennis
 *
 */
public class SeqMaxFunction extends AbstractSeqMinMaxFunction {

  @Override
  public String getName() {
    return "seq.max";
  }

  @Override
  protected Op getOp() {
    return Op.Max;
  }
}
