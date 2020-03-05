package com.googlecode.aviator.runtime.type;

import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import com.googlecode.aviator.utils.Env;

/**
 * A range in [start, end) with step.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 5.0.0
 */
public final class Range extends AviatorObject implements Iterable<AviatorNumber> {

  private static final AviatorLong ZERO = AviatorLong.valueOf(0L);
  public static final Range LOOP = new Range(ZERO, ZERO, ZERO);

  private final AviatorNumber step;
  private final AviatorNumber start;
  private final AviatorNumber end;


  public Range(final AviatorNumber start, final AviatorNumber end, final AviatorNumber step) {
    super();
    this.start = start;
    this.end = end;
    this.step = step;
  }

  @Override
  public String desc(final Map<String, Object> env) {
    return "<Range, [" + this.start.getValue(env) + ", " + this.end.getValue(env) + "], "
        + this.step.getValue(env) + ">";
  }


  @Override
  public int compare(final AviatorObject other, final Map<String, Object> env) {
    throw new UnsupportedOperationException();
  }



  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Range;
  }



  @Override
  public Object getValue(final Map<String, Object> env) {
    return this;
  }



  public AviatorNumber first() {
    return this.start;
  }

  public AviatorNumber last() {
    return this.end;
  }

  @Override
  public Iterator<AviatorNumber> iterator() {
    return new Iterator<AviatorNumber>() {
      AviatorNumber current = Range.this.start;

      @Override
      public boolean hasNext() {
        if (Range.this.step.compare(ZERO, Env.EMPTY_ENV) >= 0) {
          return this.current.compare(Range.this.end, Env.EMPTY_ENV) < 0;
        } else {
          return this.current.compare(Range.this.end, Env.EMPTY_ENV) > 0;
        }
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }

      @Override
      public AviatorNumber next() {
        if (!hasNext()) {
          throw new NoSuchElementException();
        }
        AviatorNumber result = this.current;
        this.current = (AviatorNumber) this.current.add(Range.this.step, Env.EMPTY_ENV);
        return result;
      }

    };
  }


}
