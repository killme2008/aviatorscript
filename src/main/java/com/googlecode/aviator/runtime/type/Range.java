package com.googlecode.aviator.runtime.type;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import com.googlecode.aviator.exception.CompareNotSupportedException;
import com.googlecode.aviator.utils.ArrayUtils;
import com.googlecode.aviator.utils.Env;

/**
 * A range in [start, end) with step.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 5.0.0
 */
public final class Range extends AviatorObject implements Sequence<Number> {


  private static final long serialVersionUID = 1463899968843425932L;
  private static final AviatorLong ZERO = AviatorLong.valueOf(0L);
  public static final Range LOOP = new Range(ZERO, ZERO, ZERO);

  static {
    LOOP.isLoop = true;
  }

  private final AviatorNumber step;
  private final AviatorNumber start;
  private final AviatorNumber end;
  final boolean forward;
  private boolean isLoop;

  public boolean isLoop() {
    return this.isLoop;
  }

  public Range(final AviatorNumber start, final AviatorNumber end, final AviatorNumber step) {
    super();
    this.start = start;
    this.end = end;
    this.step = step;
    this.forward = Range.this.step.compare(ZERO, Env.EMPTY_ENV) >= 0;
    this.isLoop = false;
  }

  @Override
  public String desc(final Map<String, Object> env) {
    return "<Range, [" + this.start.getValue(env) + ", " + this.end.getValue(env) + "], "
        + this.step.getValue(env) + ">";
  }


  @Override
  public int innerCompare(final AviatorObject other, final Map<String, Object> env) {
    throw new CompareNotSupportedException();
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
  public int hintSize() {
    try {
      return size();
    } catch (Throwable t) {
      return 10;
    }
  }

  public int size() {
    int size =
        ((Number) this.end.sub(this.start, null).div(this.step, null).getValue(null)).intValue();
    return size < 0 ? 0 : size;
  }

  @Override
  public Collector newCollector(final int size) {
    if (size <= 0) {
      return new Collector() {
        List<Object> list = new ArrayList<>();

        @Override
        public void add(final Object e) {
          this.list.add(e);
        }

        @Override
        public Object getRawContainer() {
          return this.list;
        }
      };
    } else {
      return new Collector() {
        Object array = Array.newInstance(Object.class, size);
        int i = 0;

        @Override
        public void add(final Object e) {
          ArrayUtils.set(this.array, this.i++, e);
        }

        @Override
        public Object getRawContainer() {
          return this.array;
        }
      };
    }
  }

  @Override
  public Iterator<Number> iterator() {
    return new Iterator<Number>() {
      AviatorNumber current = Range.this.start;

      @Override
      public boolean hasNext() {
        if (Range.this.forward) {
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
      public Number next() {
        if (!hasNext()) {
          throw new NoSuchElementException();
        }
        AviatorNumber result = this.current;
        this.current = (AviatorNumber) this.current.add(Range.this.step, Env.EMPTY_ENV);
        return (Number) result.getValue(Env.EMPTY_ENV);
      }

    };
  }


}
