package com.googlecode.aviator.runtime.type.seq;

import java.util.Iterator;
import com.googlecode.aviator.runtime.type.Sequence;

/**
 * Impl {@link Object#toString()} for sub-classes sequence.
 *
 * @author dennis(killme2008@gmail.com)
 *
 * @param <T>
 */
public abstract class AbstractSequence<T> implements Sequence<T> {

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder(super.toString());

    sb.append("[");
    boolean wasFirst = true;

    Iterator<T> it = iterator();
    while (it.hasNext()) {
      T e = it.next();

      if (wasFirst) {
        sb.append(e);
        wasFirst = false;
      } else {
        sb.append(", ").append(e);
      }
    }
    sb.append("]");

    return sb.toString();

  }


}
