package com.googlecode.aviator.runtime.function.seq;

import com.googlecode.aviator.runtime.function.AbstractFunction;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Base class for min/max function.
 *
 * @author dennis
 */
public abstract class AbstractSeqOperationFunction extends AbstractFunction {


    /**
     * Constant to avoid repeated object creation
     */
    private static final Integer INTEGER_ONE = 1;

  /**
   * Gets cardinality map.
   *
   * @param coll the coll
   * @return the cardinality map
   */
  public Map<Object, Integer> getCardinalityMap(final Collection<?> coll) {
        Map<Object, Integer> count = new HashMap<>(coll.size());
        for (Object obj : coll) {
            Integer c = count.get(obj);
            if (c == null) {
                count.put(obj, INTEGER_ONE);
            } else {
                count.put(obj, c + 1);
            }
        }
        return count;
    }

  /**
   * Gets freq.
   *
   * @param obj     the obj
   * @param freqMap the freq map
   * @return the freq
   */
  public int getFreq(final Object obj, final Map<?, ?> freqMap) {
        Integer count = (Integer) freqMap.get(obj);
        if (count != null) {
            return count;
        }
        return 0;
    }
}
