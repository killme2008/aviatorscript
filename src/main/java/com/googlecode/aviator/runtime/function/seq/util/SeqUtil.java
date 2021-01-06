package com.googlecode.aviator.runtime.function.seq.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * CollectionUtil
 */
class SeqUtil {
    /**
     * Constant to avoid repeated object creation
     */
    private static final Integer INTEGER_ONE = Integer.valueOf(1);

    public static Map getCardinalityMap(final Collection coll) {
        Map count = new HashMap(coll.size());
        for (Iterator it = coll.iterator(); it.hasNext(); ) {
            Object obj = it.next();
            Integer c = (Integer) (count.get(obj));
            if (c == null) {
                count.put(obj, INTEGER_ONE);
            } else {
                count.put(obj, Integer.valueOf(c.intValue() + 1));
            }
        }
        return count;
    }

    public static final int getFreq(final Object obj, final Map freqMap) {
        Integer count = (Integer) freqMap.get(obj);
        if (count != null) {
            return count.intValue();
        }
        return 0;
    }
}
