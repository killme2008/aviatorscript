package com.googlecode.aviator.runtime.function.seq.set;

import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;


/**
 * Is there an intersection
 *
 * @author bluecrush
 */
public class SetContainsAnyFunction extends AbstractFunction {
    @Override
    public String getName() {
        return "set.containsAny";
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        Object seq1 = arg1.getValue(env);
        Object seq2 = arg2.getValue(env);
        if (arg1!=null && arg1 != null && !Collection.class.isAssignableFrom(seq1.getClass())) {
            throw new IllegalArgumentException("arg1 `" + seq1 + "` it's not a seq.");
        }
        if (arg2!=null && arg2 != null && !Collection.class.isAssignableFrom(seq2.getClass())) {
            throw new IllegalArgumentException("arg2 `" + seq2 + "` it's not a seq.");
        }
        Collection coll1 = (Collection) seq1;
        Collection coll2 = (Collection) seq2;
        if (coll1.size() < coll2.size()) {
            for (Iterator it = coll1.iterator(); it.hasNext(); ) {
                if (coll2.contains(it.next())) {
                    return AviatorBoolean.TRUE;
                }
            }
        } else {
            for (Iterator it = coll2.iterator(); it.hasNext(); ) {
                if (coll1.contains(it.next())) {
                    return AviatorBoolean.TRUE;
                }
            }
        }
        return AviatorBoolean.FALSE;
    }
}
