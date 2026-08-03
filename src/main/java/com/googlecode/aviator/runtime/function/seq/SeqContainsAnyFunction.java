package com.googlecode.aviator.runtime.function.seq;

import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;

import java.util.Collection;
import java.util.Map;


/**
 * Is there an intersection
 *
 * @author bluecrush
 */
public class SeqContainsAnyFunction extends AbstractSeqOperationFunction {
    @Override
    public String getName() {
        return "seqUtil.containsAny";
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        Object seq1 = arg1.getValue(env);
        Object seq2 = arg2.getValue(env);
        if (!Collection.class.isAssignableFrom(seq1.getClass())) {
            throw new IllegalArgumentException("arg1 `" + seq1 + "` it's not a seq.");
        }
        if (!Collection.class.isAssignableFrom(seq2.getClass())) {
            throw new IllegalArgumentException("arg2 `" + seq2 + "` it's not a seq.");
        }
        Collection<?> coll1 = (Collection<?>) seq1;
        Collection<?> coll2 = (Collection<?>) seq2;
        if (coll1.size() < coll2.size()) {
            for (Object o : coll1) {
                if (coll2.contains(o)) {
                    return AviatorBoolean.TRUE;
                }
            }
        } else {
            for (Object o : coll2) {
                if (coll1.contains(o)) {
                    return AviatorBoolean.TRUE;
                }
            }
        }
        return AviatorBoolean.FALSE;
    }
}
