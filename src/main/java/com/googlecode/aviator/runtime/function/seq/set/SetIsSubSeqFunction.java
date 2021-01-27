package com.googlecode.aviator.runtime.function.seq.set;

import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;


/**
 * Is <arg2/> a subset of <arg1/>
 *
 * @author bluecrush
 */
public class SetIsSubSeqFunction extends AbstractFunction {
    @Override
    public String getName() {
        return "set.isSubSeq";
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        Object seq1 = arg1.getValue(env);
        Object seq2 = arg2.getValue(env);
        if (arg1 != null && !Collection.class.isAssignableFrom(seq1.getClass())) {
            throw new IllegalArgumentException("arg1 `" + seq1 + "` it's not a seq.");
        }
        if (arg2 != null && !Collection.class.isAssignableFrom(seq2.getClass())) {
            throw new IllegalArgumentException("arg2 `" + seq2 + "` it's not a seq.");
        }
        Collection coll1 = (Collection) seq1;
        Collection coll2 = (Collection) seq2;
        Map map1 = SetUtil.getCardinalityMap(coll1);
        Map map2 = SetUtil.getCardinalityMap(coll2);
        Iterator it = coll1.iterator();
        while (it.hasNext()) {
            Object obj = it.next();
            if (SetUtil.getFreq(obj, map1) > SetUtil.getFreq(obj, map2)) {
                return AviatorBoolean.FALSE;
            }
        }
        return AviatorBoolean.TRUE;
    }
}
