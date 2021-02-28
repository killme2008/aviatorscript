package com.googlecode.aviator.runtime.function.seq.util;

import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;


/**
 * Are the two sets the same
 *
 * @author bluecrush
 */
public class SeqIsEqualFunction extends AbstractFunction {
    @Override
    public String getName() {
        return "seqUtil.isEqual";
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
        Collection collection1 = (Collection) seq1;
        Collection collection2 = (Collection) seq2;
        if(collection1.size() != collection2.size()) {
            return AviatorBoolean.FALSE;
        } else {
            Map map1 = SeqUtil.getCardinalityMap(collection1);
            Map map2 = SeqUtil.getCardinalityMap(collection2);
            if(map1.size() != map2.size()) {
                return AviatorBoolean.FALSE;
            } else {
                Iterator it = map1.keySet().iterator();
                while(it.hasNext()) {
                    Object obj = it.next();
                    if(SeqUtil.getFreq(obj,map1) != SeqUtil.getFreq(obj,map2)) {
                        return AviatorBoolean.FALSE;
                    }
                }
                return AviatorBoolean.TRUE;
            }
        }
    }
}
