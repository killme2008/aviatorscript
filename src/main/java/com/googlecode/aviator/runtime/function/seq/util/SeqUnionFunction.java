package com.googlecode.aviator.runtime.function.seq.util;

import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

import java.util.*;


/**
 * Union
 *
 * @author bluecrush
 */
public class SeqUnionFunction extends AbstractFunction {
    @Override
    public String getName() {
        return "seqUtil.union";
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
        Map map1 = SeqUtil.getCardinalityMap(collection1);
        Map map2 = SeqUtil.getCardinalityMap(collection2);
        Set set1 = new HashSet(collection1);
        set1.addAll(collection2);
        ArrayList list = new ArrayList();
        Iterator iterator = set1.iterator();
        while (iterator.hasNext()) {
            Object obj = iterator.next();
            for (int i = 0, m = Math.max(SeqUtil.getFreq(obj, map1), SeqUtil.getFreq(obj, map2)); i < m; i++) {
                list.add(obj);
            }
        }
        return AviatorRuntimeJavaType.valueOf(list);
    }
}
