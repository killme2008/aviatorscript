package com.googlecode.aviator.runtime.function.seq.set;

import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;


/**
 * intersection
 *
 * @author bluecrush
 */
public class SetIntersectionFunction extends AbstractFunction {
    @Override
    public String getName() {
        return "set.intersection";
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        Object seq1 = arg1.getValue(env);
        Object seq2 = arg2.getValue(env);
        if (arg1 != null && !Set.class.isAssignableFrom(seq1.getClass())) {
            throw new IllegalArgumentException("arg1 `" + seq1 + "` it's not a seq.");
        }
        if (arg2 != null && !Set.class.isAssignableFrom(seq2.getClass())) {
            throw new IllegalArgumentException("arg2 `" + seq2 + "` it's not a seq.");
        }
        Set set1 = (Set) seq1;
        Set set2 = (Set) seq2;
        Map map1 = SetUtil.getCardinalityMap(set1);
        Map map2 = SetUtil.getCardinalityMap(set2);
        set1.addAll(set2);
        ArrayList list = new ArrayList();
        Iterator iterator = set1.iterator();
        while (iterator.hasNext()) {
            Object obj = iterator.next();
            for (int i = 0, m = Math.min(SetUtil.getFreq(obj, map1), SetUtil.getFreq(obj, map2)); i < m; i++) {
                list.add(obj);
            }
        }
        return AviatorRuntimeJavaType.valueOf(list);
    }
}
