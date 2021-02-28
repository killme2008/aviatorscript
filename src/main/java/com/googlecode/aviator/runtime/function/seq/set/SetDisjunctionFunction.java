package com.googlecode.aviator.runtime.function.seq.set;

import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;


/**
 *
 * Complement of intersection
 *
 * @author bluecrush
 */
public class SetDisjunctionFunction extends AbstractFunction {
    @Override
    public String getName() {
        return "set.disjunction";
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
        Map map1 = SetUtil.getCardinalityMap(collection1);
        Map map2 = SetUtil.getCardinalityMap(collection2);
        Set set1 = new HashSet(collection1);
        set1.addAll(collection2);
        ArrayList list = new ArrayList();
        Iterator iterator = set1.iterator();
        while (iterator.hasNext()) {
            Object obj = iterator.next();
            for (int i = 0, m = ((Math.max(SetUtil.getFreq(obj, map1), SetUtil.getFreq(obj, map2))) - (Math.min(SetUtil.getFreq(obj, map1), SetUtil.getFreq(obj, map2)))); i < m; i++) {
                list.add(obj);
            }
        }
        return AviatorRuntimeJavaType.valueOf(list);
    }



}
