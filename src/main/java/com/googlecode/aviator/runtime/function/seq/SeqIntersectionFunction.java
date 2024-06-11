package com.googlecode.aviator.runtime.function.seq;

import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;


/**
 * intersection
 *
 * @author bluecrush
 */
public class SeqIntersectionFunction extends AbstractSeqOperationFunction {
    @Override
    public String getName() {
        return "seqUtil.intersection";
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
        Collection<?> collection1 = (Collection<?>) seq1;
        Collection<?>  collection2 = (Collection<?>) seq2;
        Map<?,?> map1 = getCardinalityMap(collection1);
        Map<?,?> map2 = getCardinalityMap(collection2);
        Set<Object> set1 = new HashSet<>(collection1);
        set1.addAll(collection2);
        List<Object> list = new ArrayList<>();
        for (Object obj : set1) {
            for (int i = 0, m = Math.min(getFreq(obj, map1), getFreq(obj, map2)); i < m; i++) {
                list.add(obj);
            }
        }
        return AviatorRuntimeJavaType.valueOf(list);
    }
}
