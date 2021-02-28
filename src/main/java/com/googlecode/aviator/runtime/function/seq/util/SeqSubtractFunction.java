package com.googlecode.aviator.runtime.function.seq.util;

import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;


/**
 * Subtract
 *
 * @author bluecrush
 */
public class SeqSubtractFunction extends AbstractFunction {
    @Override
    public String getName() {
        return "seqUtil.subtract";
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
        ArrayList list = new ArrayList( collection1 );
        for (Iterator it = collection2.iterator(); it.hasNext();) {
            list.remove(it.next());
        }
        return AviatorRuntimeJavaType.valueOf(list);
    }
}
