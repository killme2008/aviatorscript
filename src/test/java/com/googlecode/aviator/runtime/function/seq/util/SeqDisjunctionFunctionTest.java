package com.googlecode.aviator.runtime.function.seq.util;

import com.googlecode.aviator.AviatorEvaluator;
import org.apache.commons.collections.CollectionUtils;
import org.junit.Test;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class SeqDisjunctionFunctionTest {

    @Test
    public void call() {
        HashSet set1 =new HashSet();
        set1.add(1);
        set1.add(2);
        set1.add(3);
        HashSet set2 =new HashSet();
        set2.add(5);
        set2.add(3);
        set2.add(4);

        Map env = new HashMap();
        env.put("set1",set1);
        env.put("set2",set2);
        Collection union = CollectionUtils.union(set1, set2);
        System.out.println(union);
        Object execute = AviatorEvaluator.execute("seqUtil.disjunction(set1,set2)", env);
        System.out.println(execute);
    }
}