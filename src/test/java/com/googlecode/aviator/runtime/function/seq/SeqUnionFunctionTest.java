package com.googlecode.aviator.runtime.function.seq;

import com.googlecode.aviator.AviatorEvaluator;
import org.junit.Test;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class SeqUnionFunctionTest {

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
        Object execute = AviatorEvaluator.execute("seqUtil.union(set1,set2)", env);
        System.out.println(execute);
    }
}