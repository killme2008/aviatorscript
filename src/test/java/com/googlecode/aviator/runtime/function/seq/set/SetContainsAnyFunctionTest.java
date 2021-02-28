package com.googlecode.aviator.runtime.function.seq.set;

import com.googlecode.aviator.AviatorEvaluator;
import junit.framework.TestCase;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class SetContainsAnyFunctionTest extends TestCase {

    public void testCall() {
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
        Object execute = AviatorEvaluator.execute("set.union(set1,set2)", env);
        System.out.println(execute);
    }
}