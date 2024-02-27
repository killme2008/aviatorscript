package com.googlecode.aviator.runtime.function.seq;

import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * seq.partition(from_seq, page_size) 将seq分页，保持原有顺序
 *
 * @author zengnianmei
 */
public class SeqPartitionFunction extends AbstractFunction {
    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        Object coll = arg1.getValue(env);
        if (coll == null) {
            return AviatorNil.NIL;
        }
        Number arg2Value = (Number) arg2.getValue(env);
        int pageSize = arg2Value.intValue();
        List<List<Object>> result = new ArrayList<>();
        int index = 0;
        for (Object element : RuntimeUtils.seq(coll, env)) {
            if (index % pageSize == 0) {
                List<Object> list = new ArrayList<>();
                result.add(list);
            }
            result.get(index / pageSize).add(element);
            index++;
        }

        return AviatorRuntimeJavaType.valueOf(result);
    }

    @Override
    public String getName() {
        return "seq.partition";
    }
}
