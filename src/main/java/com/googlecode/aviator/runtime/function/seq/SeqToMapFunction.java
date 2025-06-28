package com.googlecode.aviator.runtime.function.seq;

import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.LambdaFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

import java.util.HashMap;
import java.util.Map;

/**
 * seq.toMap(from_seq, key_mapper_lambda) Or seq.toMap(from_seq, key_mapper_lambda， value_mapper_lambda)
 *
 * @author zengnianmei
 */
public class SeqToMapFunction extends AbstractFunction {

    @Override
    public String getName() {
        return "seq.toMap";
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        Object fromSeq = arg1.getValue(env);
        if (fromSeq == null) {
            return AviatorNil.NIL;
        }
        LambdaFunction keyMapper = (LambdaFunction) arg2.getValue(env);
        if (keyMapper == null) {
            throw new IllegalArgumentException("null keyMapper");
        }
        Map<Object, Object> result = new HashMap<>();
        for (Object element : RuntimeUtils.seq(fromSeq, env)) {
            AviatorObject elementObject = AviatorRuntimeJavaType.valueOf(element);
            AviatorObject key = keyMapper.call(env, elementObject);
            result.put(key.getValue(env), element);
        }
        return AviatorRuntimeJavaType.valueOf(result);
    }

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
                              AviatorObject arg3) {
        Object fromSeq = arg1.getValue(env);
        if (fromSeq == null) {
            return AviatorNil.NIL;
        }
        LambdaFunction keyMapper = (LambdaFunction) arg2.getValue(env);
        if (keyMapper == null) {
            throw new IllegalArgumentException("null keyMapper");
        }
        LambdaFunction valueMapper = (LambdaFunction) arg3.getValue(env);
        if (valueMapper == null) {
            throw new IllegalArgumentException("null valueMapper");
        }
        Map<Object, Object> result = new HashMap<>();
        for (Object element : RuntimeUtils.seq(fromSeq, env)) {
            AviatorObject elementObject = AviatorRuntimeJavaType.valueOf(element);
            AviatorObject key = keyMapper.call(env, elementObject);
            AviatorObject value = valueMapper.call(env, elementObject);
            result.put(key.getValue(env), value.getValue(env));
        }
        return AviatorRuntimeJavaType.valueOf(result);
    }
}
