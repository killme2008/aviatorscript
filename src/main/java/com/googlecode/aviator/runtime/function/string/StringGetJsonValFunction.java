package com.googlecode.aviator.runtime.function.string;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.JSONPath;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;

import java.text.ParseException;
import java.util.Map;

/**
 * StringGetJsonValFunction [Function - String]
 * 根据json路径获取json的某个key的值
 * add by Eric Han 2023-05-12
 */
public class StringGetJsonValFunction extends AbstractFunction {

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        String jsonStr = FunctionUtils.getStringValue(arg1, env);
        String jsonKeyPath = FunctionUtils.getStringValue(arg2, env);

        // 安全性检查：确保jsonStr和jsonKeyPath不为空
        if (jsonStr == null || jsonKeyPath == null) {
            return new AviatorString(""); // 返回空字符串作为安全默认值
        }

        // 替换转义的双引号，使用更安全的方式处理JSON字符串
        jsonStr = unescapeJsonString(jsonStr);

        Object value = null;
        try {
            Object obj = parseJson(jsonStr);
            if (obj != null) {
                value = JSONPath.eval(obj, "$." + jsonKeyPath);
            }
        } catch (ParseException e) {
            e.printStackTrace();
            return new AviatorString("");
        }

        return new AviatorString(value == null ? "" : value.toString());
    }

    // 将JSON字符串中的转义字符正确处理
    private String unescapeJsonString(String jsonStr) {
        // 使用专业的JSON库进行解析和转义，这里使用fastjson为例
        return JSONObject.parseObject(jsonStr).toString();
    }

    // 判断并解析JSON字符串为对象或数组
    private Object parseJson(String jsonStr) throws ParseException {
        if (jsonStr.startsWith("{")) {
            return JSONObject.parseObject(jsonStr);
        } else if (jsonStr.startsWith("[")) {
            return JSONArray.parseArray(jsonStr);
        }
        // 对于既不是对象也不是数组的字符串，返回null
        return null;
    }

    public String getName() {
        return "string.getJsonValue";
    }

    public static void main(String[] args) {
        String jsonStr = "{\"id\\\":\\\"1\\\",\\\"returnBillID\\\":\\\"12345\\\"}";

        if(jsonStr.contains("\\\"")){
            jsonStr = jsonStr.replace("\\\"","\"");
        }

        Object obj = JSON.parse(jsonStr);
        if (obj instanceof JSONObject) {
            JSONObject jsonObject = JSON.parseObject(jsonStr);
            // 使用JSONPath来获取key
            Object key = JSONPath.eval(jsonObject,"$.returnBillID");
            System.out.println(key); // 输出：value
        }else{
            JSONArray jsonArray = JSON.parseArray(jsonStr);
            // 使用JSONPath来获取key
            Object key = JSONPath.eval(jsonArray,"$.returnBillID");
            System.out.println(key); // 输出：value
        }




    }
}
