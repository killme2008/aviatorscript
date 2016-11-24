package com.googlecode.aviator.runtime.function.custom;

import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;


/**
 * From user's issue report https://github.com/killme2008/aviator/issues/12
 * 
 * @author dennis
 *
 */
public class GetFirstNonNullFunction extends AbstractFunction {

    private AviatorObject firstNonNull(Map<String, Object> env, AviatorObject... args) {

        if (args != null) {
            for (AviatorObject arg : args) {
                if (arg.getValue(env) != null) {
                    return arg;
                }
            }
        }
        return new AviatorString(null);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        return firstNonNull(env, arg1, arg2);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3) {
        return firstNonNull(env, arg1, arg2, arg3);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4) {
        return firstNonNull(env, arg1, arg2, arg3, arg4);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13,
            AviatorObject arg14) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
            arg14);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13,
            AviatorObject arg14, AviatorObject arg15) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
            arg14, arg15);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13,
            AviatorObject arg14, AviatorObject arg15, AviatorObject arg16) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
            arg14, arg15, arg16);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13,
            AviatorObject arg14, AviatorObject arg15, AviatorObject arg16, AviatorObject arg17) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
            arg14, arg15, arg16, arg17);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13,
            AviatorObject arg14, AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
            arg14, arg15, arg16, arg17, arg18);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13,
            AviatorObject arg14, AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
            AviatorObject arg19) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
            arg14, arg15, arg16, arg17, arg18, arg19);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13,
            AviatorObject arg14, AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
            AviatorObject arg19, AviatorObject arg20) {
        return firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
            arg14, arg15, arg16, arg17, arg18, arg19, arg20);
    }


    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2, AviatorObject arg3,
            AviatorObject arg4, AviatorObject arg5, AviatorObject arg6, AviatorObject arg7, AviatorObject arg8,
            AviatorObject arg9, AviatorObject arg10, AviatorObject arg11, AviatorObject arg12, AviatorObject arg13,
            AviatorObject arg14, AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
            AviatorObject arg19, AviatorObject arg20, AviatorObject... args) {

        AviatorObject result =
                firstNonNull(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
                    arg14, arg15, arg16, arg17, arg18, arg19, arg20);
        if (result.getValue(env) != null) {
            return result;
        }
        else {
            return firstNonNull(env, args);
        }
    }


    @Override
    public String getName() {
        return "getFirstNonNull";
    }

}
