package com.googlecode.aviator.runtime.function;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Reflector;

/**
 * An aviator function wraps a class's static method.
 *
 * @since 4.2.2
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ClassMethodFunction extends AbstractVariadicFunction {

  private static final long serialVersionUID = 5946505010078966461L;
  private MethodHandle handle; // Only for one-arity function.
  private Class<?>[] pTypes;
  private String name;
  private String methodName;
  private List<Method> methods; // For reflection.
  private Class<?> clazz;
  private boolean isStatic;

  public ClassMethodFunction(final Class<?> clazz, final boolean isStatic, final String name,
      final String methodName, final List<Method> methods)
      throws IllegalAccessException, NoSuchMethodException {
    this.name = name;
    this.clazz = clazz;
    this.isStatic = isStatic;
    this.methodName = methodName;

    init(isStatic, methodName, methods);
  }

  private void init(final boolean isStatic, final String methodName, final List<Method> methods)
      throws IllegalAccessException, NoSuchMethodException {
    if (methods.size() == 1) {
      // fast path by method handle.
      this.handle = MethodHandles.lookup().unreflect(methods.get(0)).asFixedArity();
      this.pTypes = methods.get(0).getParameterTypes();
      if (!isStatic) {
        Class<?>[] newTypes = new Class<?>[this.pTypes.length + 1];
        newTypes[0] = this.clazz;
        System.arraycopy(this.pTypes, 0, newTypes, 1, this.pTypes.length);
        this.pTypes = newTypes;
      }
      if (this.handle == null) {
        throw new NoSuchMethodException("Method handle for " + methodName + " not found");
      }
    } else {
      // Slow path by reflection
      this.methods = methods;
    }
  }

  private void readObject(ObjectInputStream input) throws ClassNotFoundException, IOException {
    this.name = (String) input.readObject();
    this.clazz = (Class<?>) input.readObject();
    this.isStatic = input.readBoolean();
    this.methodName = (String) input.readObject();

    Map<String, List<Method>> allMethods = Reflector.findMethodsFromClass(clazz, isStatic);

    List<Method> methods = allMethods.get(this.methodName);
    if (methods == null) {
      methods = Collections.emptyList();
    }

    try {
      this.init(this.isStatic, this.methodName, methods);
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

  private void writeObject(ObjectOutputStream output) throws IOException {
    output.writeObject(this.name);
    output.writeObject(this.clazz);
    output.writeBoolean(this.isStatic);
    output.writeObject(this.methodName);
  }

  @Override
  public String getName() {
    return this.name;
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    Object[] jArgs = null;

    Object target = null;

    if (this.isStatic || this.handle != null) {
      jArgs = new Object[args.length];
      for (int i = 0; i < args.length; i++) {
        jArgs[i] = args[i].getValue(env);
      }
    } else {
      if (args.length < 1) {
        throw new IllegalArgumentException("Class<" + this.clazz + "> instance method "
            + this.methodName + " needs at least one argument as instance.");
      }
      jArgs = new Object[args.length - 1];
      target = args[0].getValue(env);
      for (int i = 1; i < args.length; i++) {
        jArgs[i - 1] = args[i].getValue(env);
      }
    }

    if (this.handle != null) {
      try {
        return FunctionUtils
            .wrapReturn(this.handle.invokeWithArguments(Reflector.boxArgs(this.pTypes, jArgs)));
      } catch (Throwable t) {
        throw Reflector.sneakyThrow(t);
      }
    } else {
      return FunctionUtils.wrapReturn(this.isStatic
          ? Reflector.invokeStaticMethod(this.clazz, this.methodName, this.methods, jArgs)
          : Reflector.invokeInstanceMethod(this.clazz, this.methodName, target, this.methods,
              jArgs));
    }
  }

}
