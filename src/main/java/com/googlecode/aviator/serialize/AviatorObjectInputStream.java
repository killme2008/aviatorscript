package com.googlecode.aviator.serialize;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectStreamClass;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.BaseExpression;
import com.googlecode.aviator.ClassExpression;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.ExpressionAccessor;
import com.googlecode.aviator.code.asm.ClassDefiner;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.runtime.type.AviatorBigInt;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.Range;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.Reflector;

/**
 * A special ObjectInputStream that loads a class based on the AvaitorClassLoader rather than the
 * system default.
 * 
 * @since 5.3.4
 * @author dennis
 *
 */
public class AviatorObjectInputStream extends ObjectInputStream {
  private AviatorClassLoader classLoader;
  private AviatorEvaluatorInstance instance;
  private Map<String, byte[]> classBytesCache = new HashMap<String, byte[]>();

  public AviatorObjectInputStream(InputStream in, AviatorEvaluatorInstance instance)
      throws IOException {
    super(in);
    this.classLoader = instance.getAviatorClassLoader(true);
    this.instance = instance;
    this.enableResolveObject(true);
  }

  @Override
  protected Object resolveObject(Object obj) throws IOException {
    Object object = super.resolveObject(obj);
    if (object instanceof BaseExpression) {
      BaseExpression exp = (BaseExpression) object;
      configureExpression(exp);
    }
    if (object instanceof Env) {
      ((Env) object).setInstance(this.instance);
    }

    // Processing some internal constants.
    if (object instanceof AviatorBoolean) {
      AviatorBoolean bool = (AviatorBoolean) object;
      if (bool.getBooleanValue()) {
        object = AviatorBoolean.TRUE;
      } else {
        object = AviatorBoolean.FALSE;
      }
    }
    if (object instanceof AviatorNil) {
      object = AviatorNil.NIL;
    }

    if (object instanceof Range) {
      if (((Range) object).isLoop()) {
        object = Range.LOOP;
      }
    }

    if (object instanceof Variable) {
      object = SymbolTable.tryReserveKeyword((Variable) object);
    }

    return object;
  }

  private void configureExpression(BaseExpression exp) {
    ExpressionAccessor.setInstance(exp, this.instance);
    if (exp instanceof ClassExpression) {
      ((ClassExpression) exp).setClassBytes(this.classBytesCache.get(exp.getClass().getName()));
    }
  }

  @Override
  protected Class<?> resolveClass(ObjectStreamClass desc)
      throws IOException, ClassNotFoundException {
    Class<?> clazz = null;
    try {
      clazz = super.resolveClass(desc);
    } catch (ClassNotFoundException e) {

    }
    if (clazz == null && desc.getName().startsWith("AviatorScript_")) {
      int len = this.readInt();
      byte[] classBytes = new byte[len];

      int readed = 0;
      while (readed < classBytes.length) {
        int n = this.read(classBytes, readed, classBytes.length - readed);
        if (n < 0) {
          break;
        }
        readed += n;
      }

      String name = desc.getName();
      this.classBytesCache.put(name, classBytes);
      try {
        // already in class loader
        clazz = Class.forName(name, false, this.classLoader);
      } catch (ClassNotFoundException ex) {

      }
      if (clazz == null) {
        // still not found, try to define it.
        try {
          clazz = ClassDefiner.defineClass(desc.getName(), Expression.class, classBytes,
              this.classLoader, true);
        } catch (Throwable t) {
          throw Reflector.sneakyThrow(t);
        }
      }

    }
    if (clazz == null) {
      throw new ClassNotFoundException("Class not found:" + desc.getName());
    }
    return clazz;
  }

}
