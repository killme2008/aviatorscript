package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.Env;

/**
 * __use(pkg, ...names) function to implement import use
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class UseFunction extends AbstractVariadicFunction {

  private static final long serialVersionUID = 1710427343500339000L;

  private UseFunction() {

  }

  public static final UseFunction INSTANCE = new UseFunction();


  /**
   * use package.{class1, class2};
   */
  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    if (args.length < 3) {
      throw new IllegalArgumentException(
          "Wrong arguments(" + args.length + ") passed to __use variadicCall");
    }
    if (args[0].getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException("Can't use other aviator type except varaible");
    }
    final String packageSym = ((AviatorJavaType) args[0]).getName();

    assert (env instanceof Env);
    final Env theEnv = (Env) env;

    for (int i = 1; i < args.length; i++) {
      if (args[i].getAviatorType() != AviatorType.JavaType) {
        throw new IllegalArgumentException("Can't use other aviator type except varaible");
      }
      final String name = ((AviatorJavaType) args[i]).getName();
      addSym(theEnv, packageSym, name);
    }
    return AviatorNil.NIL;
  }

  private void addSym(final Env theEnv, final String packageSym, final String name) {
    if (name.equals("*")) {
      theEnv.addPackageSymbol(packageSym);
    } else {
      theEnv.addSymbol(packageSym + name);
    }
  }

  /**
   * use package.class;
   */
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {

    if (arg1.getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException("Can't use other aviator type except varaible");
    }

    final String sym = ((AviatorJavaType) arg1).getName();

    assert (env instanceof Env);

    ((Env) env).addSymbol(sym);
    return AviatorNil.NIL;
  }



  /**
   * use package.* or use.package.{class};
   */
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    if (arg1.getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException("Can't use other aviator type except varaible");
    }
    if (arg2.getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException("Can't use other aviator type except varaible");
    }

    final String packageSym = ((AviatorJavaType) arg1).getName();
    final String name = ((AviatorJavaType) arg2).getName();
    assert (env instanceof Env);
    final Env theEnv = (Env) env;

    addSym(theEnv, packageSym, name);

    return AviatorNil.NIL;
  }



  @Override
  public String getName() {
    return Constants.USE_VAR.getLexeme();
  }

}
