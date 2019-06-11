package com.googlecode.aviator.spring;

import org.springframework.context.ApplicationContext;
import com.googlecode.aviator.FunctionLoader;

/**
 * Function loader based on spring context, try to find the function by name from spring context.
 *
 * @since 4.0.0
 * @author dennis
 * @deprecated Use {@link SpringContextFunctionLoader} instead.
 *
 */
@Deprecated
public class SringContextFunctionLoader extends SpringContextFunctionLoader
    implements FunctionLoader {
  public SringContextFunctionLoader() {
    super();
  }

  public SringContextFunctionLoader(ApplicationContext applicationContext) {
    super(applicationContext);
  }
}
