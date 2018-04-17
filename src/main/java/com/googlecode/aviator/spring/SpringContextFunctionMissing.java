package com.googlecode.aviator.spring;

import org.springframework.context.ApplicationContext;
import com.googlecode.aviator.FunctionMissing;
import com.googlecode.aviator.runtime.type.AviatorFunction;

/**
 * Function missing based on spring context, try to find the function by name from spring context.
 * 
 * @author dennis
 *
 */
public class SpringContextFunctionMissing implements FunctionMissing {

  private ApplicationContext applicationContext;


  public SpringContextFunctionMissing() {
    super();
  }


  public SpringContextFunctionMissing(ApplicationContext applicationContext) {
    super();
    this.applicationContext = applicationContext;
  }


  public ApplicationContext getApplicationContext() {
    return applicationContext;
  }


  public void setApplicationContext(ApplicationContext applicationContext) {
    this.applicationContext = applicationContext;
  }


  @Override
  public AviatorFunction onFunctionMissing(String name) {
    return (AviatorFunction) this.applicationContext.getBean(name);
  }

}
