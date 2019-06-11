package com.googlecode.aviator.spring;

import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import com.googlecode.aviator.FunctionLoader;
import com.googlecode.aviator.runtime.type.AviatorFunction;

/**
 * Function loader based on spring context, try to find the function by name from spring context.
 *
 * @since 4.0.0
 * @author dennis
 *
 */
public class SpringContextFunctionLoader implements FunctionLoader {

  private ApplicationContext applicationContext;


  public SpringContextFunctionLoader() {
    super();
  }


  public SpringContextFunctionLoader(ApplicationContext applicationContext) {
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
  public AviatorFunction onFunctionNotFound(String name) {
    try {
      return (AviatorFunction) this.applicationContext.getBean(name);
    } catch (NoSuchBeanDefinitionException e) {
      return null;
    }
  }

}
