package com.googlecode.aviator.spring;

import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import com.googlecode.aviator.FunctionLoader;
import com.googlecode.aviator.runtime.type.AviatorFunction;

/**
 * Function loader based on spring context, try to find the function by name from spring context.
 *
 * @since 4.0.0
 * @author dennis
 *
 */
public class SpringContextFunctionLoader implements FunctionLoader, ApplicationContextAware {

  private ApplicationContext applicationContext;


  public SpringContextFunctionLoader() {
    super();
  }


  public SpringContextFunctionLoader(final ApplicationContext applicationContext) {
    super();
    this.applicationContext = applicationContext;
  }


  public ApplicationContext getApplicationContext() {
    return this.applicationContext;
  }


  @Override
  public void setApplicationContext(final ApplicationContext applicationContext) {
    this.applicationContext = applicationContext;
  }


  @Override
  public AviatorFunction onFunctionNotFound(final String name) {
    try {
      return (AviatorFunction) this.applicationContext.getBean(name);
    } catch (NoSuchBeanDefinitionException e) {
      return null;
    }
  }

}
