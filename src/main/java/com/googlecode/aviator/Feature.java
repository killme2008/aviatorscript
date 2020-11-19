package com.googlecode.aviator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import com.googlecode.aviator.runtime.function.internal.CatchHandlerFunction;
import com.googlecode.aviator.runtime.function.internal.IfCallccFunction;
import com.googlecode.aviator.runtime.function.internal.NewInstanceFunction;
import com.googlecode.aviator.runtime.function.internal.ReducerBreakFunction;
import com.googlecode.aviator.runtime.function.internal.ReducerContFunction;
import com.googlecode.aviator.runtime.function.internal.ReducerFunction;
import com.googlecode.aviator.runtime.function.internal.ReducerReturnFunction;
import com.googlecode.aviator.runtime.function.internal.ThrowFunction;
import com.googlecode.aviator.runtime.function.internal.TryCatchFunction;
import com.googlecode.aviator.runtime.function.internal.UseFunction;
import com.googlecode.aviator.runtime.function.system.LoadFunction;
import com.googlecode.aviator.runtime.function.system.RequireFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;

/**
 * Syntax features.
 *
 * @author dennis(killme2008@gmaill.com)
 *
 */
public enum Feature {


  /**
   * variable assignment
   */
  Assignment,
  /**
   * return statement
   */
  Return(asList(ReducerReturnFunction.INSTANCE)),
  /**
   * if/elsif/else statement
   */
  If(asList(IfCallccFunction.INSTANCE)),
  /**
   * for loop statement
   */
  ForLoop(asList(ReducerFunction.INSTANCE, ReducerBreakFunction.INSTANCE,
      ReducerContFunction.INSTANCE, ReducerReturnFunction.INSTANCE)),
  /**
   * while statement
   */
  WhileLoop(asList(ReducerFunction.INSTANCE, ReducerBreakFunction.INSTANCE,
      ReducerContFunction.INSTANCE, ReducerReturnFunction.INSTANCE)),
  /**
   * let statement
   */
  Let(asSet(Feature.Assignment)),
  /**
   * Lexical scope
   */
  LexicalScope,
  /**
   * lambda to define function
   */
  Lambda,
  /**
   * fn to define named function
   */
  Fn(asSet(Feature.Assignment, Feature.Lambda)),
  /**
   * Internal vars such as __env__, __instance__
   */
  InternalVars,
  /**
   * module system such as exports/require/load function supporting.
   */
  Module(asList(LoadFunction.INSTANCE, RequireFunction.INSTANCE)),
  /**
   * try..catch..finally and throw statement to handle exceptions.
   */
  ExceptionHandle(
      asList(TryCatchFunction.INSTANCE, CatchHandlerFunction.INSTANCE, ThrowFunction.INSTANCE)),
  /**
   * new Class(arguments) to create an instance of special class with arguments.
   */
  NewInstance(asList(NewInstanceFunction.INSTANCE)),
  /**
   * String interpolation.For example, "a = 'aviator'; 'hello #{a}'" to generate a string 'hello
   * aviator'
   */
  StringInterpolation(asSet(Feature.InternalVars)),
  /**
   * use package.class to import java classes into current context.
   *
   * @since 5.2.0
   */
  Use(asList(UseFunction.INSTANCE));

  /**
   * Require feature sets for this feature.
   */
  private Set<Feature> prequires = Collections.emptySet();

  /**
   * Functions to support the feature.
   */
  private List<AviatorFunction> functions = Collections.emptyList();


  private Feature() {

  }

  private static List<AviatorFunction> asList(final AviatorFunction... args) {
    List<AviatorFunction> ret = new ArrayList<>(args.length);
    for (AviatorFunction f : args) {
      ret.add(f);
    }
    return ret;
  }

  private Feature(final Set<Feature> prequires) {
    this.prequires = prequires;
  }

  private Feature(final List<AviatorFunction> funcs) {
    this.functions = funcs;
  }

  private Feature(final Set<Feature> prequires, final List<AviatorFunction> funcs) {
    this.prequires = prequires;
    this.functions = funcs;
  }

  public List<AviatorFunction> getFunctions() {
    return this.functions;
  }

  /**
   * Create a feature set from arguments.
   *
   * @param args
   * @return feature set
   */
  public static Set<Feature> asSet(final Feature... args) {
    Set<Feature> set = new HashSet<>();
    for (Feature f : args) {
      set.addAll(f.prequires);
      set.add(f);
    }
    return set;
  }

  public Set<Feature> getPrequires() {
    return this.prequires;
  }


  /**
   * Returns the full feature set.
   *
   * @return
   */
  public static Set<Feature> getFullFeatures() {
    return asSet(Feature.values());
  }

  /**
   * Returns the feature set that is compatible with aviator early versions(before 5.0).
   *
   * @return
   */
  public static Set<Feature> getCompatibleFeatures() {
    return asSet(Feature.Assignment, Feature.Lambda, Feature.InternalVars);
  }

}
