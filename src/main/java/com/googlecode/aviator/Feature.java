package com.googlecode.aviator;

import java.util.HashSet;
import java.util.Set;

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
  Return,
  /**
   * if/elsif/else statement
   */
  If,
  /**
   * for loop statement
   */
  ForLoop,
  /**
   * while statement
   */
  WhileLoop,
  /**
   * Lexical scope
   */
  LexicalScope,
  /**
   * fn to define named function
   */
  Fn,
  /**
   * lambda to define function
   */
  Lambda,
  /**
   * Internal vars such as __env__, __instance__
   */
  InternalVars,
  /**
   * module system such as exports/require/load function supporting.
   */
  Module;


  /**
   * Create a feature set from arguments.
   *
   * @param args
   * @return feature set
   */
  public static Set<Feature> asSet(final Feature... args) {
    Set<Feature> set = new HashSet<>();
    for (Feature f : args) {
      set.add(f);
    }
    return set;
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
