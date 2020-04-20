package com.googlecode.aviator;

import java.util.Collections;
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
  Module,
  /**
   * try..catch..finally statement to handle errors.
   */
  TryCatchFinally;

  /**
   * Require feature sets for this feature.
   */
  private Set<Feature> prequires = Collections.emptySet();

  private Feature() {

  }

  private Feature(final Set<Feature> prequires) {
    this.prequires = prequires;
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
