package com.googlecode.aviator;

/**
 * Expression engine evaluate mode
 *
 * @author dennis(killme2008@gmail.com)
 * @since 5.3
 */
public enum EvalMode {
  /**
   * Generate JVM byecode by ASM and eval it by JVM.
   */
  ASM,
  /**
   * Interpreter mode.
   */
  INTERPRETER
}
