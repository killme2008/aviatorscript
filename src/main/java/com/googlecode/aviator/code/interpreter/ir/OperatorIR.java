package com.googlecode.aviator.code.interpreter.ir;

import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.InterpretContext;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.op.OperationRuntime;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * OperatorType IR
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class OperatorIR implements IR {
  private static final long serialVersionUID = 8149126448968020948L;

  private final OperatorType op;

  private AviatorFunction fn;


  public static final OperatorIR ADD = OperatorIR.valueOf(OperatorType.ADD);

  public static final OperatorIR MULT = OperatorIR.valueOf(OperatorType.MULT);

  public static final OperatorIR SUB = OperatorIR.valueOf(OperatorType.SUB);

  public static final OperatorIR DIV = OperatorIR.valueOf(OperatorType.DIV);

  public static final OperatorIR MOD = OperatorIR.valueOf(OperatorType.MOD);

  public static final OperatorIR BIT_AND = OperatorIR.valueOf(OperatorType.BIT_AND);

  public static final OperatorIR BIT_NOT = OperatorIR.valueOf(OperatorType.BIT_NOT);

  public static final OperatorIR BIT_OR = OperatorIR.valueOf(OperatorType.BIT_OR);

  public static final OperatorIR BIT_XOR = OperatorIR.valueOf(OperatorType.BIT_XOR);

  public static final OperatorIR EXP = OperatorIR.valueOf(OperatorType.Exponent);

  public static final OperatorIR MATCH = OperatorIR.valueOf(OperatorType.MATCH);

  public static final OperatorIR AND = OperatorIR.valueOf(OperatorType.AND);

  public static final OperatorIR OR = OperatorIR.valueOf(OperatorType.OR);

  public static final OperatorIR NOT = OperatorIR.valueOf(OperatorType.NOT);

  public static final OperatorIR NEG = OperatorIR.valueOf(OperatorType.NEG);

  public static final OperatorIR LT = OperatorIR.valueOf(OperatorType.LT);

  public static final OperatorIR LE = OperatorIR.valueOf(OperatorType.LE);

  public static final OperatorIR GT = OperatorIR.valueOf(OperatorType.GT);

  public static final OperatorIR GE = OperatorIR.valueOf(OperatorType.GE);

  public static final OperatorIR EQ = OperatorIR.valueOf(OperatorType.EQ);

  public static final OperatorIR NE = OperatorIR.valueOf(OperatorType.NEQ);

  public static final OperatorIR SHIFT_LEFT = OperatorIR.valueOf(OperatorType.SHIFT_LEFT);

  public static final OperatorIR SHIFT_RIGHT = OperatorIR.valueOf(OperatorType.SHIFT_RIGHT);

  public static final OperatorIR INDEX = OperatorIR.valueOf(OperatorType.INDEX);

  public static final OperatorIR DEF = OperatorIR.valueOf(OperatorType.DEFINE);

  public static final OperatorIR ASSIGN = OperatorIR.valueOf(OperatorType.ASSIGNMENT);

  public static final OperatorIR UNSIGNED_SHIFT_RIGHT =
      OperatorIR.valueOf(OperatorType.U_SHIFT_RIGHT);

  static OperatorIR valueOf(final OperatorType op) {
    return new OperatorIR(op);
  }

  public OperatorIR(final OperatorType op, final AviatorFunction func) {
    this.fn = func;
    this.op = op;
  }

  private OperatorIR(final OperatorType op) {
    super();
    this.op = op;
  }

  @Override
  public void eval(final InterpretContext context) {
    assert (this.op != OperatorType.FUNC);
    int arity = this.op.getArity();

    AviatorObject[] args = new AviatorObject[arity];
    for (int i = args.length - 1; i >= 0; i--) {
      args[i] = context.pop();
    }

    AviatorObject result;
    if (this.fn == null) {
      result = this.op.eval(args, context.getEnv());
    } else {
      result = OperationRuntime.evalOpFunction(context.getEnv(), args, this.op, this.fn);
    }
    context.push(result);
    context.dispatch();
  }

  @Override
  public boolean mayBeCost() {
    return true;
  }

  public OperatorType getOp() {
    return this.op;
  }

  @Override
  public String toString() {
    return this.op.name().toLowerCase();
  }
}
