/**
 *  Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.code.asm;

import static com.googlecode.aviator.asm.Opcodes.*;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicLong;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.ClassExpression;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.asm.ClassVisitor;
import com.googlecode.aviator.asm.ClassWriter;
import com.googlecode.aviator.asm.Label;
import com.googlecode.aviator.asm.MethodVisitor;
import com.googlecode.aviator.asm.util.CheckClassAdapter;
import com.googlecode.aviator.asm.util.TraceClassVisitor;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.parser.AviatorClassLoader;

/**
 * Code generator using asm
 * 
 * @author dennis
 * 
 */
public class ASMCodeGenerator implements CodeGenerator {
	// Class Writer to generate class
	private final ClassWriter classWriter;
	// Trace visitor
	private ClassVisitor traceClassVisitor;
	// Check visitor
	private ClassVisitor checkClassAdapter;
	// Method visitor
	private MethodVisitor mv;
	// Class name
	private final String className;
	// Class loader to define generated class
	private final AviatorClassLoader classLoader;

	private static final AtomicLong CLASS_COUNTER = new AtomicLong();

	/**
	 * Operands count to check stack frames
	 */
	private int operandsCount = 0;

	private int maxStacks = 0;
	private int maxLocals = 1;

	private void setMaxStacks(int newMaxStacks) {
		if (newMaxStacks > this.maxStacks) {
			this.maxStacks = newMaxStacks;
		}
	}

	public ASMCodeGenerator(AviatorClassLoader classLoader, OutputStream traceOut,
			boolean trace) {
		this.classLoader = classLoader;
		// Generate inner class name
		this.className = "Script_" + System.currentTimeMillis() + "_"
				+ CLASS_COUNTER.getAndIncrement();
		// Auto compute frames
		this.classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
		if (trace) {
			this.traceClassVisitor = new TraceClassVisitor(this.classWriter,
					new PrintWriter(traceOut));
			this.checkClassAdapter = new CheckClassAdapter(
					this.traceClassVisitor);
		} else {
			this.checkClassAdapter = new CheckClassAdapter(this.classWriter);
		}
		this.makeConstructor();
		this.startVisitMethodCode();
	}

	private void startVisitMethodCode() {
		this.mv = this.checkClassAdapter
				.visitMethod(
						ACC_PUBLIC + ACC_STATIC + ACC_FINAL,
						"run",
						"(Ljava/util/Map;)Ljava/lang/Object;",
						"(Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;)Ljava/lang/Object;",
						null);
		this.mv.visitCode();
	}

	private void endVisitCode() {

		if (this.operandsCount > 0) {
			this.loadEnv();
			this.mv.visitMethodInsn(INVOKEVIRTUAL,
					"com/googlecode/aviator/runtime/type/AviatorObject",
					"getValue", "(Ljava/util/Map;)Ljava/lang/Object;");
			this.mv.visitInsn(ARETURN);
			this.popOperand();
			this.popOperand();
		} else {
			this.mv.visitInsn(ACONST_NULL);
			this.mv.visitInsn(ARETURN);
			this.pushOperand(0);
			this.popOperand();
		}
		if (this.operandsCount > 0) {
			throw new CompileExpressionErrorException(
					"operand stack is not empty,count=" + this.operandsCount);
		}
		this.mv.visitMaxs(this.maxStacks, this.maxLocals);
		this.mv.visitEnd();

		this.checkClassAdapter.visitEnd();
	}

	/**
	 * Make a default constructor
	 */
	private void makeConstructor() {
		this.checkClassAdapter.visit(AviatorEvaluator.BYTECODE_VER, ACC_PUBLIC
				+ ACC_SUPER, this.className, null, "java/lang/Object", null);

		{
			this.mv = this.checkClassAdapter.visitMethod(ACC_PUBLIC, "<init>",
					"()V", null, null);
			this.mv.visitCode();
			this.mv.visitVarInsn(ALOAD, 0);
			this.mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object",
					"<init>", "()V");
			this.mv.visitInsn(RETURN);
			this.mv.visitMaxs(1, 1);
			this.mv.visitEnd();
		}
	}

	/**
	 * Make a label
	 * 
	 * @return
	 */
	private Label makeLabel() {
		return new Label();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.googlecode.aviator.code.CodeGenerator#onAdd(com.googlecode.aviator
	 * .lexer.token.Token)
	 */
	public void onAdd(Token<?> lookhead) {
		this.doArthOperation("add");
	}

	/**
	 * Do arithmetic operation
	 * 
	 * @param methodName
	 */
	private void doArthOperation(String methodName) {
		this.loadEnv();
		this.mv.visitMethodInsn(
				INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject",
				methodName,
				"(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
		this.popOperand();
		this.popOperand();
	}

	/**
	 * Pop a operand from stack
	 */
	private void popOperand() {
		this.operandsCount--;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.googlecode.aviator.code.CodeGenerator#onSub(com.googlecode.aviator
	 * .lexer.token.Token)
	 */
	public void onSub(Token<?> lookhead) {
		this.doArthOperation("sub");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.googlecode.aviator.code.CodeGenerator#onMult(com.googlecode.aviator
	 * .lexer.token.Token)
	 */
	public void onMult(Token<?> lookhead) {
		this.doArthOperation("mult");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.googlecode.aviator.code.CodeGenerator#onDiv(com.googlecode.aviator
	 * .lexer.token.Token)
	 */
	public void onDiv(Token<?> lookhead) {
		this.doArthOperation("div");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.googlecode.aviator.code.CodeGenerator#onMod(com.googlecode.aviator
	 * .lexer.token.Token)
	 */
	public void onMod(Token<?> lookhead) {
		this.doArthOperation("mod");
	}

	/**
	 * Do logic operation "&&" left operand
	 */
	public void onAndLeft(Token<?> lookhead) {
		this.loadEnv();
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject",
				"booleanValue", "(Ljava/util/Map;)Z");
		Label l0 = new Label();
		this.l0stack.push(l0);
		this.mv.visitJumpInsn(IFEQ, l0);

		this.popOperand(); // boolean object
		this.popOperand(); // environment

	}

	/**
	 * Do logic operation "&&" right operand
	 */
	public void onAndRight(Token<?> lookhead) {
		this.loadEnv();
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject",
				"booleanValue", "(Ljava/util/Map;)Z");
		this.mv.visitJumpInsn(IFEQ, this.l0stack.peek());
		// Result is true
		this.mv.visitFieldInsn(GETSTATIC,
				"com/googlecode/aviator/runtime/type/AviatorBoolean", "TRUE",
				"Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
		Label l1 = new Label();
		this.mv.visitJumpInsn(GOTO, l1);
		this.mv.visitLabel(this.l0stack.pop());
		// Result is false
		this.mv.visitFieldInsn(GETSTATIC,
				"com/googlecode/aviator/runtime/type/AviatorBoolean", "FALSE",
				"Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
		this.mv.visitLabel(l1);

		this.popOperand(); // boolean object
		this.popOperand(); // environment
		this.pushOperand(0);
	}

	/**
	 * Label stack for ternary operator
	 */
	private final Stack<Label> l0stack = new Stack<Label>();
	private final Stack<Label> l1stack = new Stack<Label>();

	public void onTernaryBoolean(Token<?> lookhead) {
		this.loadEnv();
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject",
				"booleanValue", "(Ljava/util/Map;)Z");
		Label l0 = new Label();
		Label l1 = new Label();
		this.l0stack.push(l0);
		this.l1stack.push(l1);
		this.mv.visitJumpInsn(IFEQ, l0);
		this.popOperand();
		this.popOperand();
		this.pushOperand(1); // add two booleans

		this.popOperand(); // pop the last result
	}

	public void onTernaryLeft(Token<?> lookhead) {
		this.mv.visitJumpInsn(GOTO, this.l1stack.peek());
		this.mv.visitLabel(this.l0stack.pop());
		this.popOperand(); // pop one boolean
	}

	public void onTernaryRight(Token<?> lookhead) {
		this.mv.visitLabel(this.l1stack.pop());
		this.popOperand(); // pop one boolean
	}

	/**
	 * Do logic operation "||" right operand
	 */
	public void onJoinRight(Token<?> lookhead) {
		this.loadEnv();
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject",
				"booleanValue", "(Ljava/util/Map;)Z");
		Label l1 = new Label();
		this.mv.visitJumpInsn(IFNE, this.l0stack.peek());
		// Result is False
		this.mv.visitFieldInsn(GETSTATIC,
				"com/googlecode/aviator/runtime/type/AviatorBoolean", "FALSE",
				"Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
		this.mv.visitJumpInsn(GOTO, l1);
		this.mv.visitLabel(this.l0stack.pop());
		// Result is True
		this.mv.visitFieldInsn(GETSTATIC,
				"com/googlecode/aviator/runtime/type/AviatorBoolean", "TRUE",
				"Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
		this.mv.visitLabel(l1);
		this.popOperand();
		this.popOperand();
		this.pushOperand(0);

	}

	/**
	 * Do logic operation "||" left operand
	 */
	public void onJoinLeft(Token<?> lookhead) {
		this.loadEnv();
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject",
				"booleanValue", "(Ljava/util/Map;)Z");
		Label l0 = new Label();
		this.l0stack.push(l0);
		this.mv.visitJumpInsn(IFNE, l0);

		this.popOperand();
		this.popOperand();

	}

	public void onEq(Token<?> lookhead) {
		this.doCompareAndJump(IFNE);
	}

	public void onMatch(Token<?> lookhead) {
		this.mv.visitInsn(SWAP);
		this.loadEnv();
		this.mv.visitMethodInsn(
				INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject",
				"match",
				"(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");

		this.popOperand();
		this.popOperand();
		this.popOperand();
		this.pushOperand(0);
	}

	public void onNeq(Token<?> lookhead) {
		this.doCompareAndJump(IFEQ);
	}

	private void doCompareAndJump(int ints) {
		this.loadEnv();
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject", "compare",
				"(Lcom/googlecode/aviator/runtime/type/AviatorObject;Ljava/util/Map;)I");
		Label l0 = this.makeLabel();
		Label l1 = this.makeLabel();
		this.mv.visitJumpInsn(ints, l0);
		this.mv.visitFieldInsn(GETSTATIC,
				"com/googlecode/aviator/runtime/type/AviatorBoolean", "TRUE",
				"Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
		this.mv.visitJumpInsn(GOTO, l1);
		this.mv.visitLabel(l0);
		this.mv.visitFieldInsn(GETSTATIC,
				"com/googlecode/aviator/runtime/type/AviatorBoolean", "FALSE",
				"Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
		this.mv.visitLabel(l1);
		this.popOperand();
		this.popOperand();
		this.popOperand();
		this.pushOperand(0);
	}

	public void onGe(Token<?> lookhead) {
		this.doCompareAndJump(IFLT);
	}

	public void onGt(Token<?> lookhead) {
		this.doCompareAndJump(IFLE);
	}

	public void onLe(Token<?> lookhead) {
		this.doCompareAndJump(IFGT);

	}

	public void onLt(Token<?> lookhead) {
		this.doCompareAndJump(IFGE);
	}

	/**
	 * 
	 * @param extras
	 *            额外的栈空间大小
	 */
	public void pushOperand(int extras) {
		this.operandsCount++;
		this.operandsCount += extras;
		this.setMaxStacks(this.operandsCount);
	}

	/**
	 * Logic operation '!'
	 */
	public void onNot(Token<?> lookhead) {
		this.pushOperand(0);

		this.mv.visitTypeInsn(CHECKCAST,
				"com/googlecode/aviator/runtime/type/AviatorObject");
		this.mv.visitVarInsn(ALOAD, 0);
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject", "not",
				"(Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");

		this.popOperand();
		this.popOperand();
		this.pushOperand(0);
	}

	/**
	 * Bit operation '~'
	 */
	public void onBitNot(Token<?> lookhead) {
		this.pushOperand(0);
		this.mv.visitTypeInsn(CHECKCAST,
				"com/googlecode/aviator/runtime/type/AviatorObject");
		this.mv.visitVarInsn(ALOAD, 0);
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject", "bitNot",
				"(Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
		this.popOperand();
		this.popOperand();
		this.pushOperand(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.googlecode.aviator.code.CodeGenerator#onNeg(com.googlecode.aviator
	 * .lexer.token.Token, int)
	 */
	public void onNeg(Token<?> lookhead) {
		this.pushOperand(0);

		this.mv.visitTypeInsn(CHECKCAST,
				"com/googlecode/aviator/runtime/type/AviatorObject");
		this.mv.visitVarInsn(ALOAD, 0);
		this.mv.visitMethodInsn(INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorObject", "neg",
				"(Ljava/util/Map;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");
		this.popOperand();
		this.popOperand();
		this.pushOperand(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.googlecode.aviator.code.CodeGenerator#getResult()
	 */
	public Expression getResult() {
		this.endVisitCode();
		byte[] bytes = this.classWriter.toByteArray();
		try {
			return new ClassExpression(this.classLoader.defineClass(
					this.className, bytes));
		} catch (Exception e) {
			throw new CompileExpressionErrorException("define class error", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.googlecode.aviator.code.CodeGenerator#onConstant(com.googlecode.aviator
	 * .lexer.token.Token)
	 */
	public void onConstant(Token<?> lookhead) {
		if (lookhead == null) {
			return;
		}
		// load token to stack
		switch (lookhead.getType()) {
		case Number:
			// load numbers
			NumberToken numberToken = (NumberToken) lookhead;
			if (numberToken.getNumber() instanceof Double) {
				this.mv.visitTypeInsn(NEW,
						"com/googlecode/aviator/runtime/type/AviatorDouble");
				this.mv.visitInsn(DUP);
				this.mv.visitLdcInsn(numberToken.getNumber());
				this.mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double",
						"valueOf", "(D)Ljava/lang/Double;");
				this.mv.visitMethodInsn(INVOKESPECIAL,
						"com/googlecode/aviator/runtime/type/AviatorDouble",
						"<init>", "(Ljava/lang/Number;)V");
			} else {
				this.mv.visitTypeInsn(NEW,
						"com/googlecode/aviator/runtime/type/AviatorLong");
				this.mv.visitInsn(DUP);
				this.mv.visitLdcInsn(numberToken.getNumber());
				this.mv.visitMethodInsn(INVOKESTATIC, "java/lang/Long",
						"valueOf", "(J)Ljava/lang/Long;");
				this.mv.visitMethodInsn(INVOKESPECIAL,
						"com/googlecode/aviator/runtime/type/AviatorLong",
						"<init>", "(Ljava/lang/Number;)V");
			}
			this.pushOperand(2);
			this.popOperand();
			this.popOperand();
			break;
		case String:
			// load string
			this.mv.visitTypeInsn(NEW,
					"com/googlecode/aviator/runtime/type/AviatorString");
			this.mv.visitInsn(DUP);
			this.mv.visitLdcInsn(lookhead.getValue(null));
			this.mv.visitMethodInsn(INVOKESPECIAL,
					"com/googlecode/aviator/runtime/type/AviatorString",
					"<init>", "(Ljava/lang/String;)V");
			this.pushOperand(2);
			this.popOperand();
			this.popOperand();
			break;
		case Pattern:
			// load pattern
			this.mv.visitTypeInsn(NEW,
					"com/googlecode/aviator/runtime/type/AviatorPattern");
			this.mv.visitInsn(DUP);
			this.mv.visitLdcInsn(lookhead.getValue(null));
			this.mv.visitMethodInsn(INVOKESPECIAL,
					"com/googlecode/aviator/runtime/type/AviatorPattern",
					"<init>", "(Ljava/lang/String;)V");
			this.pushOperand(2);
			this.popOperand();
			this.popOperand();
			break;
		case Variable:
			// load variable
			Variable variable = (Variable) lookhead;
			if (variable.equals(Variable.TRUE)) {
				this.mv.visitFieldInsn(GETSTATIC,
						"com/googlecode/aviator/runtime/type/AviatorBoolean",
						"TRUE",
						"Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
				this.pushOperand(0);
			} else if (variable.equals(Variable.FALSE)) {
				this.mv.visitFieldInsn(GETSTATIC,
						"com/googlecode/aviator/runtime/type/AviatorBoolean",
						"FALSE",
						"Lcom/googlecode/aviator/runtime/type/AviatorBoolean;");
				this.pushOperand(0);
			} else if (variable.equals(Variable.NIL)) {
				this.mv.visitFieldInsn(GETSTATIC,
						"com/googlecode/aviator/runtime/type/AviatorNil",
						"NIL",
						"Lcom/googlecode/aviator/runtime/type/AviatorNil;");
				this.pushOperand(0);
			} else {
				// check if it is a function name
				// if
				// (AviatorEvaluator.FUNC_MAP.keySet().contains(variable.getLexeme()))
				// {
				// throw new CompileExpressionErrorException("index=" +
				// variable.getStartIndex() + ","
				// + variable.getLexeme() +
				// " is a function name,please don't use it as variable");
				// }

				this.mv.visitTypeInsn(NEW,
						"com/googlecode/aviator/runtime/type/AviatorJavaType");
				this.mv.visitInsn(DUP);
				this.mv.visitLdcInsn(variable.getLexeme());
				this.mv.visitMethodInsn(INVOKESPECIAL,
						"com/googlecode/aviator/runtime/type/AviatorJavaType",
						"<init>", "(Ljava/lang/String;)V");
				this.pushOperand(2);
				this.popOperand();
				this.popOperand();

			}
			break;
		}

	}

	private String getInvokeMethodDesc(int paramCount) {
		StringBuilder sb = new StringBuilder("(Ljava/util/Map;");
		for (int i = 0; i < paramCount; i++) {
			sb.append("Lcom/googlecode/aviator/runtime/type/AviatorObject;");
		}
		sb.append(")Lcom/googlecode/aviator/runtime/type/AviatorObject;");
		return sb.toString();
	}

	public void onMethodInvoke(Token<?> lookhead) {
		final MethodMetaData methodMetaData = this.methodMetaDataStack.pop();
		final int parameterCount = methodMetaData.parameterCount;
		this.mv.visitMethodInsn(INVOKEINTERFACE,
				"com/googlecode/aviator/runtime/type/AviatorFunction", "call",
				this.getInvokeMethodDesc(parameterCount));

		this.popOperand(); // method object
		this.popOperand(); // env map
		// pop operands
		for (int i = 0; i < parameterCount; i++) {
			this.popOperand();
		}
		// push result
		this.pushOperand(0);
	}

	public void onMethodParameter(Token<?> lookhead) {
		this.methodMetaDataStack.peek().parameterCount++;
		// // add parameter to list
		// this.mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add",
		// "(Ljava/lang/Object;)Z");
		// // pop boolean
		// this.mv.visitInsn(POP);
		// this.mv.visitVarInsn(ALOAD,
		// this.methodMetaDataStack.peek().parameterListIndex);
	}

	private static class MethodMetaData {
		final String methodName;

		int parameterCount;

		public MethodMetaData(String methodName) {
			super();
			this.methodName = methodName;
			this.parameterCount = 0;
		}

	}

	private final Stack<MethodMetaData> methodMetaDataStack = new Stack<MethodMetaData>();

	public void onElementStart(Token<?> lookhead) {
		this.onConstant(lookhead);
		this.loadEnv();
	}

	public void onElementEnd(Token<?> lookhead) {
		this.mv.visitMethodInsn(
				INVOKEVIRTUAL,
				"com/googlecode/aviator/runtime/type/AviatorJavaType",
				"getElement",
				"(Ljava/util/Map;Lcom/googlecode/aviator/runtime/type/AviatorObject;)Lcom/googlecode/aviator/runtime/type/AviatorObject;");

		this.popOperand();
		this.popOperand();
		this.popOperand();
		this.pushOperand(0);
	}

	public int getLocalIndex() {
		return this.maxLocals++;
	}

	public void onMethodName(Token<?> lookhead) {
		String methodName = lookhead.getLexeme();
		this.createAviatorFunctionObject(methodName);
		this.loadEnv();
		// final int parameterLocalIndex = this.createArugmentList();
		this.methodMetaDataStack.push(new MethodMetaData(methodName));

		// pushOperand(0);

	}

	// private int createArugmentList() {
	// // create argument list
	// this.pushOperand(0);
	// this.pushOperand(0);
	// this.mv.visitTypeInsn(NEW, "java/util/ArrayList");
	// this.mv.visitInsn(DUP);
	// this.popOperand();
	// this.mv.visitMethodInsn(INVOKESPECIAL, "java/util/ArrayList", "<init>",
	// "()V");
	// // store to local variable
	// final int parameterLocalIndex = this.getLocalIndex();
	// this.mv.visitVarInsn(ASTORE, parameterLocalIndex);
	// this.mv.visitVarInsn(ALOAD, parameterLocalIndex);
	// return parameterLocalIndex;
	// }

	private void loadEnv() {
		// load env
		this.pushOperand(0);
		this.mv.visitVarInsn(ALOAD, 0);
	}

	private void createAviatorFunctionObject(String methodName) {
		this.pushOperand(0);
		this.mv.visitLdcInsn(methodName);
		this.mv.visitMethodInsn(INVOKESTATIC,
				"com/googlecode/aviator/AviatorEvaluator", "getFunction",
				"(Ljava/lang/String;)Lcom/googlecode/aviator/runtime/type/AviatorFunction;");
		this.popOperand();
		this.pushOperand(0);
	}

	public void onBitAnd(Token<?> lookhead) {
		this.doArthOperation("bitAnd");
	}

	public void onBitOr(Token<?> lookhead) {
		this.doArthOperation("bitOr");
	}

	public void onBitXor(Token<?> lookhead) {
		this.doArthOperation("bitXor");
	}

	public void onShiftLeft(Token<?> lookhead) {
		this.doArthOperation("shiftLeft");

	}

	public void onShiftRight(Token<?> lookhead) {
		this.doArthOperation("shiftRight");

	}

	public void onUnsignedShiftRight(Token<?> lookhead) {
		this.doArthOperation("unsignedShiftRight");

	}

}
