/***
 * ASM XML Adapter
 * Copyright (c) 2004, Eugene Kuleshov
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.googlecode.aviator.asm.xml;

import java.util.HashMap;
import java.util.Map;

import org.xml.sax.ContentHandler;
import org.xml.sax.helpers.AttributesImpl;

import com.googlecode.aviator.asm.AnnotationVisitor;
import com.googlecode.aviator.asm.Attribute;
import com.googlecode.aviator.asm.Label;
import com.googlecode.aviator.asm.MethodVisitor;
import com.googlecode.aviator.asm.Opcodes;
import com.googlecode.aviator.asm.Type;
import com.googlecode.aviator.asm.util.AbstractVisitor;


/**
 * A {@link MethodVisitor} that generates SAX 2.0 events from the visited
 * method.
 * 
 * @see com.googlecode.aviator.asm.xml.SAXClassAdapter
 * @see com.googlecode.aviator.asm.xml.Processor
 * 
 * @author Eugene Kuleshov
 */
public final class SAXCodeAdapter extends SAXAdapter implements MethodVisitor {

    static String[] TYPES = {
        "top",
        "int",
        "float",
        "double",
        "long",
        "null",
        "uninitializedThis" };

    private Map labelNames;

    /**
     * Constructs a new {@link SAXCodeAdapter SAXCodeAdapter} object.
     * 
     * @param h content handler that will be used to send SAX 2.0 events.
     * @param access
     */
    public SAXCodeAdapter(final ContentHandler h, final int access) {
        super(h);
        labelNames = new HashMap();

        if ((access & (Opcodes.ACC_ABSTRACT | Opcodes.ACC_INTERFACE | Opcodes.ACC_NATIVE)) == 0)
        {
            addStart("code", new AttributesImpl());
        }
    }

    public final void visitCode() {
    }

    public void visitFrame(
        final int type,
        final int nLocal,
        final Object[] local,
        final int nStack,
        final Object[] stack)
    {
        AttributesImpl attrs = new AttributesImpl();
        switch (type) {
            case Opcodes.F_NEW:
            case Opcodes.F_FULL:
                if (type == Opcodes.F_NEW) {
                    attrs.addAttribute("", "type", "type", "", "NEW");
                } else {
                    attrs.addAttribute("", "type", "type", "", "FULL");
                }
                addStart("frame", attrs);
                appendFrameTypes(true, nLocal, local);
                appendFrameTypes(false, nStack, stack);
                break;
            case Opcodes.F_APPEND:
                attrs.addAttribute("", "type", "type", "", "APPEND");
                addStart("frame", attrs);
                appendFrameTypes(true, nLocal, local);
                break;
            case Opcodes.F_CHOP:
                attrs.addAttribute("", "type", "type", "", "CHOP");
                attrs.addAttribute("",
                        "count",
                        "count",
                        "",
                        Integer.toString(nLocal));
                addStart("frame", attrs);
                break;
            case Opcodes.F_SAME:
                attrs.addAttribute("", "type", "type", "", "SAME");
                addStart("frame", attrs);
                break;
            case Opcodes.F_SAME1:
                attrs.addAttribute("", "type", "type", "", "SAME1");
                addStart("frame", attrs);
                appendFrameTypes(false, 1, stack);
                break;
        }
        addEnd("frame");
    }

    private void appendFrameTypes(
        final boolean local,
        final int n,
        final Object[] types)
    {
        for (int i = 0; i < n; ++i) {
            Object type = types[i];
            AttributesImpl attrs = new AttributesImpl();
            if (type instanceof String) {
                attrs.addAttribute("", "type", "type", "", (String) type);
            } else if (type instanceof Integer) {
                attrs.addAttribute("",
                        "type",
                        "type",
                        "",
                        TYPES[((Integer) type).intValue()]);
            } else {
                attrs.addAttribute("", "type", "type", "", "uninitialized");
                attrs.addAttribute("",
                        "label",
                        "label",
                        "",
                        getLabel((Label) type));
            }
            addElement(local ? "local" : "stack", attrs);
        }
    }

    public final void visitInsn(final int opcode) {
        addElement(AbstractVisitor.OPCODES[opcode], new AttributesImpl());
    }

    public final void visitIntInsn(final int opcode, final int operand) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "value", "value", "", Integer.toString(operand));
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitVarInsn(final int opcode, final int var) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "var", "var", "", Integer.toString(var));
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitTypeInsn(final int opcode, final String desc) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "desc", "desc", "", desc);
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitFieldInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "owner", "owner", "", owner);
        attrs.addAttribute("", "name", "name", "", name);
        attrs.addAttribute("", "desc", "desc", "", desc);
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitMethodInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "owner", "owner", "", owner);
        attrs.addAttribute("", "name", "name", "", name);
        attrs.addAttribute("", "desc", "desc", "", desc);
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitJumpInsn(final int opcode, final Label label) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "label", "label", "", getLabel(label));
        addElement(AbstractVisitor.OPCODES[opcode], attrs);
    }

    public final void visitLabel(final Label label) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "name", "name", "", getLabel(label));
        addElement("Label", attrs);
    }

    public final void visitLdcInsn(final Object cst) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("",
                "cst",
                "cst",
                "",
                SAXClassAdapter.encode(cst.toString()));
        attrs.addAttribute("",
                "desc",
                "desc",
                "",
                Type.getDescriptor(cst.getClass()));
        addElement(AbstractVisitor.OPCODES[Opcodes.LDC], attrs);
    }

    public final void visitIincInsn(final int var, final int increment) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "var", "var", "", Integer.toString(var));
        attrs.addAttribute("", "inc", "inc", "", Integer.toString(increment));
        addElement(AbstractVisitor.OPCODES[Opcodes.IINC], attrs);
    }

    public final void visitTableSwitchInsn(
        final int min,
        final int max,
        final Label dflt,
        final Label[] labels)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "min", "min", "", Integer.toString(min));
        attrs.addAttribute("", "max", "max", "", Integer.toString(max));
        attrs.addAttribute("", "dflt", "dflt", "", getLabel(dflt));
        String o = AbstractVisitor.OPCODES[Opcodes.TABLESWITCH];
        addStart(o, attrs);
        for (int i = 0; i < labels.length; i++) {
            AttributesImpl att2 = new AttributesImpl();
            att2.addAttribute("", "name", "name", "", getLabel(labels[i]));
            addElement("label", att2);
        }
        addEnd(o);
    }

    public final void visitLookupSwitchInsn(
        final Label dflt,
        final int[] keys,
        final Label[] labels)
    {
        AttributesImpl att = new AttributesImpl();
        att.addAttribute("", "dflt", "dflt", "", getLabel(dflt));
        String o = AbstractVisitor.OPCODES[Opcodes.LOOKUPSWITCH];
        addStart(o, att);
        for (int i = 0; i < labels.length; i++) {
            AttributesImpl att2 = new AttributesImpl();
            att2.addAttribute("", "name", "name", "", getLabel(labels[i]));
            att2.addAttribute("", "key", "key", "", Integer.toString(keys[i]));
            addElement("label", att2);
        }
        addEnd(o);
    }

    public final void visitMultiANewArrayInsn(final String desc, final int dims)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "desc", "desc", "", desc);
        attrs.addAttribute("", "dims", "dims", "", Integer.toString(dims));
        addElement(AbstractVisitor.OPCODES[Opcodes.MULTIANEWARRAY], attrs);
    }

    public final void visitTryCatchBlock(
        final Label start,
        final Label end,
        final Label handler,
        final String type)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "start", "start", "", getLabel(start));
        attrs.addAttribute("", "end", "end", "", getLabel(end));
        attrs.addAttribute("", "handler", "handler", "", getLabel(handler));
        if (type != null) {
            attrs.addAttribute("", "type", "type", "", type);
        }
        addElement("TryCatch", attrs);
    }

    public final void visitMaxs(final int maxStack, final int maxLocals) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("",
                "maxStack",
                "maxStack",
                "",
                Integer.toString(maxStack));
        attrs.addAttribute("",
                "maxLocals",
                "maxLocals",
                "",
                Integer.toString(maxLocals));
        addElement("Max", attrs);

        addEnd("code");
    }

    public void visitLocalVariable(
        final String name,
        final String desc,
        final String signature,
        final Label start,
        final Label end,
        final int index)
    {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "name", "name", "", name);
        attrs.addAttribute("", "desc", "desc", "", desc);
        if (signature != null) {
            attrs.addAttribute("",
                    "signature",
                    "signature",
                    "",
                    SAXClassAdapter.encode(signature));
        }
        attrs.addAttribute("", "start", "start", "", getLabel(start));
        attrs.addAttribute("", "end", "end", "", getLabel(end));
        attrs.addAttribute("", "var", "var", "", Integer.toString(index));
        addElement("LocalVar", attrs);
    }

    public final void visitLineNumber(final int line, final Label start) {
        AttributesImpl attrs = new AttributesImpl();
        attrs.addAttribute("", "line", "line", "", Integer.toString(line));
        attrs.addAttribute("", "start", "start", "", getLabel(start));
        addElement("LineNumber", attrs);
    }

    public AnnotationVisitor visitAnnotationDefault() {
        return new SAXAnnotationAdapter(getContentHandler(),
                "annotationDefault",
                0,
                null,
                null);
    }

    public AnnotationVisitor visitAnnotation(
        final String desc,
        final boolean visible)
    {
        return new SAXAnnotationAdapter(getContentHandler(),
                "annotation",
                visible ? 1 : -1,
                null,
                desc);
    }

    public AnnotationVisitor visitParameterAnnotation(
        final int parameter,
        final String desc,
        final boolean visible)
    {
        return new SAXAnnotationAdapter(getContentHandler(),
                "parameterAnnotation",
                visible ? 1 : -1,
                parameter,
                desc);
    }

    public void visitEnd() {
        addEnd("method");
    }

    public final void visitAttribute(final Attribute attr) {
        // TODO Auto-generated SAXCodeAdapter.visitAttribute
    }

    private final String getLabel(final Label label) {
        String name = (String) labelNames.get(label);
        if (name == null) {
            name = Integer.toString(labelNames.size());
            labelNames.put(label, name);
        }
        return name;
    }

}
