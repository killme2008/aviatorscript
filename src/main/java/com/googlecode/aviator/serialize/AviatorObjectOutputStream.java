package com.googlecode.aviator.serialize;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.ClassExpression;
import com.googlecode.aviator.runtime.type.Range;

/**
 * A special ObjectOutputStream that will write the generated script class byte array.
 * 
 * @author dennis
 * @since 5.3.4
 *
 */
public class AviatorObjectOutputStream extends ObjectOutputStream {

  private Map<String, byte[]> classBytesCache = new HashMap<String, byte[]>();

  public AviatorObjectOutputStream(OutputStream out) throws IOException {
    super(out);
    this.enableReplaceObject(true);
  }


  @Override
  protected Object replaceObject(Object obj) throws IOException {
    if (obj instanceof ClassExpression) {
      this.classBytesCache.put(obj.getClass().getName(), ((ClassExpression) obj).getClassBytes());
    }

    return super.replaceObject(obj);
  }

  @Override
  protected void annotateClass(Class<?> cl) throws IOException {
    if (ClassExpression.class.isAssignableFrom(cl) && cl != ClassExpression.class) {
      byte[] classBytes = this.classBytesCache.get(cl.getName());
      if (classBytes == null) {
        throw new IllegalArgumentException("Class bytes not found: " + cl.getName()
            + ", forgot to enable Options.SERIALIZABLE before compiling the script?");
      }
      this.writeInt(classBytes.length);
      this.write(classBytes);

    }
  }

}
