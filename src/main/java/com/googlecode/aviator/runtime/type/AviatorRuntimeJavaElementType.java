package com.googlecode.aviator.runtime.type;

import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.utils.ArrayUtils;
import com.googlecode.aviator.utils.Reflector;

public class AviatorRuntimeJavaElementType extends AviatorRuntimeJavaType {


  private static final long serialVersionUID = -955529214730255727L;
  private final Object index;
  private final Object container;

  private final ContainerType containerType;

  public static enum ContainerType {
    List, Array, Map
  }

  public AviatorRuntimeJavaElementType(final ContainerType containerType, final Object container,
      final Object index, final Callable<Object> callable) {
    super(null);
    setCallable(callable);
    this.container = container;
    this.index = index;
    this.containerType = containerType;
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  @Override
  public AviatorObject setValue(final AviatorObject value, final Map<String, Object> env) {
    Object val = value.getValue(env);
    switch (this.containerType) {
      case Array:
        ArrayUtils.set(this.container, (int) this.index,
            Reflector.boxArg(this.container.getClass().getComponentType(), val));
        break;
      case List:
        ((List) this.container).set((int) this.index, val);
        break;
      case Map:
        ((Map) this.container).put(this.index, val);
        break;
      default:
        throw new ExpressionRuntimeException("Unknown container type: " + this.containerType);
    }
    return AviatorRuntimeJavaType.valueOf(val);
  }
}
