package com.googlecode.aviator.runtime.type.string;

import java.util.Map;

/**
 * A string segment.
 * 
 * @author dennis(killme2008@gmail.com)
 *
 */
public interface StringSegment {
  StringBuilder appendTo(StringBuilder sb, Map<String, Object> env);
}
