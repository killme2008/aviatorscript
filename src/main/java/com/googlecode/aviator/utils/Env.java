/*
 * Copyright (c) 2009, Dennis M. Sosnoski. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted
 * provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this list of conditions
 * and the following disclaimer. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution. Neither the name of JiBX nor the names of its
 * contributors may be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 * WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.googlecode.aviator.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.Range;

/**
 * Expression execute environment.Modifed from ChainedMap in jibx.
 *
 * @author dennis
 *
 * @param <String>
 * @param <Object>
 */
public class Env implements Map<String, Object> {
  /** Default values map. */
  private final Map<String, Object> mDefaults;

  /**
   * Current evaluator instance that executes current expression.
   */
  private AviatorEvaluatorInstance instance;


  /** Override values map. */
  private Map<String, Object> mOverrides;

  private Expression expression;

  private List<String> importedSymbols;

  private List<String> importedPackages;

  // Caching resolved classes
  private Map<String/* class name */, Class<?>> resolvedClasses;


  public static final Map<String, Object> EMPTY_ENV = Collections.emptyMap();

  /**
   * Constructs an env instance with empty state.
   */
  public Env() {
    this(EMPTY_ENV);
  }

  /**
   * Constructor.
   *
   * @param defaults map providing defaults for keys not set directly
   */
  public Env(final Map<String, Object> defaults) {
    this.mDefaults = defaults;
  }

  public Env(final Map<String, Object> defaults, final Map<String, Object> overrides) {
    this.mDefaults = defaults;
    this.mOverrides = overrides;
  }

  public void setmOverrides(final Map<String, Object> mOverrides) {
    this.mOverrides = mOverrides;
  }

  public List<String> getImportedSymbols() {
    return this.importedSymbols;
  }

  public Expression getExpression() {
    return this.expression;
  }

  public void setExpression(final Expression expression) {
    this.expression = expression;
  }

  public Map<String, Object> getDefaults() {
    return this.mDefaults;
  }

  public String addSymbol(final String sym) {
    if (this.importedSymbols == null) {
      this.importedSymbols = new ArrayList<>();
    }
    this.importedSymbols.add(sym);
    invalidateCache();
    return sym;
  }

  public String addPackageSymbol(final String sym) {
    if (this.importedPackages == null) {
      this.importedPackages = new ArrayList<>();
    }
    this.importedPackages.add(sym);
    invalidateCache();
    return sym;
  }

  private void invalidateCache() {
    if (this.resolvedClasses != null) {
      this.resolvedClasses.clear();
    }
  }

  public AviatorEvaluatorInstance getInstance() {
    return this.instance;
  }

  public void setInstance(final AviatorEvaluatorInstance instance) {
    this.instance = instance;
  }

  public void configure(final AviatorEvaluatorInstance instance, final Expression exp) {
    this.instance = instance;
    this.expression = exp;
  }

  private String findSymbol(final String name) throws ClassNotFoundException {
    final String postfix = "." + name;
    String targetSym = null;
    if (this.importedSymbols != null) {
      for (String sym : this.importedSymbols) {
        if (sym.endsWith(postfix)) {
          targetSym = sym;
          break;
        }
      }
    }
    return targetSym;
  }

  public Class<?> resolveClassSymbol(final String name) throws ClassNotFoundException {
    return this.resolveClassSymbol(name, true);
  }

  public Class<?> resolveClassSymbol(final String name, final boolean checkIfAllow)
      throws ClassNotFoundException {
    Class<?> clazz = null;
    if (name.contains(".")) {
      clazz = classForName(name);
      if (clazz != null) {
        return checkIfClassIsAllowed(checkIfAllow, clazz);
      }
    } else {
      // java.lang.XXX
      clazz = classForName("java.lang." + name);
      if (clazz != null) {
        return checkIfClassIsAllowed(checkIfAllow, clazz);
      }
      // from cache
      clazz = retrieveFromCache(name);
      if (clazz != null) {
        return checkIfClassIsAllowed(checkIfAllow, clazz);
      }
      // from imported packages
      clazz = resolveFromImportedPackages(name);
      if (clazz != null) {
        return checkIfClassIsAllowed(checkIfAllow, clazz);
      }
      // from imported classes
      clazz = resolveFromImportedSymbols(name, clazz);
      if (clazz != null) {
        return checkIfClassIsAllowed(checkIfAllow, clazz);
      }

      // try to find from parent env.
      if (clazz == null && this.mDefaults instanceof Env) {
        clazz = ((Env) this.mDefaults).resolveClassSymbol(name, checkIfAllow);
      }
    }

    if (clazz == null) {
      throw new ClassNotFoundException(name);
    }

    return clazz;
  }

  private Class<?> checkIfClassIsAllowed(final boolean checkIfAllow, final Class<?> clazz) {
    if (checkIfAllow) {
      Set<Class<?>> allowedList = this.instance.getOptionValue(Options.ALLOWED_CLASS_SET).classes;
      if (allowedList != null) {
        // Null list means allowing all classes
        if (!allowedList.contains(clazz)) {
          throw new ExpressionRuntimeException(
              "`" + clazz + "` is not in allowed class set, check Options.ALLOWED_CLASS_SET");
        }
      }
      Set<Class<?>> assignableList =
          this.instance.getOptionValue(Options.ASSIGNABLE_ALLOWED_CLASS_SET).classes;
      if (assignableList != null) {
        for (Class<?> aClass : assignableList) {
          if (aClass.isAssignableFrom(clazz)) {
            return clazz;
          }
        }
        throw new ExpressionRuntimeException(
            "`" + clazz + "` is not in allowed class set, check Options.ALLOWED_CLASS_SET");
      }
    }
    return clazz;
  }

  private Class<?> resolveFromImportedPackages(final String name) {
    Class<?> clazz = null;
    if (this.importedPackages != null) {
      for (String pkg : this.importedPackages) {
        clazz = classForName(pkg + name);
        if (clazz != null) {
          put2cache(name, clazz);
          return clazz;
        }
      }
    }
    return clazz;
  }

  private Class<?> resolveFromImportedSymbols(final String name, Class<?> clazz)
      throws ClassNotFoundException {
    final String classSym = findSymbol(name);
    if (classSym != null) {
      clazz = classForName(classSym);
      if (clazz != null) {
        put2cache(name, clazz);
      }
    }
    return clazz;
  }

  private void put2cache(final String name, final Class<?> ret) {
    if (this.resolvedClasses == null) {
      this.resolvedClasses = new HashMap<>();
    }
    this.resolvedClasses.put(name, ret);
  }

  private Class<?> retrieveFromCache(final String name) {
    return this.resolvedClasses != null ? this.resolvedClasses.get(name) : null;
  }

  private Class<?> classForName(final String name) {
    try {
      return Class.forName(name);
    } catch (ClassNotFoundException e) {
      return null;
    }
  }

  /**
   * Clear all override key-value pairs. This only effects the overrides, not the defaults.
   */
  @Override
  public void clear() {
    if (this.mDefaults != EMPTY_ENV) {
      this.mDefaults.clear();
    }
    if (this.mOverrides != null && this.mOverrides != EMPTY_ENV) {
      this.mOverrides.clear();
    }
  }

  /**
   * Check if a key has a defined value. This will return <code>true</code> if the key is present in
   * the overrides map or the defaults map.
   *
   * @param key
   * @return <code>true</code> if key defined, <code>false</code> if not
   */
  @Override
  public boolean containsKey(final Object key) {
    Map<String, Object> overrides = getmOverrides(true);
    return overrides.containsKey(key)
        || (this.mDefaults != overrides ? this.mDefaults.containsKey(key) : false);
  }

  /**
   * Check if a value is present.
   *
   * @param value
   * @return <code>true</code> if value present as an override, <code>false</code> if not
   */
  @Override
  public boolean containsValue(final Object value) {
    return getmOverrides(true).containsValue(value) || this.mDefaults.containsValue(value);
  }

  /**
   * Get the set of entries.
   *
   * @return override entries
   */
  @Override
  public Set<Entry<String, Object>> entrySet() {
    Set<Entry<String, Object>> ret = new HashSet<Entry<String, Object>>(this.mDefaults.entrySet());
    ret.addAll(getmOverrides(true).entrySet());
    return ret;
  }

  /**
   * Get value for key. If the key is present in the overrides map, the value from that map is
   * returned; otherwise, the value for the key in the defaults map is returned.
   *
   * @param key
   * @return value (<code>null</code> if key not present)
   */
  @Override
  public Object get(final Object key) {
    // Should check ENV_VAR at first
    // TODO: performance tweak
    if (Constants.REDUCER_LOOP_VAR == key) {
      return Range.LOOP;
    }
    if (Constants.REDUCER_EMPTY_VAR == key) {
      return Constants.REDUCER_EMPTY;
    }

    if (Constants.ENV_VAR == key) {
      this.instance.ensureFeatureEnabled(Feature.InternalVars);
      return this;
    }
    if (Constants.FUNC_ARGS_VAR == key) {
      this.instance.ensureFeatureEnabled(Feature.InternalVars);
      return FunctionUtils.getFunctionArguments(this);
    }
    if (Constants.INSTANCE_VAR == key) {
      this.instance.ensureFeatureEnabled(Feature.InternalVars);
      return this.instance;
    }
    if (Constants.EXP_VAR == key) {
      this.instance.ensureFeatureEnabled(Feature.InternalVars);
      return this.expression;
    }

    Map<String, Object> overrides = getmOverrides(true);
    Object ret = null;
    if (overrides.containsKey(key)) {
      ret = overrides.get(key);
    } else {
      ret = this.mDefaults.get(key);
    }
    return ret;
  }

  /**
   * Check if no overrides are defined.
   *
   * @return <code>true</code> if no overrides, <code>false</code> if any present
   */
  @Override
  public boolean isEmpty() {
    return getmOverrides(true).isEmpty() && this.mDefaults.isEmpty();
  }

  /**
   * Get the set of keys. This only returns the keys in the overrides map.
   *
   * @return keys
   */
  @Override
  public Set<String> keySet() {
    Set<String> ret = new HashSet<String>(this.mDefaults.keySet());
    ret.addAll(getmOverrides(true).keySet());
    return ret;
  }

  /**
   * Set an override value.
   *
   * @param key
   * @param value
   * @return
   */
  public Object override(final String key, final Object value) {
    Object prior;
    Map<String, Object> overrides = getmOverrides(false);
    if (overrides.containsKey(key)) {
      prior = overrides.put(key, value);
    } else {
      overrides.put(key, value);
      prior = this.mDefaults != null ? this.mDefaults.get(key) : null;
    }
    return prior;
  }

  /**
   * Assign an value, if it's already in overrides, it will update it, otherwise set it to default
   * map.
   *
   * @param key
   * @param value
   * @return previous value for key (from default map, if not present in overrides)
   */
  @Override
  public Object put(final String key, final Object value) {
    Object prior = null;
    Map<String, Object> overrides = getmOverrides(false);
    if (overrides.containsKey(key)) {
      prior = overrides.put(key, value);
    } else {
      if (this.mDefaults.containsKey(key)) {
        prior = this.mDefaults.put(key, value);
      } else {
        overrides.put(key, value);
      }
    }
    return prior;
  }

  /**
   * Add all key-value pairs from another map into the overrides map.
   *
   * @param map
   */
  @Override
  public void putAll(final Map map) {
    getmOverrides(false).putAll(map);
  }

  /**
   * Remove a key-value pair. If the key was previously present in the overrides map it is simply
   * removed from that map. If it was not present in the overrides map but is present in the
   * defaults map, a null entry is added to the overrides map for that key.
   *
   * @param key
   * @return previous value for key
   */
  @Override
  public Object remove(final Object key) {
    if (getmOverrides(true).containsKey(key)) {
      return getmOverrides(false).remove(key);
    } else {
      return this.mDefaults.remove(key);
    }
  }

  /**
   * Remove a key-value pair from overrides.
   *
   * @param key
   * @return
   */
  public Object forgot(final Object key) {
    final Map<String, Object> overrides = getmOverrides(true);
    if (overrides != this.mDefaults && overrides.containsKey(key)) {
      return getmOverrides(false).remove(key);
    } else {
      return null;
    }
  }

  /**
   * Get the number of entries in the map. This returns the entry count for the overrides map only.
   *
   * @return entry count
   */
  @Override
  public int size() {
    return keySet().size();
  }

  /**
   * Get the values. This returns only the values in the overrides map.
   *
   * @return values
   */
  @Override
  public Collection<Object> values() {
    Collection<Object> vals = new ArrayList<Object>();
    for (String key : keySet()) {
      vals.add(get(key));
    }
    return vals;
  }


  /**
   * Gets the map as a String.
   *
   * @return a string version of the map
   */
  @Override
  public String toString() {
    StringBuilder buf = new StringBuilder(32 * size());
    buf.append(super.toString()).append("{"). //
        append(Constants.INSTANCE_VAR).append("=").append(this.instance).append(", ").//
        append(Constants.EXP_VAR).append("=").append(this.expression).append(", ").//
        append(Constants.ENV_VAR).append("=").append("<this>");

    Iterator<String> it = keySet().iterator();
    boolean hasNext = it.hasNext();
    if (hasNext) {
      buf.append(", ");
    }
    while (hasNext) {
      String key = it.next();
      Object value = get(key);
      buf.append(key).append('=').append(value == this ? "<this>" : value);

      hasNext = it.hasNext();
      if (hasNext) {
        buf.append(',').append(' ');
      }
    }

    buf.append('}');
    return buf.toString();
  }

  private Map<String, Object> getmOverrides(final boolean readOnly) {
    if (this.mOverrides == null) {
      if (readOnly) {
        return EMPTY_ENV;
      }
      this.mOverrides = new HashMap<>();
    }
    return this.mOverrides;
  }
}
