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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.function.internal.ReducerResult;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.Range;

/**
 * Expression execute environment.Modifed from ChainedMap in jibx.
 *
 * @author dennis
 *
 * @param <String>
 * @param <Object>
 */
public class Env implements Map<String, Object>, Serializable {
  private static final long serialVersionUID = -7793716992176999689L;

  /** Default values map. */
  private final Map<String, Object> mDefaults;

  /**
   * Current evaluator instance that executes current expression.
   */
  private transient AviatorEvaluatorInstance instance;

  /** Override values map. */
  private Map<String, Object> mOverrides;

  private Expression expression;

  private List<String> importedSymbols;

  private List<String> importedPackages;

  // Caching resolved classes
  private transient Map<String/* class name */, Class<?>> resolvedClasses;

  public static final Map<String, Object> EMPTY_ENV = Collections.emptyMap();

  // The execution start timestamp in nanoseconds.
  private long startNs = -1;

  public static class IntCounter {
    transient int n = 0;
  }

  // The "execution" checkpoint times
  private transient IntCounter checkPoints = null;



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


  public long getStartNs() {
    return startNs;
  }

  public int incExecCheckpointsAndGet() {
    if (this.checkPoints == null) {
      checkPoints = new IntCounter();
    }
    return ++checkPoints.n;
  }


  public IntCounter getCheckPoints() {
    return checkPoints;
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

  // Configure the env.
  public void configure(final AviatorEvaluatorInstance instance, final Expression exp, long startNs,
      IntCounter checkPoints) {
    this.instance = instance;
    this.expression = exp;
    setStats(startNs, checkPoints);
  }

  private void setStats(long startNs, IntCounter checkPoints) {
    if (this.startNs == -1 && startNs > 0) {
      this.startNs = startNs;
      this.checkPoints = checkPoints;
    }
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
    // from cache
    Class<?> clazz = retrieveFromCache(name);
    if (clazz == NullClass.class) {
      throw new ClassNotFoundException(name);
    }
    if (clazz == null) {
      if (name.contains(".")) {
        clazz = classForName(name);
      } else {
        // java.lang.XXX
        clazz = classForName("java.lang." + name);
        // from imported packages
        if (clazz == null) {
          clazz = resolveFromImportedPackages(name);
        }
        // from imported classes
        if (clazz == null) {
          clazz = resolveFromImportedSymbols(name, clazz);
        }
        // try to find from parent env.
        if (clazz == null && this.mDefaults instanceof Env) {
          clazz = ((Env) this.mDefaults).resolveClassSymbol(name, checkIfAllow);
        }
      }
    }

    if (clazz == null) {
      put2cache(name, NullClass.class);
      throw new ClassNotFoundException(name);
    }
    put2cache(name, clazz);
    return this.instance.checkIfClassIsAllowed(checkIfAllow, clazz);
  }

  private Class<?> resolveFromImportedPackages(final String name) {
    Class<?> clazz = null;
    if (this.importedPackages != null) {
      for (String pkg : this.importedPackages) {
        clazz = classForName(pkg + name);
        if (clazz != null) {
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

  static class TargetObjectTask implements GetValueTask {

    public TargetObjectTask(Object target) {
      super();
      this.target = target;
    }

    Object target;

    @Override
    public Object call(Env env) {
      return target;
    }

  }

  static interface GetValueTask {
    Object call(Env env);
  }

  /**
   * Internal variable tasks to get the value.
   */
  private static final IdentityHashMap<String, GetValueTask> INTERNAL_VARIABLES =
      new IdentityHashMap<String, GetValueTask>();

  static {
    INTERNAL_VARIABLES.put(Constants.REDUCER_LOOP_VAR, new TargetObjectTask(Range.LOOP));
    INTERNAL_VARIABLES.put(Constants.REDUCER_EMPTY_VAR,
        new TargetObjectTask(ReducerResult.withEmpty(AviatorNil.NIL)));
    INTERNAL_VARIABLES.put(Constants.ENV_VAR, new GetValueTask() {

      @Override
      public Object call(Env env) {
        env.instance.ensureFeatureEnabled(Feature.InternalVars);
        return env;
      }

    });
    INTERNAL_VARIABLES.put(Constants.FUNC_ARGS_VAR, new GetValueTask() {

      @Override
      public Object call(Env env) {
        env.instance.ensureFeatureEnabled(Feature.InternalVars);
        return FunctionUtils.getFunctionArguments(env);
      }

    });
    INTERNAL_VARIABLES.put(Constants.INSTANCE_VAR, new GetValueTask() {

      @Override
      public Object call(Env env) {
        env.instance.ensureFeatureEnabled(Feature.InternalVars);
        return env.instance;
      }

    });

    INTERNAL_VARIABLES.put(Constants.EXP_VAR, new GetValueTask() {

      @Override
      public Object call(Env env) {
        env.instance.ensureFeatureEnabled(Feature.InternalVars);
        return env.expression;
      }

    });

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
    GetValueTask task = INTERNAL_VARIABLES.get(key);
    if (task != null) {
      return task.call(this);
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
      this.mOverrides = new ArrayHashMap<>();
      // this.mOverrides = new HashMap<>();
    }
    return this.mOverrides;
  }

  /**
   * Default Value when cannot resolve class symbol.
   */
  static class NullClass {
  }
}
