package com.googlecode.aviator.script;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import javax.script.Bindings;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.utils.Env;

/**
 * Aviator bindings
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class AviatorBindings implements Bindings {
  private final Env env;


  public AviatorBindings() {
    super();
    this.env = new Env();
  }

  public AviatorBindings(final Bindings parentEnv) {
    super();
    this.env = new Env(parentEnv);
  }

  public void setmOverrides(final Map<String, Object> mOverrides) {
    this.env.setmOverrides(mOverrides);
  }

  public Map<String, Object> getDefaults() {
    return this.env.getDefaults();
  }

  public AviatorEvaluatorInstance getInstance() {
    return this.env.getInstance();
  }

  public void setInstance(final AviatorEvaluatorInstance instance) {
    this.env.setInstance(instance);
  }

  @Override
  public void clear() {
    this.env.clear();
  }

  @Override
  public boolean equals(final Object obj) {
    return this.env.equals(obj);
  }

  @Override
  public boolean containsKey(final Object key) {
    return this.env.containsKey(key);
  }

  @Override
  public boolean containsValue(final Object value) {
    return this.env.containsValue(value);
  }

  @Override
  public Set<Entry<String, Object>> entrySet() {
    return this.env.entrySet();
  }

  @Override
  public Object get(final Object key) {
    return this.env.get(key);
  }

  @Override
  public boolean isEmpty() {
    return this.env.isEmpty();
  }

  @Override
  public Set<String> keySet() {
    return this.env.keySet();
  }

  public Object override(final String key, final Object value) {
    return this.env.override(key, value);
  }

  @Override
  public Object put(final String key, final Object value) {
    return this.env.put(key, value);
  }

  @Override
  public void putAll(final Map map) {
    this.env.putAll(map);
  }

  @Override
  public Object remove(final Object key) {
    return this.env.remove(key);
  }

  public Object forgot(final Object key) {
    return this.env.forgot(key);
  }

  @Override
  public int size() {
    return this.env.size();
  }

  @Override
  public Collection<Object> values() {
    return this.env.values();
  }

  @Override
  public String toString() {
    return this.env.toString();
  }

}
