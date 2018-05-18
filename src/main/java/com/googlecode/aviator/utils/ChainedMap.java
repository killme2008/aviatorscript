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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Partial implementation of {@link java.util.Map} which provides a merged view of a defaults map
 * with an overrides map. Although this can be used as a map for most purposes, methods which return
 * live views of the keys or values in the map only take into account the overrides, not the
 * defaults.
 */
public class ChainedMap<K, V> implements Map<K, V> {
  /** Default values map. */
  private final Map<K, V> mDefaults;

  /** Override values map. */
  private final Map<K, V> mOverrides;

  public Map<K, V> getDefaults() {
    return mDefaults;
  }

  /**
   * Constructor.
   *
   * @param defaults map providing defaults for keys not set directly
   */
  public ChainedMap(Map<K, V> defaults) {
    mDefaults = defaults;
    mOverrides = new HashMap<K, V>();
  }

  /**
   * Clear all override key-value pairs. This only effects the overrides, not the defaults.
   */
  @Override
  public void clear() {
    mDefaults.clear();
  }

  /**
   * Check if a key has a defined value. This will return <code>true</code> if the key is present in
   * the overrides map with a non-null value, or if the key is not present in the overrides map but
   * is present in the defaults map.
   *
   * @param key
   * @return <code>true</code> if key defined, <code>false</code> if not
   */
  @Override
  public boolean containsKey(Object key) {
    if (mOverrides.containsKey(key)) {
      return mOverrides.get(key) != null;
    } else {
      return mDefaults.containsKey(key);
    }
  }

  /**
   * Check if a value is present.
   *
   * @param value
   * @return <code>true</code> if value present as an override, <code>false</code> if not
   */
  @Override
  public boolean containsValue(Object value) {
    return mOverrides.containsValue(value) || mDefaults.containsValue(value);
  }

  /**
   * Get the set of entries.
   *
   * @return override entries
   */
  @Override
  public Set<Entry<K, V>> entrySet() {
    Set<Entry<K, V>> ret = new HashSet<Entry<K, V>>(mDefaults.entrySet());
    ret.addAll(this.mOverrides.entrySet());
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
  public V get(Object key) {
    if (mOverrides.containsKey(key)) {
      return mOverrides.get(key);
    } else {
      return mDefaults.get(key);
    }
  }

  /**
   * Check if no overrides are defined.
   *
   * @return <code>true</code> if no overrides, <code>false</code> if any present
   */
  @Override
  public boolean isEmpty() {
    return mOverrides.isEmpty();
  }

  /**
   * Get the set of keys. This only returns the keys in the overrides map.
   *
   * @return keys
   */
  @Override
  public Set<K> keySet() {
    Set<K> ret = new HashSet<K>(mDefaults.keySet());
    ret.addAll(this.mOverrides.keySet());
    return ret;
  }

  /**
   * Set an override value. This just adds the key-value pair to the override map.
   *
   * @param key
   * @param value
   * @return previous value for key (from default map, if not present in overrides)
   */
  @Override
  public V put(K key, V value) {
    Object prior;
    if (mOverrides.containsKey(key)) {
      prior = mOverrides.put(key, value);
    } else {
      mOverrides.put(key, value);
      prior = mDefaults.get(key);
    }
    return (V) prior;
  }

  /**
   * Add all key-value pairs from another map into the overrides map.
   *
   * @param map
   */
  @Override
  public void putAll(Map map) {
    mOverrides.putAll(map);
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
  public V remove(Object key) {
    if (mOverrides.containsKey(key)) {
      return mOverrides.remove(key);
    } else {
      return mDefaults.remove(key);
    }
  }

  /**
   * Get the number of entries in the map. This returns the entry count for the overrides map only.
   *
   * @return entry count
   */
  @Override
  public int size() {
    return mOverrides.size();
  }

  /**
   * Get the values. This returns only the values in the overrides map.
   *
   * @return values
   */
  @Override
  public Collection<V> values() {
    return mOverrides.values();
  }
}
