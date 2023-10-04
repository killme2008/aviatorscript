package com.googlecode.aviator.utils;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ArrayHashMap<K, V> extends AbstractMap<K, V>
    implements Map<K, V>, Cloneable, Serializable {

  private static final float LOAD_FACTOR = 0.85f;

  private static final long serialVersionUID = 362498820763181265L;

  private static class MapEntry<K, V> implements Map.Entry<K, V>, Serializable {

    private static final long serialVersionUID = 1759214536880718767L;
    K key;
    V value;
    int hash;
    boolean deleted;

    public MapEntry(final K key, final V value, final int hash) {
      super();
      this.key = key;
      this.value = value;
      this.hash = hash;
    }

    @Override
    public K getKey() {
      return this.key;
    }

    @Override
    public V getValue() {
      return this.value;
    }

    @Override
    public V setValue(final V value) {
      V old = this.value;
      this.value = value;
      return old;
    }
  }

  private MapEntry<K, V>[] entries;

  private int size;

  private int loadThreshold;

  static final int hash(final Object key) {
    int h;
    return (key == null) ? 0 : (h = key.hashCode()) ^ (h >>> 16);
  }

  public ArrayHashMap() {
    this.clear();
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  private void setEntries(final MapEntry[] entries) {
    this.entries = entries;
    this.loadThreshold = Math.round(this.entries.length * LOAD_FACTOR);
  }

  @Override
  public int size() {
    return this.size;
  }

  @Override
  public boolean containsKey(final Object key) {
    MapEntry<K, V>[] table = this.entries;

    if (this.size == 0) {
      return false;
    }

    int hash = hash(key);
    int index = hash & (table.length - 1);
    for (int i = index; i < table.length; i++) {
      MapEntry<K, V> entry = table[i];
      if (entry == null) {
        return false;
      }

      if (entry.deleted) {
        if (entry.hash == hash && entry.key.equals(key)) {
          return false;
        }
      } else if (entry.hash == hash && entry.key.equals(key)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public V get(final Object key) {
    MapEntry<K, V>[] table = this.entries;

    int hash = hash(key);
    int index = hash & (table.length - 1);
    for (int i = index; i < table.length; i++) {
      MapEntry<K, V> entry = table[i];
      if (entry == null) {
        return null;
      }

      if (entry.deleted) {
        if (entry.hash == hash && entry.key.equals(key)) {
          return null;
        }
      } else if (entry.hash == hash && entry.key.equals(key)) {
        return entry.value;
      }
    }
    return null;
  }

  @Override
  public void clear() {
    this.size = 0;
    setEntries(new MapEntry[8]);
  }

  @Override
  public V put(final K key, final V value) {
    int hash = hash(key);

    MapEntry<K, V>[] table = this.entries;
    while (true) {

      int index = hash & (table.length - 1);
      MapEntry<K, V> tombstone = null;
      for (int i = index; i < table.length; i++) {
        MapEntry<K, V> entry = table[i];
        if (entry != null && !entry.deleted) {
          if (entry.hash == hash && entry.key.equals(key)) {
            return entry.setValue(value);
          }
        } else if (entry != null && entry.deleted) {
          if (tombstone == null) {
            tombstone = entry;
          }
        } else if (tombstone != null) {
          this.size++;
          tombstone.key = key;
          tombstone.hash = hash;
          tombstone.deleted = false;
          resizeIfLoadHigh(table);

          return tombstone.setValue(value);
        } else {
          entry = new MapEntry<K, V>(key, value, hash);
          table[i] = entry;
          this.size++;
          resizeIfLoadHigh(table);

          return null;
        }
      }

      table = resize(table);
      setEntries(table);
    }
  }

  private void resizeIfLoadHigh(final MapEntry<K, V>[] table) {
    if (this.size >= this.loadThreshold) {
      setEntries(this.resize(table));
    }
  }

  @SuppressWarnings("unchecked")
  private MapEntry<K, V>[] resize(final MapEntry<K, V>[] table) {
    MapEntry<K, V>[] newTable = new MapEntry[table.length * 2];

    for (int i = 0; i < table.length; i++) {
      MapEntry<K, V> entry = table[i];

      if (entry != null && !entry.deleted) {
        int hash = entry.hash;
        int index = hash & (newTable.length - 1);
        boolean success = false;
        for (int j = index; j < newTable.length; j++) {
          if (newTable[j] == null) {
            newTable[j] = entry;
            success = true;
            break;
          }
        }
        assert (success);
      }
    }
    return newTable;
  }


  @Override
  public V remove(final Object key) {
    MapEntry<K, V>[] table = this.entries;

    int hash = hash(key);
    int index = hash & (table.length - 1);
    for (int i = index; i < table.length; i++) {
      MapEntry<K, V> entry = table[i];
      if (entry == null) {
        return null;
      }

      if (entry.deleted) {
        if (entry.hash == hash && entry.key.equals(key)) {
          return null;
        }
      }

      if (entry.hash == hash && entry.key.equals(key)) {
        entry.deleted = true;
        entry.hash = 0;
        this.size--;
        return entry.setValue(null);
      }
    }
    return null;
  }

  @Override
  public Set<Entry<K, V>> entrySet() {
    Set<Entry<K, V>> entrySet = new HashSet<>();
    for (int i = 0; i < this.entries.length; i++) {
      final MapEntry<K, V> entry = this.entries[i];
      if (entry != null && !entry.deleted) {
        entrySet.add(entry);
      }
    }
    return entrySet;
  }
}
