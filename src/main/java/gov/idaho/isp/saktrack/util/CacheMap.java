package gov.idaho.isp.saktrack.util;


import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.DelayQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;

public class CacheMap<K, V> implements Map<K, V> {
  private final TimeValue keyExpiration;
  private final Map<K, V> internalMap;
  private final Map<K, ExpiringKey<K>> expiringKeys;
  private final DelayQueue<ExpiringKey> delayQueue = new DelayQueue<>();
  private final boolean renewOnGet;

  public CacheMap() {
    this(false);
  }

  public CacheMap(TimeValue keyExpiration) {
    this(keyExpiration, false);
  }

  public CacheMap(boolean renewKeyExpirationOnGet) {
    this(TimeValue.of(Long.MAX_VALUE, TimeUnit.DAYS), renewKeyExpirationOnGet);
  }

  public CacheMap(TimeValue keyExpiration, boolean renewKeyExpirationOnGet) {
    this.keyExpiration = keyExpiration;
    this.renewOnGet = renewKeyExpirationOnGet;
    this.internalMap = new ConcurrentHashMap<>();
    this.expiringKeys = new WeakHashMap<>();
  }

  public CacheMap(TimeValue keyExpiration, boolean renewKeyExpirationOnGet, int initialCapacity) {
    this.keyExpiration = keyExpiration;
    this.renewOnGet = renewKeyExpirationOnGet;
    this.internalMap = new ConcurrentHashMap<>(initialCapacity);
    this.expiringKeys = new WeakHashMap<>(initialCapacity);
  }

  public CacheMap(TimeValue keyExpiration, boolean renewKeyExpirationOnGet, int initialCapacity, float loadFactor) {
    this.keyExpiration = keyExpiration;
    this.renewOnGet = renewKeyExpirationOnGet;
    this.internalMap = new ConcurrentHashMap<>(initialCapacity, loadFactor);
    this.expiringKeys = new WeakHashMap<>(initialCapacity, loadFactor);
  }

  public TimeValue getDefaultKeyExpiration() {
    return keyExpiration;
  }

  private void cleanCache() {
    ExpiringKey<K> delayedKey = delayQueue.poll();
    while (delayedKey != null) {
      internalMap.remove(delayedKey.getKey());
      expiringKeys.remove(delayedKey.getKey());
      delayedKey = delayQueue.poll();
    }
  }

  @Override
  public int size() {
    cleanCache();
    return internalMap.size();
  }

  @Override
  public boolean isEmpty() {
    cleanCache();
    return internalMap.isEmpty();
  }

  @Override
  public boolean containsKey(Object key) {
    cleanCache();
    return internalMap.containsKey((K) key);
  }

  @Override
  public boolean containsValue(Object value) {
    cleanCache();
    return internalMap.containsValue((V) value);
  }

  @Override
  public V get(Object key) {
    cleanCache();
    if (renewOnGet) {
      renewKey((K) key);
    }
    return internalMap.get((K) key);
  }

  @Override
  public V put(K key, V value) {
    return put(key, value, keyExpiration);
  }

  public V put(K key, V value, TimeValue keyExpiration) {
    cleanCache();
    ExpiringKey delayedKey = new ExpiringKey(key, keyExpiration);
    ExpiringKey oldKey = expiringKeys.put((K) key, delayedKey);
    if (oldKey != null) {
      expireKey(oldKey);
      expiringKeys.put((K) key, delayedKey);
    }
    delayQueue.offer(delayedKey);
    return internalMap.put(key, value);
  }

  @Override
  public void putAll(Map<? extends K, ? extends V> map) {
    for (Map.Entry<? extends K, ? extends V> e : map.entrySet()) {
      put(e.getKey(), e.getValue());
    }
  }

  @Override
  public V remove(Object key) {
    V removedValue = internalMap.remove((K) key);
    expireKey(expiringKeys.remove((K) key));
    return removedValue;
  }

  public boolean renewKey(K key) {
    ExpiringKey<K> delayedKey = expiringKeys.get((K) key);
    if (delayedKey != null) {
      delayedKey.renew();
      return true;
    }
    return false;
  }

  @Override
  public Set<K> keySet() {
    cleanCache();
    return internalMap.keySet();
  }

  @Override
  public Collection<V> values() {
    cleanCache();
    return internalMap.values();
  }

  @Override
  public Set<Map.Entry<K, V>> entrySet() {
    cleanCache();
    return internalMap.entrySet();
  }

  @Override
  public void clear() {
    delayQueue.clear();
    expiringKeys.clear();
    internalMap.clear();
  }

  private void expireKey(ExpiringKey<K> delayedKey) {
    if (delayedKey != null) {
      delayedKey.expire();
      delayQueue.remove(delayedKey);
      cleanCache();
    }
  }

  @Override
  public int hashCode() {
    int hash = 7;
    hash = 29 * hash + Objects.hashCode(this.internalMap);
    hash = 29 * hash + Objects.hashCode(this.expiringKeys);
    return hash;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final CacheMap<?, ?> other = (CacheMap<?, ?>) obj;
    if (!Objects.equals(this.internalMap, other.internalMap)) {
      return false;
    }
    return Objects.equals(this.expiringKeys, other.expiringKeys);
  }

  @Override
  public String toString() {
    return "CacheMap{" + "keyExpiration=" + keyExpiration + ", internalMap=" + internalMap + ", expiringKeys=" + expiringKeys + ", delayQueue=" + delayQueue + ", renewOnGet=" + renewOnGet + '}';
  }

  private class ExpiringKey<K> implements Delayed {
    private long startTime = System.currentTimeMillis();
    private final long timeToLiveMillis;
    private final K key;

    public ExpiringKey(K key, TimeValue timeValue) {
      this.timeToLiveMillis = timeValue.toMillis().getValue();
      this.key = key;
    }

    public K getKey() {
      return key;
    }

    @Override
    public boolean equals(Object obj) {
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      final ExpiringKey<K> other = (ExpiringKey<K>) obj;
      if (this.key != other.key && (this.key == null || !this.key.equals(other.key))) {
        return false;
      }
      return true;
    }

    @Override
    public int hashCode() {
      int hash = 7;
      hash = 31 * hash + (this.key != null ? this.key.hashCode() : 0);
      return hash;
    }

    @Override
    public long getDelay(TimeUnit unit) {
      return unit.convert(getDelayMillis(), TimeUnit.MILLISECONDS);
    }

    private long getDelayMillis() {
      return (startTime + timeToLiveMillis) - System.currentTimeMillis();
    }

    public void renew() {
      startTime = System.currentTimeMillis();
    }

    public void expire() {
      startTime = Long.MIN_VALUE;
    }

    @Override
    public int compareTo(Delayed that) {
      return Long.compare(getDelayMillis(), ((ExpiringKey) that).getDelayMillis());
    }

    @Override
    public String toString() {
      return "ExpiringKey{" + "key=" + key + ", startTime=" + startTime + ", timeToLiveMillis=" + timeToLiveMillis + '}';
    }
  }
}

