package java.lang;

import java.util.HashMap;

/**
 * A generic mapping implementation with default value support.
 *
 * @param <K> Type of the key
 * @param <V> Type of the value
 */
public class Mapping<K, V> {
    private final HashMap<K, V> map = new HashMap<>();
    private final V defaultValue;

    /**
     * Default constructor with null default value.
     */
    public Mapping() {
        this.defaultValue = null;
    }

    /**
     * Constructor with a default value.
     *
     * @param defaultValue The default value to return when a key is not found.
     */
    public Mapping(V defaultValue) {
        this.defaultValue = defaultValue;
    }


    /**
     * Retrieves the value associated with the given key.
     * If the key does not exist, it returns the default value.
     *
     * @param key The key to look up.
     * @return The associated value, or the default value if the key is missing.
     */
    public V get(K key) {
        if (map.containsKey(key)) {
            return map.get(key);
        }
        return defaultValue;
    }

    /**
     * Sets a value for the given key.
     *
     * @param key   The key to set.
     * @param value The value to associate with the key.
     */
    public void set(K key, V value) {
        if (key == null) {
            throw new IllegalArgumentException("Key cannot be null");
        }
        map.put(key, value);
    }

    /**
     * Checks if a key exists in the mapping.
     *
     * @param key The key to check.
     * @return true if the key exists, false otherwise.
     */
    public boolean containsKey(K key) {
        return map.containsKey(key);
    }

    /**
     * Removes a key from the mapping.
     *
     * @param key The key to remove.
     */
    public void remove(K key) {
        map.remove(key);
    }

    /**
     * Clears all key-value pairs in the mapping.
     */
    public void clear() {
        map.clear();
    }

    /**
     * Gets the default value for this mapping.
     *
     * @return The default value.
     */
    public V getDefaultValue() {
        return defaultValue;
    }

    /**
     * Checks if the mapping has a default value set.
     *
     * @return true if a default value is set, false otherwise.
     */
    public boolean hasDefaultValue() {
        return defaultValue != null;
    }
}
