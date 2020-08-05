package cz.cvut.kbss.jopa.model;

import java.io.Serializable;
import java.util.*;

/**
 * Represents a string with translations to (possibly) multiple languages.
 * <p>
 * This allows an application to naturally work in an internationalized environment leveraging the <a
 * href="https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string">language-tagged strings</a> of the Semantic
 * Web.
 * <p>
 * Instances of this class basically represent all available translations of the string, allowing to read (and write)
 * particular versions by specifying the corresponding language tag. A special case supported by this class are <a
 * href="https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal">simple literals</a> (language-less strings with
 * type {@code xsd:string}), for which the language tag is {@code null}.
 * <p>
 * Note that this class is not thread-safe.
 */
public class MultilingualString implements Serializable {

    private final Map<String, String> value = new HashMap<>(8);

    /**
     * Sets value in the specified language.
     * <p>
     * This overrides any previous value in the specified language, if it existed.
     *
     * @param value    String value in the specified language
     * @param language Language to use with the specified value. Passing {@code null} has the same effect as {@link
     *                 #set(String)}
     * @see #set(String)
     */
    public void set(String value, String language) {
        Objects.requireNonNull(value);
        this.value.put(language, value);
    }

    /**
     * Sets value without language.
     * <p>
     * That is, the specified value will be stored as a simple literal (type xsd:string).
     *
     * @param value Value to set
     * @see #set(String, String)
     */
    public void set(String value) {
        Objects.requireNonNull(value);
        this.value.put(null, value);
    }

    /**
     * Gets value for the specified language.
     * <p>
     * If no language is specified, either the simple literal value is returned (if present), or any other existing
     * value is returned. However, note that, in case of missing simple literal, repeated calls may return values in
     * different languages. If there are no translations, {@code null} is returned.
     *
     * @param language Requested language (language tag). Can be {@code null}
     * @return Value of this string for the specified language (or {@code null} if not available)
     */
    public String get(String language) {
        return language != null ? value.get(language) : get();
    }

    /**
     * Gets value of simple literal represented by this instance.
     * <p>
     * If this instances does not represent a simple literal, any other existing value is returned. However, note that
     * in that case repeated calls may return values in different languages.
     * <p>
     * If this object is empty (i.e., neither simple literal nor any translations are available), {@code null} is
     * returned.
     *
     * @return Value of simple literal represented by this string
     */
    public String get() {
        return value.getOrDefault(null, value.isEmpty() ? null : value.get(value.keySet().iterator().next()));
    }

    /**
     * Checks whether this string contains value in the specified language.
     * <p>
     * If no language is specified (argument is {@code null}), this method will return {@code true} if there is value
     * without language tag (simple literal) or in any other language.
     *
     * @param language Requested language (language tag)
     * @return {@code true} if this string exists in the specified language
     */
    public boolean contains(String language) {
        return value.containsKey(language) || (language == null && !value.isEmpty());
    }

    /**
     * Gets the set of languages for which translations exist in the instance.
     * <p>
     * Note that the result may contain {@code null}, indicating a simple literal
     *
     * @return Set of languages available in this multilingual string
     */
    public Set<String> getLanguages() {
        return Collections.unmodifiableSet(value.keySet());
    }

    /**
     * Gets the translations contained in this instance.
     *
     * Convenience method for accessing all values at once.
     *
     * @return Map of language -> translation values
     */
    public Map<String, String> getValue() {
        return Collections.unmodifiableMap(value);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MultilingualString)) {
            return false;
        }
        MultilingualString that = (MultilingualString) o;
        return value.equals(that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    /**
     * Creates a new instance of {@link MultilingualString} and sets the specified value in the specified language.
     * <p>
     * Convenience method for creating strings with one (initial) translation.
     *
     * @param value    String value
     * @param language Language of the value (language tag)
     * @return New instance of {@code MultiLangString}
     */
    public static MultilingualString create(String value, String language) {
        final MultilingualString instance = new MultilingualString();
        instance.set(value, language);
        return instance;
    }
}
