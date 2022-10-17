package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.query.QueryHints;
import cz.cvut.kbss.ontodriver.Statement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Processes query hints.
 * <p>
 * To add a new hint (inspired by Eclipselink):
 * <ul>
 *     <li>Define a hint in {@link QueryHints}</li>
 *     <li>Ad an inner class extending {@link Hint} corresponding to the new hint. The class should implement the
 *     {@link Hint#applyToQuery(Object, AbstractQuery, Statement)} method and provide an array of values for transformation.</li>
 *     <li>Register the new hint class by calling {@link Hint#registerHint(Hint)} with an instance of the hint</li>
 * </ul>
 */
public class QueryHintsHandler {

    private static final Logger LOG = LoggerFactory.getLogger(QueryHintsHandler.class);

    /**
     * Gets query hints supported by this implementation.
     *
     * @return A set of query hint names
     */
    public static Set<String> getSupportedHints() {
        return Hint.getSupportedHints();
    }

    /**
     * Applies the specified hint value to the specified query.
     * <p>
     * Note that if the hint is unknown, it is ignored.
     *
     * @param hintName  Query hint name
     * @param hintValue Query hint value
     * @param query     Query to apply the hint value to
     * @param statement OntoDriver statement that will be used to execute the specified query. The hint value may be
     *                  applied directly to it
     * @throws IllegalArgumentException If the hint value is not supported
     */
    public static void apply(String hintName, Object hintValue, AbstractQuery query, Statement statement) {
        Hint.apply(hintName, hintValue, query, statement);
    }

    /**
     * Representation of a query hint.
     * <p>
     * The {@code valueArray} ensures that only supported values are provided to {@link #applyToQuery(Object,
     * AbstractQuery, Statement)}. For example, if a hint is boolean-based and the configuration passes string "true",
     * it is mapped to {@code true} based on {@code valueMap} which was constructed automatically from {@code
     * valueArray}.
     * <p>
     * The {@code valueArray} is a two-dimensional array where each element is a single value, which in turn is a
     * two-element array where the first element is a string representation of the value and the second one is the value
     * itself.
     */
    static abstract class Hint {

        private static final Map<String, Hint> HINTS = new HashMap<>();

        Object[][] valueArray;
        HashMap<String, Object> valueMap;
        Object defaultValue;
        String name;

        Hint(String name, Object defaultValue) {
            this.name = name;
            this.defaultValue = defaultValue;
        }

        static void registerHint(Hint hint) {
            hint.initialize();
            HINTS.put(hint.name, hint);
        }

        static Set<String> getSupportedHints() {
            return HINTS.keySet();
        }

        static String toUpperCase(String str) {
            return str.toUpperCase(Locale.ROOT);
        }

        static boolean shouldUseDefault(Object hintValue) {
            return hintValue instanceof String && ((String) hintValue).isEmpty();
        }

        static void apply(String hintName, Object hintValue, AbstractQuery query, Statement statement) {
            if (!HINTS.containsKey(hintName)) {
                LOG.warn("Unsupported query hint '{}'.", hintName);
                return;
            }
            HINTS.get(hintName).apply(hintValue, query, statement);
        }

        void apply(Object hintValue, AbstractQuery query, Statement statement) {
            Object toApply = hintValue;
            if (shouldUseDefault(hintValue)) {
                toApply = defaultValue;
            } else if (valueMap != null) {
                toApply = valueMap.get(toUpperCase(hintValue.toString()));
                if (toApply == null) {
                    throw new IllegalArgumentException("Unsupported value '" + hintValue + "' of hint '" + name + "'.");
                }
            }
            applyToQuery(toApply, query, statement);
        }

        void initialize() {
            if (valueArray != null) {
                this.valueMap = new HashMap<>(valueArray.length);
                for (Object[] elem : valueArray) {
                    valueMap.put(toUpperCase(elem[0].toString()), elem[1]);
                }
                // Don't need it anymore
                this.valueArray = null;
            }
        }

        /**
         * Applies the specified value of this hint to the specified query (or statement, if more suitable).
         *
         * @param hintValue Value of the hint to apply
         * @param query     Query to apply the value to
         * @param statement OntoDriver statement that will be used to execute the specified query. The hint value may be
         *                  applied directly to it
         */
        abstract void applyToQuery(Object hintValue, AbstractQuery query, Statement statement);
    }

    /**
     * Allows disabling inference for query execution.
     *
     * @see QueryHints#DISABLE_INFERENCE
     */
    protected static class DisableInferenceHint extends Hint {

        static {
            registerHint(new DisableInferenceHint());
        }

        DisableInferenceHint() {
            super(QueryHints.DISABLE_INFERENCE, Boolean.FALSE);
            this.valueArray =
                    new Object[][]{{Boolean.TRUE.toString(), Boolean.TRUE}, {Boolean.FALSE.toString(), Boolean.FALSE},};
        }

        @Override
        void applyToQuery(Object hintValue, AbstractQuery query, Statement statement) {
            assert statement != null;
            if (Boolean.TRUE == hintValue) {
                statement.disableInference();
            }
        }
    }
}
