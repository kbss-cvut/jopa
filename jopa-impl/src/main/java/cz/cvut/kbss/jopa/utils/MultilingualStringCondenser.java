package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.LangString;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Condenses translations into as few {@link cz.cvut.kbss.jopa.model.MultilingualString}s as possible.
 */
public class MultilingualStringCondenser {

    private final List<MultilingualString> values = new ArrayList<>();

    /**
     * Adds the specified {@code LangString} to this condenser.
     *
     * <p>If {@code value} contains a language tag, the method attempts to find an
     * existing {@link MultilingualString} in the internal collection that does not already have a value for that
     * language. The value is set on that {@code MultilingualString}; if no suitable instance is found, a new
     * {@code MultilingualString} containing the value and its language is created and added to the collection.</p>
     *
     * <p>If {@code value} has no language tag, the method looks for an existing
     * {@code MultilingualString} that does not yet contain a simple (language‑less) value. The value is stored there,
     * or a new {@code MultilingualString} representing a simple value is created and added.</p>
     *
     * @param value the {@link LangString} to be added to the condenser
     */
    public void add(LangString value) {
        if (value.getLanguage().isPresent()) {
            String language = value.getLanguage().get();
            for (MultilingualString mls : values) {
                if (!mls.contains(language)) {
                    mls.set(language, value.getValue());
                    return;
                }
            }
            final MultilingualString newOne = MultilingualString.create(value.getValue(), language);
            values.add(newOne);
        } else {
            for (MultilingualString mls : values) {
                if (!mls.containsSimple()) {
                    mls.set(value.getValue());
                    return;
                }
            }
            final MultilingualString newOne = MultilingualString.create(value.getValue(), null);
            values.add(newOne);
        }
    }

    /**
     * Gets the values.
     *
     * @return Unmodifiable list of multilingual strings
     */
    public List<MultilingualString> getValues() {
        return Collections.unmodifiableList(values);
    }
}
