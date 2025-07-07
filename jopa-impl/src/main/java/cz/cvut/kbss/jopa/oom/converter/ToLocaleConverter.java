package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.util.Locale;

/**
 * Default converter for instances of {@link java.util.Locale}.
 * <p>
 * It stores the values in language tag form as RDF simple literals.
 */
public class ToLocaleConverter implements ConverterWrapper<Locale, Object> {

    @Override
    public Object convertToAxiomValue(Locale value) {
        return new Literal(value.toLanguageTag(), XSD.STRING);
    }

    @Override
    public Locale convertToAttribute(Object value) {
        if (value instanceof Literal literal) {
            return Locale.forLanguageTag(literal.getLexicalForm());
        } else if (value instanceof LangString langString) {
            return Locale.forLanguageTag(langString.getValue());
        }
        return Locale.forLanguageTag(value.toString());
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return String.class.equals(type) || Literal.class.equals(type) || LangString.class.equals(type);
    }
}
