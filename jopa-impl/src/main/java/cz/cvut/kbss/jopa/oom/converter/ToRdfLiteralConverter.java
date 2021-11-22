package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Objects;

/**
 * Converter for working with RDF literals with explicit datatype mapping ({@link cz.cvut.kbss.ontodriver.model.Literal}).
 * <p>
 * The attribute value then represents the lexical form of the literal and should be a {@code String}.
 */
public class ToRdfLiteralConverter implements ConverterWrapper<String, Object> {

    private final String datatype;

    public ToRdfLiteralConverter(String datatype) {
        this.datatype = Objects.requireNonNull(datatype);
    }

    @Override
    public Object convertToAxiomValue(String value) {
        return value != null ? new Literal(value, datatype) : null;
    }

    @Override
    public String convertToAttribute(Object value) {
        if (value instanceof Literal) {
            return ((Literal) value).getLexicalForm();
        }
        if (value instanceof LangString) {
            return ((LangString) value).getValue();
        }
        // TODO Verify datatype match?
        return value.toString();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return !NamedResource.class.isAssignableFrom(type) && !URI.class.equals(type);
    }
}
