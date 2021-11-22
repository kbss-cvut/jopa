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
 * <p>
 * Note that the conversion to attribute value does not perform any validation regarding match between the value and the specified datatype IRI.
 * So, for example, a value read from the repository as {@code Integer} will be mapped by this converter to an attribute with
 * datatype {@code xsd:boolean} without any exceptions.
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
        return value.toString();
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return !NamedResource.class.isAssignableFrom(type) && !URI.class.equals(type);
    }
}
