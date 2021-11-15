package cz.cvut.kbss.ontodriver.model;

import java.io.Serializable;
import java.util.Objects;

/**
 * Represents an RDF literal consisting of a lexical form and datatype IRI.
 * <p>
 * To represent language-tagged string values, use {@link LangString}.
 */
public class Literal implements Serializable {

    private final String lexicalForm;

    private final String datatype;

    public Literal(String lexicalForm, String datatype) {
        this.lexicalForm = Objects.requireNonNull(lexicalForm);
        this.datatype = Objects.requireNonNull(datatype);
    }

    /**
     * Gets the lexical form of this literal.
     *
     * @return String containing the lexical form
     */
    public String getLexicalForm() {
        return lexicalForm;
    }

    /**
     * Gets the datatype identifier.
     *
     * @return String containing the datatype IRI
     */
    public String getDatatype() {
        return datatype;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Literal)) {
            return false;
        }
        Literal literal = (Literal) o;
        return lexicalForm.equals(literal.lexicalForm) && datatype.equals(literal.datatype);
    }

    @Override
    public int hashCode() {
        return Objects.hash(lexicalForm, datatype);
    }

    @Override
    public String toString() {
        return "\"" + lexicalForm + "\"^^<" + datatype + ">";
    }
}
