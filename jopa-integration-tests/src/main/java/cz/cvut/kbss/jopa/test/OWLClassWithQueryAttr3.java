package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Sparql;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OwlClassWithQueryAttr3)
public class OWLClassWithQueryAttr3 {

    private static final String QUERY = "SELECT ?pluralAttribute\n" +
                                        "WHERE {?this <" + Vocabulary.P_HAS_SIMPLE_LIST + "> ?pluralAttribute}";

    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.P_HAS_SIMPLE_LIST, simpleLiteral = true)
    private Set<String> pluralAttribute;

    @Sparql(query=QUERY)
    private Set<String> pluralQueryAttribute;

    public OWLClassWithQueryAttr3() {
    }

    public OWLClassWithQueryAttr3(URI uri) {
        this.uri = uri;
    }

    /**
     * @param uri the uri to set
     */
    public void setUri(URI uri) {
        this.uri = uri;
    }

    /**
     * @return the uri
     */
    public URI getUri() {
        return uri;
    }

    public Set<String> getPluralAttribute() {
        return pluralAttribute;
    }

    public void setPluralAttribute(Set<String> pluralAttribute) {
        this.pluralAttribute = pluralAttribute;
    }

    public Set<String> getPluralQueryAttribute() {
        return pluralQueryAttribute;
    }

    public void setPluralQueryAttribute(Set<String> pluralQueryAttribute) {
        this.pluralQueryAttribute = pluralQueryAttribute;
    }

    public static String getClassIri() {
        return OWLClassWithQueryAttr3.class.getAnnotation(OWLClass.class).iri();
    }

    public static String getSparqlQuery() {
        return QUERY;
    }

    @Override
    public String toString() {
        String out = "OWLClassWithQueryAttr: uri = " + uri;
        out += ", pluralAttribute = " + pluralAttribute;
        return out;
    }
}
