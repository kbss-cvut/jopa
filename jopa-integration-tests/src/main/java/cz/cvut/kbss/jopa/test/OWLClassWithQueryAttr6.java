package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OwlClassWithQueryAttr6)
public class OWLClassWithQueryAttr6 {

    private static final String QUERY = "SELECT ?pluralAttribute\n" +
            "WHERE {?this <" + Vocabulary.P_HAS_SIMPLE_LIST + "> ?pluralAttribute}";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_SIMPLE_LIST, cascade = CascadeType.ALL)
    private Set<OWLClassA> pluralAttribute;

    @Sparql(query=QUERY, fetchType = FetchType.LAZY)
    private Set<OWLClassA> pluralQueryAttribute;

    public OWLClassWithQueryAttr6() {
    }

    public OWLClassWithQueryAttr6(URI uri) {
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

    public Set<OWLClassA> getPluralAttribute() {
        return pluralAttribute;
    }

    public void setPluralAttribute(Set<OWLClassA> pluralAttribute) {
        this.pluralAttribute = pluralAttribute;
    }

    public Set<OWLClassA> getPluralQueryAttribute() {
        return pluralQueryAttribute;
    }

    public void setPluralQueryAttribute(Set<OWLClassA> pluralQueryAttribute) {
        this.pluralQueryAttribute = pluralQueryAttribute;
    }

    public static String getClassIri() {
        return OWLClassWithQueryAttr6.class.getAnnotation(OWLClass.class).iri();
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
