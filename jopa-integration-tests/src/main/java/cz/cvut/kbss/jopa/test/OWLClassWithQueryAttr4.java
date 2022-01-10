package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Sparql;

import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = Vocabulary.C_OwlClassWithQueryAttr4)
public class OWLClassWithQueryAttr4 {

    private static final String STR_ATT_FIELD = "stringAttribute";
    private static final String ASK_QUERY_ATT_FIELD = "askQueryAttribute";
    private static final String QUERY = "ASK" +
                                        "WHERE {?this <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> ?stringAttribute}";

    @Id
    private URI uri;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute", simpleLiteral = true)
    private String stringAttribute;

    @Sparql(query=QUERY)
    private Boolean askQueryAttribute;

    public OWLClassWithQueryAttr4() {
    }

    public OWLClassWithQueryAttr4(URI uri) {
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

    public String getStringAttribute() {
        return stringAttribute;
    }

    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    public Boolean getAskQueryAttribute() {
        return askQueryAttribute;
    }

    public void setAskQueryAttribute(Boolean askQueryAttribute) {
        this.askQueryAttribute = askQueryAttribute;
    }

    public static String getClassIri() {
        return OWLClassWithQueryAttr4.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getStrAttField() throws NoSuchFieldException {
        return OWLClassWithQueryAttr4.class.getDeclaredField(STR_ATT_FIELD);
    }

    public static Field getStrQueryAttField() throws NoSuchFieldException {
        return OWLClassWithQueryAttr4.class.getDeclaredField(ASK_QUERY_ATT_FIELD);
    }

    public static String getSparqlQuery() {
        return QUERY;
    }

    @Override
    public String toString() {
        String out = "OWLClassWithQueryAttr: uri = " + uri;
        out += ", stringAttribute = " + stringAttribute;
        return out;
    }
}
