package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Sparql;

import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = Vocabulary.c_OwlClassWithQueryAttr)
public class OWLClassWithQueryAttr {

    private static final String STR_ATT_FIELD = "stringAttribute";
    private static final String STR_QUERY_ATT_FIELD = "stringQueryAttribute";

    @Id
    private URI uri;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute")
    private String stringAttribute;

    @Sparql("PREFIX jopa:<http://krizik.felk.cvut.cz/ontologies/jopa/>\n" +
            "SELECT ?stringAttribute\n" +
            "WHERE {$this jopa:attributes#B-stringAttribute ?stringAttribute}")
    private String stringQueryAttribute;

    public OWLClassWithQueryAttr() {
    }

    public OWLClassWithQueryAttr(URI uri) {
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

    public String getStringQueryAttribute() {
        return stringQueryAttribute;
    }

    public void setStringQueryAttribute(String stringQueryAttribute) {
        this.stringQueryAttribute = stringQueryAttribute;
    }

    public static String getClassIri() {
        return OWLClassWithQueryAttr.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getStrAttField() throws NoSuchFieldException, SecurityException {
        return OWLClassWithQueryAttr.class.getDeclaredField(STR_ATT_FIELD);
    }

    public static Field getStrQueryAttField() throws NoSuchFieldException {
        return OWLClassWithQueryAttr.class.getDeclaredField(STR_QUERY_ATT_FIELD);
    }

    @Override
    public String toString() {
        String out = "OWLClassWithQueryAttr: uri = " + uri;
        out += ", stringAttribute = " + stringAttribute;
        return out;
    }
}
