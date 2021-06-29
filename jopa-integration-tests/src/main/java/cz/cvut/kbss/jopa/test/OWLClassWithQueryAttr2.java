package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;
import java.net.URI;

@SparqlResultSetMapping(name = OWLClassWithQueryAttr2.MAPPING_NAME, entities = {
        @EntityResult(entityClass = OWLClassWithQueryAttr2.class, fields = {
                @FieldResult(name = "uri", variable = "x"),
                @FieldResult(name = "entityAttribute", variable = "y"),
                @FieldResult(name = "entityQueryAttribute", variable = "z")
        })
})
@OWLClass(iri = Vocabulary.C_OwlClassWithQueryAttr2)
public class OWLClassWithQueryAttr2 {

    public static final String MAPPING_NAME = "OWLClassWithQueryAttr2.entityMapping";

    private static final String QUERY_ENTITY = "SELECT ?entityAttribute\n" +
                                               "WHERE {?this <" + Vocabulary.P_HAS_OWL_CLASS_A + "> ?entityAttribute}";

    @Id
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.P_HAS_OWL_CLASS_A, fetch = FetchType.EAGER)
    private OWLClassA entityAttribute;

    @Sparql(query=QUERY_ENTITY)
    private OWLClassA entityQueryAttribute;

    public OWLClassWithQueryAttr2() {
    }

    public OWLClassWithQueryAttr2(URI uri) {
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

    public OWLClassA getEntityAttribute() {
        return entityAttribute;
    }

    public void setEntityAttribute(OWLClassA entityAttribute) {
        this.entityAttribute = entityAttribute;
    }

    public OWLClassA getEntityQueryAttribute() {
        return entityQueryAttribute;
    }

    public void setEntityQueryAttribute(OWLClassA entityQueryAttribute) {
        this.entityQueryAttribute = entityQueryAttribute;
    }

    public static String getClassIri() {
        return OWLClassWithQueryAttr2.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getEntityAttributeField() throws Exception {
        return OWLClassWithQueryAttr2.class.getDeclaredField("entityAttribute");
    }

    @Override
    public String toString() {
        String out = "OWLClassWithQueryAttr2: uri = " + uri;
        out += ", entityAttribute = " + entityAttribute;
        return out;
    }
}
