package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.annotations.*;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassS")
public abstract class OWLClassS implements Serializable {

    @Id(generated = true)
    private URI uri;

    @ParticipationConstraints(nonEmpty = true)
    @OWLAnnotationProperty(iri = CommonVocabulary.RDFS_LABEL)
    private String name;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public static String getClassIri() {
        return OWLClassS.class.getDeclaredAnnotation(OWLClass.class).iri();
    }

    public static Field getUriField() throws NoSuchFieldException {
        return OWLClassS.class.getDeclaredField("uri");
    }

    public static Field getNameField() throws NoSuchFieldException {
        return OWLClassS.class.getDeclaredField("name");
    }
}
