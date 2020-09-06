package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.c_OwlClassU)
public class OWLClassU implements Serializable {

    @Id(generated = true)
    private URI id;

    @OWLDataProperty(iri = Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE)
    private MultilingualString singularStringAtt;

    @OWLDataProperty(iri = Vocabulary.P_U_PLURAL_MULTILINGUAL_ATTRIBUTE)
    private Set<MultilingualString> pluralStringAtt;

    public OWLClassU() {
    }

    public OWLClassU(URI id) {
        this.id = id;
    }

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    public MultilingualString getSingularStringAtt() {
        return singularStringAtt;
    }

    public void setSingularStringAtt(MultilingualString singularStringAtt) {
        this.singularStringAtt = singularStringAtt;
    }

    public Set<MultilingualString> getPluralStringAtt() {
        return pluralStringAtt;
    }

    public void setPluralStringAtt(Set<MultilingualString> pluralStringAtt) {
        this.pluralStringAtt = pluralStringAtt;
    }

    @Override
    public String toString() {
        return "OWLClassU{" +
                "id=" + id +
                ", singularStringAtt=" + singularStringAtt +
                ", pluralStringAtt=" + pluralStringAtt +
                '}';
    }

    public static String getClassIri() {
        return OWLClassU.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getIdField() throws Exception {
        return OWLClassU.class.getDeclaredField("id");
    }

    public static Field getSingularStringAttField() throws Exception {
        return OWLClassU.class.getDeclaredField("singularStringAtt");
    }

    public static Field getPluralStringAttField() throws Exception {
        return OWLClassU.class.getDeclaredField("pluralStringAtt");
    }
}
