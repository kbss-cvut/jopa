package cz.cvut.kbss.jopa.oom.model;

import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.test.OWLClassA;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Set;

/**
 * Created by ledvima1 on 12.12.14.
 */
@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassL")
public class OWLClassL {
    private URI uri;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleSequence")
    @ParticipationConstraints({
            @ParticipationConstraint(min = 1, owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
    })
    private List<OWLClassA> simpleList;

    @Sequence(type = SequenceType.referenced)
    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasReferencedSequence")
    @ParticipationConstraints({
            @ParticipationConstraint(max = 2, owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
    })
    private List<OWLClassA> referencedList;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA")
    @ParticipationConstraints({
            @ParticipationConstraint(min = 1, max = 2, owlObjectIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")
    })
    private Set<OWLClassA> set;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public List<OWLClassA> getSimpleList() {
        return simpleList;
    }

    public void setSimpleList(List<OWLClassA> simpleList) {
        this.simpleList = simpleList;
    }

    public List<OWLClassA> getReferencedList() {
        return referencedList;
    }

    public void setReferencedList(List<OWLClassA> referencedList) {
        this.referencedList = referencedList;
    }

    public Set<OWLClassA> getSet() {
        return set;
    }

    public void setSet(Set<OWLClassA> set) {
        this.set = set;
    }

    @Override
    public String toString() {
        return "OWLClassL{" +
                "uri=" + uri +
                ", simpleList=" + simpleList +
                ", referencedList=" + referencedList +
                ", set=" + set +
                '}';
    }

    public static String getClassIri() {
        return OWLClassL.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getReferencedListField() throws NoSuchFieldException {
        return OWLClassL.class.getDeclaredField("referencedList");
    }

    public static Field getSimpleListField() throws NoSuchFieldException {
        return OWLClassL.class.getDeclaredField("simpleList");
    }

    public static Field getSetField() throws NoSuchFieldException {
        return OWLClassL.class.getDeclaredField("set");
    }
}
