package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_CHILD_A)
public class OWLChildClassA implements OWLParentA, OWLParentB {
    @OWLAnnotationProperty(iri = Vocabulary.DC_SOURCE)
    private Set<String> pluralAnnotationProperty;
    @OWLDataProperty(iri = Vocabulary.P_E_STRING_ATTRIBUTE)
    private String stringAttribute;
    @Id
    private URI id;

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }
    @Types(fetchType = FetchType.EAGER)
    private Set<String> types;

    public Set<String> getTypes() {
        return types;
    }

    public void setTypes(Set<String> types) {
        this.types = types;
    }

    @Override
    public String getStringAttribute() {
        return stringAttribute;
    }

    @Override
    public void setStringAttribute(String stringAttribute) {
        this.stringAttribute = stringAttribute;
    }

    @Override
    public Set<String> getPluralAnnotationProperty() {
        return pluralAnnotationProperty;
    }

    @Override
    public void setPluralAnnotationProperty(Set<String> pluralAnnotationProperty) {
        this.pluralAnnotationProperty = pluralAnnotationProperty;
    }

}
