package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.annotations.*;

import java.util.Map;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_V)
public class OWLClassV {

    @Id(generated = true)
    private String uri;

    @OWLAnnotationProperty(iri = CommonVocabulary.RDFS_LABEL)
    private String name;

    @OWLAnnotationProperty(iri = CommonVocabulary.DC_DESCRIPTION)
    private String description;

    @Types
    private Set<String> types;

    @Properties(fetchType = FetchType.EAGER)
    private Map<String, Set<String>> properties;

    @OWLObjectProperty(iri = Vocabulary.V_HAS_THING)
    private Set<Thing> things;

    public String getUri() {
        return uri;
    }

    public void setUri(String uri) {
        this.uri = uri;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Set<String> getTypes() {
        return types;
    }

    public void setTypes(Set<String> types) {
        this.types = types;
    }

    public Map<String, Set<String>> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Set<String>> properties) {
        this.properties = properties;
    }

    public Set<Thing> getThings() {
        return things;
    }

    public void setThings(Set<Thing> things) {
        this.things = things;
    }
}
