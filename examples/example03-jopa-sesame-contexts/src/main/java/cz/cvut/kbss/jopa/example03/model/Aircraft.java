package cz.cvut.kbss.jopa.example03.model;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;

@OWLClass(iri = Vocabulary.Aircraft)
public class Aircraft {

    @Id(generated = true)
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.p_manufacturer)
    private String manufacturer;

    @OWLDataProperty(iri = Vocabulary.p_type)
    private String type;

    @OWLDataProperty(iri = Vocabulary.p_registration)
    private String registration;

    @OWLDataProperty(iri = Vocabulary.p_stateOfRegistry)
    private String stateOfRegistry;

    public Aircraft() {
    }

    public Aircraft(String manufacturer, String type) {
        this.manufacturer = manufacturer;
        this.type = type;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getManufacturer() {
        return manufacturer;
    }

    public void setManufacturer(String manufacturer) {
        this.manufacturer = manufacturer;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getRegistration() {
        return registration;
    }

    public void setRegistration(String registration) {
        this.registration = registration;
    }

    public String getStateOfRegistry() {
        return stateOfRegistry;
    }

    public void setStateOfRegistry(String stateOfRegistry) {
        this.stateOfRegistry = stateOfRegistry;
    }

    @Override
    public String toString() {
        return manufacturer + " " + type + "(" + registration + ")";
    }
}
