package cz.cvut.kbss.jopa.example03.model;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

@OWLClass(iri = Vocabulary.Accident)
public class Accident {

    @Id(generated = true)
    private URI uri;

    @OWLObjectProperty(iri = Vocabulary.p_hasFlight)
    private Set<Flight> flightsAffected;

    @OWLDataProperty(iri = Vocabulary.p_cause)
    private String cause;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Set<Flight> getFlightsAffected() {
        return flightsAffected;
    }

    public void setFlightsAffected(Set<Flight> flightsAffected) {
        this.flightsAffected = flightsAffected;
    }

    public void addAffectedFlight(Flight flight) {
        if (flightsAffected == null) {
            this.flightsAffected = new HashSet<>();
        }
        flightsAffected.add(flight);
    }

    public String getCause() {
        return cause;
    }

    public void setCause(String cause) {
        this.cause = cause;
    }

    @Override
    public String toString() {
        return "Accident{" +
                "cause='" + cause + '\'' +
                ", flightsAffected=" + (flightsAffected != null ? flightsAffected : "[]") +
                '}';
    }
}
