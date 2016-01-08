package cz.cvut.kbss.jopa.example03.model;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;

@OWLClass(iri = Vocabulary.Flight)
public class Flight {

    @Id(generated = true)
    private URI uri;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = Vocabulary.p_flightNum)
    private Integer flightNumber;

    @OWLObjectProperty(iri = Vocabulary.p_hasPlane) // Lazy fetch by default
    private Aircraft plane;

    @ParticipationConstraints(nonEmpty = true)
    @OWLObjectProperty(iri = Vocabulary.p_hasOperator, fetch = FetchType.EAGER)
    private Operator operator;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Integer getFlightNumber() {
        return flightNumber;
    }

    public void setFlightNumber(Integer flightNumber) {
        this.flightNumber = flightNumber;
    }

    public Aircraft getPlane() {
        return plane;
    }

    public void setPlane(Aircraft plane) {
        this.plane = plane;
    }

    public Operator getOperator() {
        return operator;
    }

    public void setOperator(Operator operator) {
        this.operator = operator;
    }

    @Override
    public String toString() {
        return "Flight " + flightNumber + " - " + operator;
    }
}
