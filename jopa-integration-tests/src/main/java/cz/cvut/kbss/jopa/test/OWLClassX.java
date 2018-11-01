package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_X)
public class OWLClassX {

    @Id(generated = true)
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.P_X_LOCAL_DATE_ATTRIBUTE)
    private LocalDate localDate;

    @OWLDataProperty(iri = Vocabulary.P_X_LOCAL_DATETIME_ATTRIBUTE)
    private LocalDateTime localDateTime;

    @OWLDataProperty(iri = Vocabulary.P_X_INSTANT_ATTRIBUTE)
    private Instant instant;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public LocalDate getLocalDate() {
        return localDate;
    }

    public void setLocalDate(LocalDate localDate) {
        this.localDate = localDate;
    }

    public LocalDateTime getLocalDateTime() {
        return localDateTime;
    }

    public void setLocalDateTime(LocalDateTime localDateTime) {
        this.localDateTime = localDateTime;
    }

    public Instant getInstant() {
        return instant;
    }

    public void setInstant(Instant instant) {
        this.instant = instant;
    }

    @Override
    public String toString() {
        return "OWLClassX{" +
                "uri=" + uri +
                ", localDate=" + localDate +
                ", localDateTime=" + localDateTime +
                ", instant=" + instant +
                '}';
    }
}
