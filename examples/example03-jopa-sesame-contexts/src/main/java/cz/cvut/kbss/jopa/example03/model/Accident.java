/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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
