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
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassO")
public class OWLClassO {

    @Id
    private URI uri;

    @OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasE", cascade = {CascadeType.MERGE}, fetch = FetchType.EAGER)
    private Set<OWLClassE> owlClassESet;

    public OWLClassO() {
    }

    public OWLClassO(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public Set<OWLClassE> getOwlClassESet() {
        return owlClassESet;
    }

    public void setOwlClassESet(Set<OWLClassE> owlClassESet) {
        this.owlClassESet = owlClassESet;
    }

    @Override
    public String toString() {
        return "OWLClassO{" +
                "uri=" + uri +
                ", owlClassESet=" + owlClassESet +
                '}';
    }
}
