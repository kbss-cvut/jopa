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
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;

import java.net.URI;

@OWLClass(iri = Vocabulary.Operator)
public class Operator {

    @Id
    private URI uri;

    @ParticipationConstraints(nonEmpty = true)  // Name has to be set
    @OWLDataProperty(iri = Vocabulary.p_name)
    private String name;

    @ParticipationConstraints(nonEmpty = true)
    @OWLDataProperty(iri = Vocabulary.p_code)
    private String code;

    public Operator() {
    }

    public Operator(String name, String code) {
        this.name = name;
        this.code = code;
        generateUri();
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
        generateUri();
    }

    private void generateUri() {
        assert code != null && !code.isEmpty();

        this.uri = URI.create(Vocabulary.BASE_URI + code);
    }

    @Override
    public String toString() {
        return name + " (" + code + ") - " + uri;
    }
}
