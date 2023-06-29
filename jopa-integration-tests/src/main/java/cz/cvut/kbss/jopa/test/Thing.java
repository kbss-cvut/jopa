/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.vocabulary.OWL;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.util.Map;
import java.util.Set;

@OWLClass(iri = OWL.THING)
public class Thing {

    @Id(generated = true)
    private String uri;

    @OWLAnnotationProperty(iri = RDFS.LABEL)
    private String name;

    @OWLAnnotationProperty(iri = Vocabulary.DC_DESCRIPTION)
    private String description;

    @Types
    private Set<String> types;

    @Properties
    private Map<String, Set<String>> properties;

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
}
